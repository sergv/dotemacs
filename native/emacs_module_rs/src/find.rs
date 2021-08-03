// Copyright 2021 Sergey Vinokurov
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use std::mem::MaybeUninit;
use std::path::PathBuf;
use std::result;
use std::sync::Arc;
use std::sync::Barrier;
use std::sync::mpsc;

use crossbeam::queue::ArrayQueue;

use anyhow::{self, Result};
use crossbeam;
use crossbeam::thread::ScopedJoinHandle;
use globset::{Glob, GlobSet, GlobBuilder, GlobSetBuilder};

fn mk_glob(pat: &str) -> result::Result<Glob, globset::Error> {
    let mut b = GlobBuilder::new(pat);
    b.case_insensitive(true);
    b.literal_separator(false);
    b.backslash_escape(false);
    b.build()
}

fn glob_should_test_against_abs(pat: &str) -> bool {
    pat.chars().any(std::path::is_separator)
}

const THREADS: usize = 2;

pub struct GlobEntry {
    pub rel: GlobSet,
    pub abs: GlobSet,
}

pub struct IgnoreAllow {
    pub ignore: GlobEntry,
    pub allow: GlobEntry,
    pub have_rel: bool,
    pub have_abs: bool,
}

impl GlobEntry {
    fn is_match(&self, entry: &std::fs::DirEntry, cache_path: &mut Option<PathBuf>) -> bool {
        if self.rel.is_match(entry.file_name()) {
            true
        } else if !self.abs.is_empty() {
            let p = entry.path();
            let res = self.abs.is_match(&p);
            // Store expensive-to-compute path so that it may be reused.
            *cache_path = Some(p);
            res
        } else {
            false
        }
    }
}

impl IgnoreAllow {
    fn new(ignore: GlobEntry, allow: GlobEntry) -> Self {
        let have_rel = !ignore.rel.is_empty() || !allow.rel.is_empty();
        let have_abs = !ignore.abs.is_empty() || !allow.abs.is_empty();
        IgnoreAllow { ignore, allow, have_rel, have_abs }
    }

    fn is_match(&self, entry: &std::fs::DirEntry, cache_path: &mut Option<PathBuf>) -> bool {
        if self.have_rel {
            let name = entry.file_name();
            let rel_cand = globset::Candidate::new(&name);

            if self.ignore.rel.is_match_candidate(&rel_cand) {
                return false;
            }

            if self.have_abs {
                let path = entry.path();
                let abs_cand = globset::Candidate::new(&path);

                if self.ignore.abs.is_match_candidate(&abs_cand) {
                    *cache_path = Some(path);
                    return false;
                }

                let res =
                    self.allow.rel.is_match_candidate(&rel_cand) ||
                    self.allow.abs.is_match_candidate(&abs_cand);

                // Store expensive-to-compute path so that it may be reused.
                *cache_path = Some(path);

                res
            } else {
                self.allow.rel.is_match_candidate(&rel_cand)
            }
        } else if self.have_abs {
            let path = entry.path();
            let path_cand = globset::Candidate::new(&path);
            let res =
                !self.ignore.abs.is_match_candidate(&path_cand) &&
                self.allow.abs.is_match_candidate(&path_cand);
            *cache_path = Some(path);
            res
        } else {
            false
        }
    }
}

pub struct Ignores {
    pub files: IgnoreAllow,
    pub ignored_dirs: GlobEntry,
}

impl Ignores {
    pub fn new<E, I1, I2, I3, I4, I5, S1, S2, S3, S4, S5>(
        globs: I1,
        ignored_file_globs: I2,
        ignored_dir_globs: I3,
        ignored_dir_prefixes_globs: I4,
        ignored_abs_dirs: I5,
    ) -> result::Result<Self, E>
        where
        E: From<globset::Error>,
        I1: Iterator<Item = result::Result<S1, E>>,
        I2: Iterator<Item = result::Result<S2, E>>,
        I3: Iterator<Item = result::Result<S3, E>>,
        I4: Iterator<Item = result::Result<S4, E>>,
        I5: Iterator<Item = result::Result<S5, E>>,
        S1: AsRef<str>,
        S2: AsRef<str>,
        S3: AsRef<str>,
        S4: AsRef<str>,
        S5: AsRef<str>,
    {
        let mut wanted_file_abs_builder = GlobSetBuilder::new();
        let mut wanted_file_rel_builder = GlobSetBuilder::new();
        let mut ignored_file_abs_builder = GlobSetBuilder::new();
        let mut ignored_file_rel_builder = GlobSetBuilder::new();
        let mut ignored_dir_abs_builder = GlobSetBuilder::new();
        let mut ignored_dir_rel_builder = GlobSetBuilder::new();

        for x in globs {
            let y = x?;
            let z = y.as_ref();
            let g = mk_glob(z)?;
            if glob_should_test_against_abs(z) {
                wanted_file_abs_builder.add(g);
            } else {
                wanted_file_rel_builder.add(g);
            }
        }
        for x in ignored_file_globs {
            let y = x?;
            let z = y.as_ref();
            let g = mk_glob(z)?;
            if glob_should_test_against_abs(z) {
                ignored_file_abs_builder.add(g);
            } else {
                ignored_file_rel_builder.add(g);
            }
        }

        {
            let mut tmp = String::new();
            for x in ignored_abs_dirs {
                let y = x?;
                ignored_dir_abs_builder.add(mk_glob(y.as_ref())?);
                tmp.clear();
            }
            for x in ignored_dir_globs {
                let y = x?;
                let z = y.as_ref();
                tmp.push_str("**/");
                tmp.extend(z.chars());
                let g = mk_glob(&tmp)?;
                if glob_should_test_against_abs(z) {
                    ignored_dir_abs_builder.add(g);
                } else {
                    ignored_dir_rel_builder.add(g);
                }
                tmp.clear();
            }
            for x in ignored_dir_prefixes_globs {
                let y = x?;
                let z = y.as_ref();
                tmp.push_str("**/");
                tmp.extend(z.chars());
                tmp.push('*');
                let g = mk_glob(&tmp)?;
                if glob_should_test_against_abs(z) {
                    ignored_dir_abs_builder.add(g);
                } else {
                    ignored_dir_rel_builder.add(g);
                }
                tmp.clear();
            }
        }

        let wanted_files_rel = wanted_file_rel_builder.build()?;
        let wanted_files_abs = wanted_file_abs_builder.build()?;
        let ignored_files_rel = ignored_file_rel_builder.build()?;
        let ignored_files_abs = ignored_file_abs_builder.build()?;
        let ignored_dirs_rel = ignored_dir_rel_builder.build()?;
        let ignored_dirs_abs = ignored_dir_abs_builder.build()?;

        Ok(Ignores {
            files: IgnoreAllow::new(
                GlobEntry { rel: ignored_files_rel, abs: ignored_files_abs },
                GlobEntry { rel: wanted_files_rel, abs: wanted_files_abs }
            ),
            ignored_dirs: GlobEntry { rel: ignored_dirs_rel, abs: ignored_dirs_abs },
        })
    }
}

pub trait Root: Clone + Send + 'static {
    fn from_path(input: &PathBuf) -> Self;
}

/// Use this instance to disable keeping track of original roots.
impl Root for () {
    fn from_path(_input: &PathBuf) -> Self {
        ()
    }
}

/// Use this instance to keep track of original roots and pass them to the consume callback.
impl Root for Arc<PathBuf> {
    fn from_path(input: &PathBuf) -> Self {
        Arc::new(input.clone())
    }
}

// Define a function callable by Lisp.
pub fn find_rec<'a, Str, Iter, Consume, OrigRoot, Res, HandleFile, InitThread, State>(
    roots: Iter,
    ignores: &Ignores,
    init_thread: InitThread,
    handle_file: HandleFile,
    mut consume: Consume,
) -> Result<()>
    where
    Str: AsRef<str>,
    Iter: Iterator<Item = Result<Str>> + ExactSizeIterator,
    Consume: FnMut(Res) -> Result<()>,
    OrigRoot: Root + std::fmt::Debug,
    Res: Send + 'static,
    HandleFile: FnMut(&mut State, OrigRoot, PathBuf, &mut mpsc::SyncSender<Res>) -> Result<()> + Send + Clone,
    InitThread: FnMut() -> Result<State> + Send + Clone,
{
    let (report_result, receive_result) = mpsc::sync_channel(2 * THREADS);
    let roots_count = roots.len();

    let tasks_queue = ArrayQueue::new((10 * THREADS).max(roots_count));

    for r in roots {
        let path = std::path::PathBuf::from(std::ffi::OsString::from(r?.as_ref()));
        if !ignores.ignored_dirs.abs.is_match(&path) && !ignores.ignored_dirs.rel.is_match(&path) {
            tasks_queue.push((Root::from_path(&path), path.clone())).expect("Task queue should have enough size to hold initial set of roots");
        }
    }

    let tasks = &tasks_queue;

    let barr = Barrier::new(THREADS);
    let barr_ref = &barr;

    crossbeam::scope(
        move |s| -> result::Result<_, _> {

            type Handle<'a> = ScopedJoinHandle<'a, anyhow::Result<()>>;

            let main_id: Handle = {
                let private_report_result = report_result.clone();
                let mut handle = handle_file.clone();
                let mut init = init_thread.clone();
                s.spawn(
                    move |_| {
                        let mut s = init()?;
                        process_main(
                            barr_ref,
                            tasks,
                            private_report_result,
                            ignores,
                            |root, x, chan| handle(&mut s, root, x, chan),
                        )
                    }
                )
            };

            const INIT: MaybeUninit<Handle> = MaybeUninit::uninit();
            let mut handles: [_; THREADS] = [INIT; THREADS];
            handles[0] = MaybeUninit::new(main_id);

            for i in 1..THREADS {
                let private_report_result = report_result.clone();
                let mut handle = handle_file.clone();
                let mut init = init_thread.clone();
                let id: Handle = s.spawn(
                    move |_| -> result::Result<_, _> {
                        let mut s = init()?;
                        process_child(
                            barr_ref,
                            tasks,
                            private_report_result,
                            ignores,
                            |root, x, chan| handle(&mut s, root, x, chan),
                        )
                    }
                );
                handles[i] = MaybeUninit::new(id);
            }

            std::mem::drop(report_result);
            while let Ok(x) = receive_result.recv() {
               consume(x)?
            }

            for h in handles {
                unsafe {
                    h.assume_init().join().unwrap().unwrap();
                }
            }

            Ok(())
        }
    ).unwrap()
}

fn process_main<R, A, F, E>(
    barr: &Barrier,
    tasks: &ArrayQueue<(R, PathBuf)>,
    mut report_result: mpsc::SyncSender<A>,
    ignores: &Ignores,
    mut handle_file: F,
) -> result::Result<(), E>
    where
    A: Send + 'static,
    R: Root,
    F: FnMut(R, PathBuf, &mut mpsc::SyncSender<A>) -> result::Result<(), E>,
    E: From<std::io::Error>,
{
    let mut children_awoken = false;

    let mut local_queue: Vec<(R, PathBuf)> = Vec::new();
    loop {
        let (orig_root, root) = match local_queue.pop() {
            Some(x) =>
                match tasks.push(x) {
                    Ok(()) => continue,
                    Err(y) => y,
                },
            None => match tasks.pop() {
                Some(x) => x,
                None => break,
            },
        };

        visit_dir(
            root,
            ignores,
            |path| match tasks.push((orig_root.clone(), path)) {
                Ok(()) => (),
                Err(task) => local_queue.push(task),
            },
            |x| { handle_file(orig_root.clone(), x, &mut report_result) },
        )?;

        if !children_awoken && tasks.is_full() {
            // Wake up children.
            barr.wait();
            children_awoken = true;
        }
    }
    // Wake up children if not already - we’re wrapping up.
    if !children_awoken {
        barr.wait();
    }
    Ok(())
}

fn process_child<R, A, F, E>(
    barr: &Barrier,
    tasks: &ArrayQueue<(R, PathBuf)>,
    mut report_result: mpsc::SyncSender<A>,
    ignores: &Ignores,
    mut handle_file: F,
) -> result::Result<(), E>
    where
    A: Send + 'static,
    R: Root,
    F: FnMut(R, PathBuf, &mut mpsc::SyncSender<A>) -> result::Result<(), E>,
    E: From<std::io::Error>,
{
    barr.wait();

    let mut local_queue: Vec<(R, PathBuf)> = Vec::new();
    loop {
        let (orig_root, root) = match local_queue.pop() {
            Some(x) =>
                match tasks.push(x) {
                    Ok(()) => continue,
                    Err(y) => y,
                },
            None => match tasks.pop() {
                Some(x) => x,
                None => break,
            },
        };

        visit_dir(
            root,
            ignores,
            |path| match tasks.push((orig_root.clone(), path)) {
                Ok(()) => (),
                Err(task) => local_queue.push(task),
            },
            |x| { handle_file(orig_root.clone(), x, &mut report_result) },
        )?;
    }
    Ok(())
}

pub fn visit_dir<D, F, E>(
    root: PathBuf,
    ignores: &Ignores,
    mut record_dir: D,
    mut record_file: F,
) -> result::Result<(), E>
    where
    D: FnMut(PathBuf) -> (),
    F: FnMut(PathBuf) -> result::Result<(), E>,
    E: From<std::io::Error>,
{
    for entry in std::fs::read_dir(root)? {
        let entry = entry?;
        let typ = entry.file_type()?;
        let mut tmp = None;
        if typ.is_file() {
            if ignores.files.is_match(&entry, &mut tmp) {
                let path = tmp.unwrap_or_else(|| entry.path());
                record_file(path)?;
            }
        } else if typ.is_dir() {
            if !ignores.ignored_dirs.is_match(&entry, &mut tmp) {
                let path = tmp.unwrap_or_else(|| entry.path());
                record_dir(path);
            }
        }
    }
    Ok(())
}
