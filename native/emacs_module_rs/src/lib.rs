
#![allow(dead_code)]

use std::iter::IntoIterator;
use std::path::{PathBuf, Path};
use std::result;
use std::sync::{Arc, mpsc};

use anyhow;
use emacs;
use emacs::{defun, Env, Result, Value, Vector, IntoLisp};
use grep_matcher::Matcher;
use grep_regex::{RegexMatcher, RegexMatcherBuilder};
use grep_searcher::{self, Searcher, SearcherBuilder};
use pathdiff;

pub mod emacs_conv;
pub mod find;
pub mod fuzzy_match;
pub mod path;

use emacs_conv::*;
use path::EmacsPath;

emacs::use_symbols!(nil make_egrep_match length);

// Emacs won't load the module without this.
emacs::plugin_is_GPL_compatible!();

// Register the initialization hook that Emacs will call when it loads the module.
#[emacs::module(name = "rust_native")]
fn init(env: &Env) -> Result<Value<'_>> {
    Ok(nil.bind(env))
}

fn decode_vector_of_chars(v: Vector) -> Result<Vec<char>> {
    v.into_iter()
     .map(|x| {
         x.into_rust().and_then(|y| {
             std::char::from_u32(y)
                 .ok_or_else(|| anyhow::Error::msg(format!("Invalid character: {}", y)))
         })
     })
     .collect()
}

#[defun]
fn score_matches<'a>(
    env: &'a Env,
    input_group_seps: Vector<'a>,
    needle: String,
    haystacks: Value<'a>,
) -> Result<Value<'a>>
{
    let mut reuse = fuzzy_match::ReuseState::new();

    let mut scored = Vec::new();

    let group_seps: Vec<char> = decode_vector_of_chars(input_group_seps)?;

    for haystack in ListIter::new(haystacks) {
        let haystack = haystack?;
        let haystack_str: String = haystack.clone().into_rust()?;

        let m: fuzzy_match::Match<()> = fuzzy_match::fuzzy_match(
            &needle,
            &haystack_str,
            &group_seps,
            &mut reuse
        );

        scored.push((m.score, haystack_str, haystack));
    }

    scored.sort_unstable_by(
        |(x, xstr, _), (y, ystr, _)| {
            // Propagate greatest score to the beginning.
            // Resolve equal scores by comparing lengths.
            x.cmp(y).reverse().then_with(|| xstr.len().cmp(&ystr.len()))
        });

    let mut results = IncrementalResList::new(env)?;
    for (_, _, s) in scored {
        results.update(s)?;
    }
    results.finalize()
}

#[defun]
fn score_single_match<'a>(
    env: &'a Env,
    input_group_seps: Vector<'a>,
    needle: String,
    haystack: String,
) -> Result<Value<'a>>
{
    let mut reuse = fuzzy_match::ReuseState::new();

    let group_seps: Vec<char> = decode_vector_of_chars(input_group_seps)?;

    let m: fuzzy_match::Match<Vec<fuzzy_match::StrIdx>> = fuzzy_match::fuzzy_match(
        &needle,
        &haystack,
        &group_seps,
        &mut reuse,
    );

    env.cons(m.score.into_lisp(env)?, to_list(env, m.positions.into_iter())?)
}

#[defun]
fn find_rec<'a>(
    env: &'a Env,
    input_roots: Value,
    input_globs: Value,
    input_ignored_file_globs: Value,
    input_ignored_dir_globs: Value,
    input_ignored_dir_prefixes_globs: Value,
    input_ignored_abs_dirs: Value,
) -> Result<Value<'a>>
{
    let roots_count: usize = env.call(length, (input_roots,))?.into_rust()?;
    let roots = to_strings_iter(input_roots);

    let globs = to_strings_iter(input_globs);
    let ignored_file_globs = to_strings_iter(input_ignored_file_globs);
    let ignored_dir_globs = to_strings_iter(input_ignored_dir_globs);
    let ignored_dir_prefixes_globs = to_strings_iter(input_ignored_dir_prefixes_globs);
    let ignored_abs_dirs = to_strings_iter(input_ignored_abs_dirs);

    let ignores = find::Ignores::new(globs, ignored_file_globs, ignored_dir_globs, ignored_dir_prefixes_globs, ignored_abs_dirs)?;

    let mut s = IncrementalResErrList::new(env)?;

    find::find_rec(
        roots,
        roots_count,
        &ignores,
        || Ok(()),
        |_state, _orig_root: (), x, chan| {
            chan.send(path_to_string(x)).map_err(anyhow::Error::new)
        },
        |y| s.update(y)
    )?;

    let (files, errs) = s.finalize()?;
    env.cons(files, errs)
}

// Define a function callable by Lisp.
#[defun]
fn find_rec_serial<'a>(
    env: &'a Env,
    input_roots: Value,
    input_globs: Value,
    input_ignored_file_globs: Value,
    input_ignored_dir_globs: Value,
    input_ignored_dir_prefixes_globs: Value,
    input_ignored_abs_dirs: Value,
) -> Result<Value<'a>>
{
    let roots = to_strings_iter(input_roots);
    let globs = to_strings_iter(input_globs);
    let ignored_file_globs = to_strings_iter(input_ignored_file_globs);
    let ignored_dir_globs = to_strings_iter(input_ignored_dir_globs);
    let ignored_dir_prefixes_globs = to_strings_iter(input_ignored_dir_prefixes_globs);
    let ignored_abs_dirs = to_strings_iter(input_ignored_abs_dirs);

    let ignores = &find::Ignores::new(globs, ignored_file_globs, ignored_dir_globs, ignored_dir_prefixes_globs, ignored_abs_dirs)?;

    let mut local_queue: Vec<PathBuf> = Vec::new();
    for r in roots {
        let path = std::path::PathBuf::from(std::ffi::OsString::from(r?));
        if !ignores.ignored_dirs.abs.is_match(&path) && !ignores.ignored_dirs.rel.is_match(&path) {
            local_queue.push(path);
        }
    }

    let mut s = IncrementalResErrList::new(env)?;

    loop {
        let root: PathBuf =
            match local_queue.pop() {
                Some(x) => x,
                None => break,
            };

        find::visit_dir(
            root,
            &ignores,
            |p| local_queue.push(p),
            |x| s.update(path_to_string(x)),
        )?
    }
    let (files, errs) = s.finalize()?;
    env.cons(files, errs)
}

#[defun]
fn grep<'a>(
    env: &'a Env,
    input_roots: Value,
    regexp: String,
    input_globs: Value,
    input_ignored_file_globs: Value,
    input_ignored_dir_globs: Value,
    input_ignored_dir_prefixes_globs: Value,
    input_ignored_abs_dirs: Value,
    input_case_insensitive: Value,
) -> Result<Value<'a>>
{
    let roots_count: usize = env.call(length, (input_roots,))?.into_rust()?;
    let roots = to_strings_iter(input_roots);

    let globs = to_strings_iter(input_globs);
    let ignored_file_globs = to_strings_iter(input_ignored_file_globs);
    let ignored_dir_globs = to_strings_iter(input_ignored_dir_globs);
    let ignored_dir_prefixes_globs = to_strings_iter(input_ignored_dir_prefixes_globs);
    let ignored_abs_dirs = to_strings_iter(input_ignored_abs_dirs);

    let case_insensitive = input_case_insensitive.is_not_nil();

    let ignores = find::Ignores::new(globs, ignored_file_globs, ignored_dir_globs, ignored_dir_prefixes_globs, ignored_abs_dirs)?;

    let mut results = IncrementalResList::new(env)?;

    find::find_rec(
        roots,
        roots_count,
        &ignores,
        || {
            let searcher = SearcherBuilder::new()
                .line_number(true)
                .multi_line(true)
                .memory_map(unsafe { grep_searcher::MmapChoice::auto() })
                .binary_detection(grep_searcher::BinaryDetection::none())
                .build();

            let matcher = RegexMatcherBuilder::new()
                .case_insensitive(case_insensitive)
                .multi_line(true)
                .build(&regexp)?;

            Ok((searcher, matcher))
        },
        |(searcher, matcher), orig_root: Arc<PathBuf>, path, results| {
            let sink = GrepSink {
                rel_path_cache: None,
                abs_path_cache: None,
                abs_path: &path,
                orig_root: &*orig_root,
                matcher: &matcher,
                results: &results,
            };
            searcher.search_path(&*matcher, &path, sink).map_err(|x| x.err)
        },
        |m| {
            results.update(m)
        }
    )?;

    results.finalize()?.into_lisp(env)
}

struct Match {
    line: u32,
    column: u16,
    prefix: String,
    body: String,
    suffix: String,
    abs_path: Arc<EmacsPath>,
    rel_path: Arc<EmacsPath>,
}

impl<'a> emacs::IntoLisp<'a> for Match {
    fn into_lisp(self, env: &'a Env) -> Result<Value<'a>> {
        env.call(
            make_egrep_match,
            (&*self.abs_path,
             &*self.rel_path,
             self.line,
             self.column,
             self.prefix,
             self.body,
             self.suffix
            )
        )
    }
}

struct GrepSink<'a, 'b, 'c> {
    // Cached relative path of currently processedfile.
    rel_path_cache: Option<Arc<EmacsPath>>,
    // Cached absolute path of currently processedfile.
    abs_path_cache: Option<Arc<EmacsPath>>,
    abs_path: &'a Path,
    orig_root: &'a Path,
    matcher: &'b RegexMatcher,
    results: &'c mpsc::SyncSender<Match>,
}

struct Error {
    err: emacs::Error,
}

impl Error {
    fn msg<T>(msg: T) -> Self
        where
        T: std::fmt::Display + std::fmt::Debug + Send + Sync + 'static
    {
        Error {
            err: emacs::Error::msg(msg)
        }
    }
}

impl grep_searcher::SinkError for Error {
    fn error_message<T: std::fmt::Display>(message: T) -> Self {
        Error::msg(message.to_string())
    }
}

impl<'a, 'b, 'c> grep_searcher::Sink for GrepSink<'a, 'b, 'c> {
    type Error = Error;

    fn matched(&mut self, _searcher: &Searcher, m: &grep_searcher::SinkMatch) -> result::Result<bool, Self::Error> {
        let line = m.line_number().expect("Line numbers must be available") as u32;
        let matched_lines = m.bytes();

        let rel_path = match self.rel_path_cache {
            Some(ref x) => x,
            None => {
                let path = match pathdiff::diff_paths(self.abs_path, self.orig_root) {
                    None => return Err(Error::msg(format!(
                        "Invariant violation: abs_path {:?} should be under orig_root {:?}",
                        self.abs_path,
                        self.orig_root
                    ))),
                    Some(x) => x,
                };
                self.rel_path_cache = Some(Arc::new(EmacsPath::new(path).map_err(Error::msg)?));
                self.rel_path_cache.as_ref().unwrap()
            }
        };

        let abs_path = match self.abs_path_cache {
            Some(ref x) => x,
            None => {
                self.abs_path_cache = Some(Arc::new(EmacsPath::new(self.abs_path.to_owned()).map_err(Error::msg)?));
                self.abs_path_cache.as_ref().unwrap()
            }
        };

        let submatch = self.matcher.find(matched_lines).unwrap().expect("Match is guaranteed to be found");

        let prefix = String::from_utf8_lossy(&matched_lines[..submatch.start()]);
        let body = String::from_utf8_lossy(&matched_lines[submatch]);
        let suffix = String::from_utf8_lossy(&matched_lines[submatch.end()..]);
        let suffix = suffix.trim_end_matches(|c| c == '\r' || c == '\n');

        let column = submatch.start() as u16;

        self.results.send(Match {
            line,
            column,
            prefix: prefix.to_string(),
            body: body.to_string(),
            suffix: suffix.to_string(),
            rel_path: rel_path.clone(),
            abs_path: abs_path.clone(),
        }).map_err(|err| Error::msg(format!("Failed to send match: {}", err)))?;

        Ok(true)
    }
}
