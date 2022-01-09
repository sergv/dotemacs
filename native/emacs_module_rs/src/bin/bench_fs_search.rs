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

use globset;

use emacs_native_rs::find;


fn main() {
    let roots = vec!["/home/sergey/projects/rust"];
    let roots_count = roots.len();

    let globs = vec!["*.toml"];
    let ignored_file_globs = vec!["*.so", "/home/sergey/projects/rust/projects/ripgrep/Cross.toml"];
    let ignored_dir_globs = vec![".git"];
    let ignored_dir_prefixes_globs = vec![".car"];
    let ignored_abs_dirs: Vec<&str> = vec![];

    let ignores = {
        let tmp: Result<find::Ignores, globset::Error> = find::Ignores::new(
            globs.into_iter().map(Ok),
            ignored_file_globs.into_iter().map(Ok),
            ignored_dir_globs.into_iter().map(Ok),
            ignored_dir_prefixes_globs.into_iter().map(Ok),
            ignored_abs_dirs.into_iter().map(Ok),
        );
        tmp.unwrap()
    };

    let res: anyhow::Result<()> = find::find_rec(
        roots.into_iter().map(Ok),
        roots_count,
        &ignores,
        || Ok(()),
        |_, _orig_root: (), path, chan| chan.send(path).map_err(anyhow::Error::new),
        |x| {
            println!("{:?}", x);
            Ok(())
        }
    );

    res.unwrap();
}
