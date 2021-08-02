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

use std::path::PathBuf;
use std::result::Result;

use emacs;
use emacs::{Env, Value};

pub struct EmacsPath {
    path: String,
}

impl EmacsPath {
    pub fn new(path: PathBuf) -> Result<Self, String> {
        match path.into_os_string().into_string() {
            Err(err) => Err(format!("Path has invalid utf8 encoding: {:?}", err)),
            Ok(s) => {
                #[cfg(target_family = "windows")]
                let path = unsafe {
                    for b in s.as_bytes_mut() {
                        match b {
                            b'\\' => *b = b'/',
                            _ => (),
                        }
                    }
                    s
                };
                #[cfg(target_family = "unix")]
                let path = s;
                Ok(EmacsPath { path })
            }
        }
    }
}

impl<'a> emacs::IntoLisp<'a> for &EmacsPath {
    fn into_lisp(self, env: &'a Env) -> emacs::Result<Value<'a>> {
        self.path.clone().into_lisp(env)
    }
}

