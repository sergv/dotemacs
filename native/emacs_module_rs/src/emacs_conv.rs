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
use std::result;

use emacs;
use emacs::{Env, Result, Value, Vector, FromLisp, IntoLisp};

emacs::use_symbols!(nil setcdr);

pub fn path_to_string(path: PathBuf) -> result::Result<String, String> {
    match path.to_str() {
        None => Err(format!("Invalid file name: {:?}", path)),
        Some(x) => Ok(x.to_string()),
    }
}

pub fn to_strings_iter<'a>(
    input: Value<'a>
) -> DecodingListIter<'a, String>
{
    DecodingListIter::new(input)
}

pub fn resize<'a>(v: Vector<'a>) -> Result<Vector<'a>> {
    let n = v.len();
    let res = v.value().env.make_vector(if n == 0 { 1 } else { n * 2 }, nil)?;
    for (i, x) in v.into_iter().enumerate() {
        res.set(i, x)?;
    }
    Ok(res)
}

pub fn take<'a>(count: usize, v: Vector<'a>) -> Result<Vector<'a>> {
    let res = v.value().env.make_vector(count, nil)?;
    for i in 0..count {
        res.set(i, v.get::<Value<'a>>(i)?)?;
    }
    Ok(res)
}

/// State for incrementally collecting potentially failing results into a pair of vectors.
pub struct IncrementalResErrVec<'a> {
    a: Vector<'a>,
    cap_a: usize,
    size_a: usize,

    b: Vector<'a>,
    cap_b: usize,
    size_b: usize,
}

impl<'a> IncrementalResErrVec<'a> {
    fn env(&self) -> &'a Env {
        self.a.value().env
    }

    pub fn new(env: &'a Env) -> Result<IncrementalResErrVec> {
        Ok(IncrementalResErrVec {
            a: env.make_vector(0, nil)?,
            cap_a: 0,
            size_a: 0,

            b: env.make_vector(0, nil)?,
            cap_b: 0,
            size_b: 0,
        })
    }

    pub fn update<A, B>(&mut self, x: result::Result<A, B>) -> Result<()>
        where
        A: IntoLisp<'a>,
        B: IntoLisp<'a>,
    {
        match x {
            Ok(y) => {
                let new_size = self.size_a + 1;
                if new_size > self.cap_a {
                    self.a = resize(self.a)?;
                    self.cap_a = self.a.len();
                }
                self.a.set(self.size_a, y.into_lisp(self.env())?)?;
                self.size_a = new_size;
            }
            Err(y) => {
                let new_size = self.size_b + 1;
                if new_size > self.cap_b {
                    self.b = resize(self.b)?;
                    self.cap_b = self.b.len();
                }
                self.b.set(self.size_b, y.into_lisp(self.env())?)?;
                self.size_b = new_size;
            }
        }
        Ok(())
    }

    pub fn finalize(self) -> Result<(Vector<'a>, Vector<'a>)> {
        let a = take(self.size_a, self.a)?;
        let b = take(self.size_b, self.b)?;
        Ok((a, b))
    }
}

/// State for incrementally collecting potentially failing results into a pair of lists.
pub struct IncrementalResErrList<'a> {
    succs: IncrementalResList<'a>,
    errs: IncrementalResList<'a>,
}

impl<'a> IncrementalResErrList<'a> {
    pub fn new(env: &'a Env) -> Result<IncrementalResErrList> {
        Ok(IncrementalResErrList {
            succs: IncrementalResList::new(env)?,
            errs: IncrementalResList::new(env)?,
        })
    }

    pub fn update<A, B>(&mut self, x: result::Result<A, B>) -> Result<()>
        where
        A: IntoLisp<'a>,
        B: IntoLisp<'a>,
    {
        match x {
            Ok(y) => {
                self.succs.update(y)
            }
            Err(y) => {
                self.errs.update(y)
            }
        }
    }

    pub fn finalize(self) -> Result<(Value<'a>, Value<'a>)> {
        Ok((self.succs.finalize()?, self.errs.finalize()?))
    }
}


/// State for incrementally collecting successful results into a vector.
pub struct IncrementalResVec<'a> {
    items: Vector<'a>,
    cap: usize,
    size: usize,
}

impl<'a> IncrementalResVec<'a> {
    fn env(&self) -> &'a Env {
        self.items.value().env
    }

    pub fn new(env: &'a Env) -> Result<IncrementalResVec> {
        Ok(IncrementalResVec {
            items: env.make_vector(0, nil)?,
            cap: 0,
            size: 0,
        })
    }

    pub fn update<A>(&mut self, x: A) -> Result<()>
        where
        A: IntoLisp<'a>,
    {
        let new_size = self.size + 1;
        if new_size > self.cap {
            self.items = resize(self.items)?;
            self.cap = self.items.len();
        }
        self.items.set(self.size, x.into_lisp(self.env())?)?;
        self.size = new_size;
        Ok(())
    }

    pub fn finalize(self) -> Result<Vector<'a>> {
        let a = take(self.size, self.items)?;
        Ok(a)
    }
}

/// State for incrementally collecting successful results into a list.
pub struct IncrementalResList<'a> {
    store: Value<'a>,
    last_cell: Value<'a>,
}

impl<'a> IncrementalResList<'a> {
    fn env(&self) -> &'a Env {
        self.store.env
    }

    pub fn new(env: &'a Env) -> Result<IncrementalResList> {
        let tmp = env.cons(nil, nil)?;
        Ok(IncrementalResList {
            store: tmp,
            last_cell: tmp,
        })
    }

    pub fn update<A>(&mut self, x: A) -> Result<()>
        where
        A: IntoLisp<'a>,
    {
        let env = self.env();
        self.last_cell =
            env.call(
                setcdr,
                (self.last_cell, env.cons(x.into_lisp(env)?, nil)?))?;
        Ok(())
    }

    pub fn finalize(self) -> Result<Value<'a>> {
        self.store.cdr()
    }
}

pub fn to_list<'a, I, A>(env: &'a Env, iter: I) -> emacs::Result<Value<'a>>
    where
    I: Iterator<Item = A>,
    A: IntoLisp<'a>,
{
    let mut s = IncrementalResList::new(env)?;
    for x in iter {
        s.update(x)?;
    }
    s.finalize()
}

pub struct ListIter<'a> {
    list: Value<'a>,
}

impl<'a> ListIter<'a> {
    pub fn new(list: Value<'a>) -> Self {
        ListIter { list }
    }
}

impl<'a> Iterator for ListIter<'a> {
    type Item = emacs::Result<Value<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.list.is_not_nil() {
            let rest = match self.list.cdr() {
                Err(err) => return Some(Err(err)),
                Ok(x) => x,
            };
            let x = match self.list.car() {
                Err(err) => return Some(Err(err)),
                Ok(x) => x,
            };
            self.list = rest;
            Some(Ok(x))
        } else {
            None
        }
    }
}

pub struct DecodingListIter<'a, A> {
    iter: ListIter<'a>,
    item: std::marker::PhantomData<A>,
}

impl<'a, A> DecodingListIter<'a, A> {
    pub fn new(list: Value<'a>) -> Self {
        DecodingListIter {
            iter: ListIter::new(list),
            item: std::marker::PhantomData,
        }
    }
}

impl<'a, A: FromLisp<'a>> Iterator for DecodingListIter<'a, A> {
    type Item = emacs::Result<A>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            None => None,
            Some(Err(err)) => Some(Err(err)),
            Some(Ok(x)) => Some(A::from_lisp(x)),
        }
    }
}
