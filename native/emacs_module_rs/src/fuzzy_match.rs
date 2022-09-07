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

use fnv::FnvHashMap;

type Heat = i16;

pub type StrIdx = i16;

const LAST_CHAR_BONUS: Heat = 1;
const INIT_SCORE: Heat = -35;
const LEADING_PENALTY: Heat = -45;
const WORD_START: Heat = 85;

#[inline]
#[cfg(debug)]
fn read_arr<A>(xs: &[A], idx: i16) -> &A {
    xs.get(idx as usize)
}

#[inline]
#[cfg(not(debug))]
fn read_arr<A>(xs: &[A], idx: i16) -> &A {
    unsafe {
        xs.get_unchecked(idx as usize)
    }
}

#[inline]
#[cfg(debug)]
fn read_arr_mut<A>(xs: &mut [A], idx: i16) -> &mut A {
    xs.get_mut(idx as usize)
}

#[inline]
#[cfg(not(debug))]
fn read_arr_mut<A>(xs: &mut [A], idx: i16) -> &mut A {
    unsafe {
        xs.get_unchecked_mut(idx as usize)
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct Match<PS> {
    pub score: Heat,
    pub positions: PS,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Submatch {
    score: Heat,
    position: StrIdx,
    contiguous_count: i16,
    prev: i16,
}

pub trait Positions {
    fn infer_positions(idx: SubmatchIdx, submatches: &Vec<Submatch>) -> Self;
    fn empty() -> Self;
    fn singleton(idx: StrIdx) -> Self;
}

impl Positions for Vec<StrIdx> {
    fn infer_positions(mut idx: SubmatchIdx, submatches: &Vec<Submatch>) -> Self {
        let mut positions = Vec::new();
        while idx >= 0 {
            let m = read_arr(submatches, idx);
            positions.push(m.position);
            idx = m.prev;
        }
        positions
    }

    fn empty() -> Self {
        Vec::new()
    }

    fn singleton(idx: SubmatchIdx) -> Self {
        vec![idx]
    }
}

impl Positions for () {
    fn infer_positions(_idx: SubmatchIdx, _submatches: &Vec<Submatch>) -> Self { () }
    fn empty() -> Self { () }
    fn singleton(_idx: StrIdx) -> Self { () }
}

mod occurs {
    use super::*;

    type Positions<'a> = &'a [StrIdx];

    pub struct PositionsStore {
        ptr: usize,
        len: usize,
        cap: usize,
    }

    struct PositionsVec<'a, 'b> {
        store: &'a mut PositionsStore,
        vec: Vec<Positions<'b>>,
    }

    impl<'a, 'b> PositionsVec<'a, 'b> {
        fn new(store: &'a mut PositionsStore) -> Self {
            let vec = unsafe {
                Vec::from_raw_parts(store.ptr as *mut Positions<'b>, store.len, store.cap)
            };
            PositionsVec {
                store,
                vec
            }
        }
    }

    impl<'a, 'b> Drop for PositionsVec<'a, 'b> {
        fn drop(&mut self) {
            *self.store = PositionsStore::from_vec(
                std::mem::replace(&mut self.vec, Vec::new())
            );
        }
    }

    impl PositionsStore {
        fn new<'a>() -> Self {
            PositionsStore::from_vec(Vec::new())
        }

        fn from_vec<'a>(mut v: Vec<Positions<'a>>) -> Self {
            let s = PositionsStore {
                ptr: v.as_mut_ptr() as usize,
                len: v.len(),
                cap: v.capacity(),
            };
            std::mem::forget(v);
            s
        }

        fn with_vec<'a, 'b, F, A>(
            &'a mut self,
            f: F
        ) -> A
            where
            F: for<'d> FnOnce(&'d mut Vec<Positions<'b>>) -> A
        {
            let mut r = PositionsVec::new(self);
            f(&mut r.vec)
        }

        // Take care since vec can be mutated while original PositionsState is still being retained.
        unsafe fn as_vec<'a>(&mut self) -> Vec<Positions<'a>>
        {
            // Not much need to zero everything out if we require user to check that by hand!!
            // let res = Vec::from_raw_parts(self.ptr as *mut CharPositions<'a>, self.len, self.cap);
            // self.ptr = std::ptr::null::<&'a [StrIdx]>() as usize;
            // self.len = 0;
            // self.cap = 0;
            // res
            Vec::from_raw_parts(self.ptr as *mut Positions<'a>, self.len, self.cap)
        }
    }

    pub struct ReuseState {
        needle_occurs: FnvHashMap<char, Vec<StrIdx>>,
        pos: PositionsStore,
    }

    impl Drop for ReuseState {
        fn drop(&mut self) {
            // If we reached this point then just clean up the vector - nothing will happen
            // to the original PositionsState any more.
            unsafe {
                std::mem::drop(self.pos.as_vec())
            }
        }
    }

    impl ReuseState {
        pub fn new() -> ReuseState {
            ReuseState {
                needle_occurs: FnvHashMap::default(),
                pos: PositionsStore::new(),
            }
        }
    }

    pub fn with_occurrences<'a, 'b, 'c, F, A>(
        reuse: &'a mut ReuseState,
        needle: &'b str,
        haystack: &'c str,
        f: F,
    ) -> A
        where
        F: for<'d> FnOnce(usize, Option<&'d Vec<&'a [StrIdx]>>) -> A
    {
        let size = needle.chars().count();

        let needle_occurs = &mut reuse.needle_occurs;
        let pos = &mut reuse.pos;

        // Reuse vectors inside hash map instead of creating new ones.
        for v in needle_occurs.values_mut() {
            v.clear();
        }
        for c in needle.chars() {
            needle_occurs.entry(c).or_insert_with(|| Vec::new());
        }

        for (pos, c) in haystack.chars().enumerate() {
            match needle_occurs.get_mut(&c) {
                None => (),
                Some(ps) => ps.push(pos as StrIdx),
            }
            if is_capital(c) {
                let mut lower = c.to_lowercase();
                if lower.len() == 1 {
                    match needle_occurs.get_mut(&lower.next().unwrap()) {
                        None => (),
                        Some(ps) => ps.push(pos as StrIdx),
                    }
                }
            }
        }

        // Lifetime of created vector is restricted so everything’s ok.
        pos.with_vec(move |positions: &mut Vec<&'a [StrIdx]>| {
            positions.clear();
            for c in needle.chars() {
                // Each character was initialized in the prepare function so unwrap is safe.
                let ps = needle_occurs.get(&c).unwrap() as &'a [StrIdx];
                if ps.is_empty() {
                    return f(size, None);
                }
                positions.push(ps)
            }
            f(size, Some(positions))
        })
    }
}

fn no_match<PS>() -> Match<PS>
    where
    PS: Positions,
{
    Match { score: 0, positions: Positions::empty() }
}

/// Return subslice that is strictly bigger that the argument index. Assumes input slice is sorted.
fn bigger<'a, T: Ord>(x: T, xs: &'a [T]) -> &'a [T] {
    match xs.binary_search(&x) {
        Ok(i) => &xs[i + 1..],
        Err(j) => &xs[j..],
    }
}

pub struct ReuseState {
    occurs: occurs::ReuseState,
    cache: FnvHashMap<(StrIdx, StrIdx), Option<SubmatchIdx>>,
    submatches: Vec<Submatch>,
    heatmap: Vec<Heat>,
}

impl ReuseState {
    pub fn new() -> Self {
        ReuseState {
            occurs: occurs::ReuseState::new(),
            cache: FnvHashMap::default(),
            submatches: Vec::new(),
            heatmap: Vec::new(),
        }
    }
}

pub fn fuzzy_match<'a, 'b, 'c, 'd, PS>(
    needle: &'a str,
    haystack: &'b str,
    group_seps: &'c [char],
    reuse_state: &'d mut ReuseState,
) -> Match<PS>
    where
    PS: Positions,
{
    heatmap(haystack, group_seps, &mut reuse_state.heatmap);

    fuzzy_match_impl(
        &mut reuse_state.occurs,
        needle,
        haystack,
        &mut reuse_state.cache,
        &mut reuse_state.submatches,
        &reuse_state.heatmap,
    )
}

fn fuzzy_match_impl<'a, 'b, 'c, 'd, 'e, 'f, PS>(
    occurs_reuse: &'a mut occurs::ReuseState,
    needle: &'b str,
    haystack: &'c str,
    cache: &'d mut FnvHashMap<(StrIdx, StrIdx), Option<SubmatchIdx>>,
    submatches: &'e mut Vec<Submatch>,
    heatmap: &'f [Heat],
) -> Match<PS>
where
    PS: Positions,
{
    if needle.is_empty() {
        return no_match();
    }

    occurs::with_occurrences(occurs_reuse, needle, haystack, |needle_size, positions| {
        let positions: &Vec<&[StrIdx]> = match positions {
            None => return no_match(),
            Some(x) => x,
        };

        cache.clear();
        submatches.clear();

        if needle_size == 1 {
            let submatch = match_singleton_needle(&heatmap, &positions[0]);
            match submatch {
                None => return no_match(),
                Some(s) => Match {
                    score: s.score,
                    positions: Positions::singleton(s.position),
                },
            }
        } else {
            let submatch = top_down_match(
                cache,
                submatches,
                &heatmap,
                &positions,
                0,
                -1,
                needle_size as StrIdx - 1,
            );

            match submatch {
                None => return no_match(),
                Some(sub_idx) => {
                    let score = read_arr(submatches, sub_idx).score;
                    Match {
                        score,
                        positions: Positions::infer_positions(sub_idx, submatches),
                    }
                }
            }
        }
    })
}


type SubmatchIdx = i16;

fn is_score_better(new: i16, old: i16) -> bool {
    // If scores are equal then perefer later submatces (i.e. the ones
    // that occured later in the needle) to the earlier ones.
    new >= old
}

// Terminal submatch, doesn’t get filled.
const TERMINAL_SUBMATCH: SubmatchIdx = -1;

fn match_singleton_needle<'a, 'b>(
    heatmap: &'a [Heat],
    occurrences: &'b [StrIdx],
) -> Option<Submatch> {
    let mut remaining_occurs = occurrences.iter();
    if let Some(idx) = remaining_occurs.next() {
        let score = *read_arr(heatmap, *idx);

        let mut max_submatch = Submatch {
            score,
            position: *idx,
            contiguous_count: 0,
            prev: TERMINAL_SUBMATCH
        };

        for idx in remaining_occurs {
            let score = *read_arr(heatmap, *idx);
            if score >= max_submatch.score {
                max_submatch.score = score;
                max_submatch.position = *idx;
            }
        }

        Some(max_submatch)
    } else {
        None
    }
}

fn top_down_match<'a, 'b, 'c, 'd, 'e>(
    cache: &'a mut FnvHashMap<(StrIdx, StrIdx), Option<SubmatchIdx>>,
    submatches: &'b mut Vec<Submatch>,
    heatmap: &'c [Heat],
    positions: &'d Vec<&'e [StrIdx]>,
    needle_idx: StrIdx,
    haystack_idx: StrIdx,
    end_idx: StrIdx,
) -> Option<SubmatchIdx>
{
    let key = (needle_idx, haystack_idx);
    match cache.get(&key) {
        Some(res) => return *res,
        None => (),
    }

    let remaining_occurs = bigger(haystack_idx, *read_arr(positions, needle_idx));

    let mut max_submatch = None;
    let mut max_score = 0;

    for idx in remaining_occurs {
        let submatch = top_down_submatch_at(
            *idx,
            cache,
            submatches,
            heatmap,
            positions,
            needle_idx + 1,
            *idx,
            end_idx,
        );

        match (max_submatch, submatch) {
            (_, None) => (),
            (None, Some(sub_idx)) => {
                max_submatch = submatch;
                max_score = read_arr(submatches, sub_idx).score;
            }
            (Some(_), Some(sub_idx)) => {
                let new_score = read_arr(submatches, sub_idx).score;
                if is_score_better(new_score, max_score) {
                    max_score = new_score;
                    max_submatch = submatch;
                }
            }
        }
    }

    cache.insert(key, max_submatch.clone());
    max_submatch
}

fn add_submatch(submatches: &mut Vec<Submatch>, x: Submatch) -> SubmatchIdx {
    let idx = submatches.len();
    submatches.push(x);
    idx as SubmatchIdx
}

fn contiguous_bonus(is_contiguous: bool, contiguous_count: i16) -> Heat {
    if is_contiguous {
        60 + 15 * contiguous_count.min(3)
    } else {
        0
    }
}

fn top_down_submatch_at<'a, 'b, 'c, 'd, 'e>(
    idx1: StrIdx,
    cache: &'a mut FnvHashMap<(StrIdx, StrIdx), Option<SubmatchIdx>>,
    submatches: &'b mut Vec<Submatch>,
    heatmap: &'c [Heat],
    positions: &'d Vec<&'e [StrIdx]>,
    needle_idx: StrIdx,
    haystack_idx: StrIdx,
    end_idx: StrIdx,
) -> Option<SubmatchIdx>
{
    let key = (needle_idx, haystack_idx);
    match cache.get(&key) {
        Some(res) => return *res,
        None => (),
    }

    let res;

    if needle_idx == end_idx {
        let mut remaining_occurs =
            bigger(haystack_idx, *read_arr(positions, needle_idx))
            .iter();
        // Fuse single-character submatches with submatch for penultimate character in order to
        // not allocate list of submatches for single characters.
        if let Some(idx2) = remaining_occurs.next() {
            let is_contiguous2 = idx1 + 1 == *idx2;
            let score1 = *read_arr(heatmap, idx1);
            let score2 = *read_arr(heatmap, *idx2);
            let mut max_submatch = Submatch {
                score: score1 + score2 + contiguous_bonus(is_contiguous2, 0),
                position: idx1,
                contiguous_count: if is_contiguous2 { 1 } else { 0 },
                prev: -1, // Gets filled out at the end!
            };
            let mut terminal_submatch_for_max = Submatch {
                score: score2,
                position: *idx2,
                contiguous_count: 0,
                prev: TERMINAL_SUBMATCH, // Does not get filled out - terminal submatch.
            };

            for idx3 in remaining_occurs {
                let is_contiguous3 = idx1 + 1 == *idx3;
                let score3 = *read_arr(heatmap, *idx3);
                let new_score = score1 + score3 + contiguous_bonus(is_contiguous3, 0);
                if is_score_better(new_score, max_submatch.score) {
                    max_submatch.score = new_score;
                    max_submatch.contiguous_count = if is_contiguous3 { 1 } else { 0 };

                    terminal_submatch_for_max.score = score3;
                    terminal_submatch_for_max.position = *idx3;
                }
            }

            max_submatch.prev = add_submatch(submatches, terminal_submatch_for_max);
            res = Some(add_submatch(submatches, max_submatch));
        } else {
            res = None;
        }
    } else {
        match top_down_match(cache, submatches, heatmap, positions, needle_idx, haystack_idx, end_idx) {
            None => res = None,
            Some(idx) => {
                let score1 = read_arr(heatmap, idx1);
                let submatch = read_arr(submatches, idx);
                let is_contiguous = idx1 + 1 == submatch.position;
                let m = Submatch {
                    score: score1 + submatch.score + contiguous_bonus(is_contiguous, submatch.contiguous_count),
                    position: idx1,
                    contiguous_count: if is_contiguous { submatch.contiguous_count + 1 } else { 0 },
                    prev: idx,
                };
                res = Some(add_submatch(submatches, m));
            }
        }
    }
    cache.insert(key, res.clone());
    res
}

pub fn heatmap<'a>(
    s: &str,
    group_seps: &[char], // sorted
    mut heatmap: &'a mut Vec<i16>,
) -> &'a mut Vec<Heat> {
    heatmap.clear();
    if s.is_empty() {
        return heatmap;
    }

    let mut group_idx = 0;

    let mut is_base_path = false;
    let mut group_start: i16 = 0;
    let mut group_end: i16 = -1; // to account for fake separator
    let mut group_score = 0;
    let mut group_non_base_score = 0;

    let split = split_with_seps(' ', s, group_seps);

    let groups_count = match &split {
        Ok((_, _)) => 1,
        Err(groups) => groups.len() as i16,
    };

    let init_adjustment = if groups_count > 1 { -2 * groups_count } else { 0 };

    heatmap.resize(s.chars().count(), INIT_SCORE + init_adjustment);
    *heatmap.last_mut().unwrap() += LAST_CHAR_BONUS;

    match split {
        Ok((prev, text)) => {
            analyze_group(
                prev,
                text,
                &mut heatmap,
                &mut is_base_path,
                &mut group_idx,
                groups_count,
                &mut group_start,
                &mut group_end,
                &mut group_score,
                &mut group_non_base_score,
            )
        },
        Err(groups) => {
            let mut prev_is_base_path = false;
            let mut prev_group_start = 0;
            let mut prev_group_end = 0;
            let mut prev_group_score = 0;
            let mut prev_group_non_base_score = 0;
            for (prev, text) in groups {
                analyze_group(
                    prev,
                    text,
                    &mut heatmap,
                    &mut is_base_path,
                    &mut group_idx,
                    groups_count,
                    &mut group_start,
                    &mut group_end,
                    &mut group_score,
                    &mut group_non_base_score,
                );

                if prev_is_base_path && is_base_path {
                    let delta = prev_group_non_base_score - prev_group_score;
                    apply_group_score(delta, heatmap, prev_group_start, prev_group_end);
                    prev_is_base_path = false;
                }

                if is_base_path {
                    prev_is_base_path = is_base_path;
                    prev_group_start = group_start;
                    prev_group_end = group_end;
                    prev_group_score = group_score;
                    prev_group_non_base_score = group_non_base_score;
                }
            }
        }
    };

    heatmap
}

fn apply_group_score(score: Heat, heatmap: &mut [Heat], start: i16, end: i16) {
    for i in start..end {
        *read_arr_mut(heatmap, i) += score;
    }
    if (end as usize) < heatmap.len()  {
        *read_arr_mut(heatmap, end) += score;
    }
}

fn analyze_group(
    mut prev: char,
    text: &str,
    heatmap: &mut [Heat],
    is_base_path: &mut bool,
    group_idx: &mut i16,
    groups_count: i16,
    group_start: &mut i16,
    group_end: &mut i16,
    group_score: &mut Heat,
    group_non_base_score: &mut Heat,
) {
    let mut word_char_idx = 0;
    let mut word_idx = -1;
    let mut word_count = 0;

    *group_start = *group_end + 1;

    let mut chars_count = 0;

    for (i, c) in text.chars().enumerate() {
        let j = *group_start + i as i16;
        let is_word = !is_word(prev) && is_word(c);
        if is_word {
            word_count += 1;
        }

        let is_boundary = is_word || !prev.is_uppercase() && c.is_uppercase();
        if is_boundary {
            word_idx += 1;
            word_char_idx = 0;
            *read_arr_mut(heatmap, j) += WORD_START;
        }

        if word_idx >= 0 {
            *read_arr_mut(heatmap, j) += (-3) * word_idx - word_char_idx;
        }

        word_char_idx += 1;
        if penalizes_if_leading(c) {
            let k = j + 1;
            if (k as usize) < heatmap.len() {
                *read_arr_mut(heatmap, k) += LEADING_PENALTY;
            }
        }
        prev = c;
        chars_count += 1;
    }

    *group_end = *group_start + chars_count;

    // Update score for trailing separator of a group.
    let k = *group_end;
    if (k as usize) < heatmap.len() && word_idx >= 0 {
        *read_arr_mut(heatmap, k) += (-3) * word_idx - word_char_idx;
    }

    let base_path = word_count != 0;
    *is_base_path = base_path;
    *group_score = calc_group_score(base_path, groups_count, word_count, *group_idx);
    if base_path {
        *group_non_base_score =
            calc_group_score(false, groups_count, word_count, *group_idx);
    }

    *group_idx += 1;

    apply_group_score(*group_score, heatmap, *group_start, *group_end);
}

#[derive(Debug)]
struct SplitWithSeps<'a, 'b> {
    prev: char,
    s: &'a str,
    group_seps: &'b [char],
}

impl<'a, 'b> SplitWithSeps<'a, 'b> {
    fn is_sep(&self, c: char) -> bool {
        is_member(c, self.group_seps)
    }
}

impl<'a, 'b> Iterator for SplitWithSeps<'a, 'b> {
    type Item = (char, &'a str);

    fn next(&mut self) -> Option<Self::Item> {
        match self.s.split_once(|c| self.is_sep(c)) {
            None => {
                if self.s.is_empty() {
                    None
                } else {
                    let tmp = self.s;
                    self.s = "";
                    Some((self.prev, tmp))
                }
            },
            Some((prefix, suffix)) => {
                let sep = self.s[prefix.len()..].chars().next().unwrap();
                let tmp = self.prev;
                self.prev = sep;
                self.s = suffix;
                Some((tmp, prefix))
            }
        }
    }
}

impl<'a, 'b> ExactSizeIterator for SplitWithSeps<'a, 'b> {
    fn len(&self) -> usize {
        self.s.chars().filter(|c| self.is_sep(*c)).count() + 1
    }
}

fn split_with_seps<'a, 'b>(
    first_sep: char,
    s: &'a str,
    group_seps: &'b [char],
) -> Result<(char, &'a str), SplitWithSeps<'a, 'b>>
{
    if group_seps.is_empty() {
        Ok((first_sep, s))
    } else {
        // Assert that group_seps is sorted.
        debug_assert!(
            group_seps
                .iter()
                .fold((group_seps[0], true), |(prev, is_sorted), &c| (c, is_sorted && prev <= c))
                .1
        );
        Err(SplitWithSeps {
            prev: first_sep,
            s,
            group_seps
        })
    }
}

fn calc_group_score(is_base_path: bool, groups_count: i16, word_count: i16, n: i16) -> Heat {
    if is_base_path {
        35 + (groups_count - 2).max(0) - word_count
    } else {
        let delta = if n == 0 {
            -3
        } else {
            -6 + n
        };
        delta
    }
}

fn is_member(c: char, xs: &[char]) -> bool {
    xs.binary_search(&c).is_ok()
}

fn is_word_separator(c: char) -> bool {
    match c {
        ' ' | '*' | '+' | '-' | '_' | ':' | ';' | '.' | '/' | '\\' => true,
        _ => false,
    }
}

fn is_word(c: char) -> bool {
    !is_word_separator(c)
}

fn is_capital(c: char) -> bool {
    !is_word_separator(c) && c.is_uppercase()
}

fn penalizes_if_leading(c: char) -> bool {
    c == '.'
}

#[cfg(test)]
mod test {
    use super::*;

    const FOOBAR_HEATMAP: &[Heat] = &[84, -2, -3, -4, -5, -5];

    pub fn fuzzy_match_test(
        heatmap: &[Heat],
        needle: &str,
        haystack: &str,
    ) -> Match<Vec<StrIdx>>
    {
        let s = &mut ReuseState::new();
        fuzzy_match_impl(
            &mut s.occurs,
            needle,
            haystack,
            &mut s.cache,
            &mut s.submatches,
            heatmap
        )
    }

    #[test]
    fn heat_map1() {
        assert_eq!(heatmap("foo", &[], &mut Vec::new()), &mut vec![84, -2, -2]);
    }

    #[test]
    fn heat_map2() {
        assert_eq!(heatmap("bar", &[], &mut Vec::new()), &mut vec![84, -2, -2]);
    }

    #[test]
    fn heat_map3() {
        assert_eq!(heatmap("foo.bar", &[], &mut Vec::new()), &mut vec![83, -3, -4, -5, 35, -6, -6]);
    }

    #[test]
    fn heat_map4() {
        assert_eq!(
            heatmap("foo/bar/baz", &[], &mut Vec::new()),
            &mut vec![82, -4, -5, -6, 79, -7, -8, -9, 76, -10, -10]
        );
    }

    #[test]
    fn heat_map5() {
        assert_eq!(
            heatmap("foo/bar/baz", &['/'], &mut Vec::new()),
            &mut vec![41, -45, -46, -47, 39, -47, -48, -49, 79, -7, -7]
        );
    }

    #[test]
    fn heat_map6() {
        assert_eq!(
            heatmap("foo/bar+quux/fizz.buzz/frobnicate/frobulate", &[], &mut Vec::new()),
            &mut vec![78, -8, -9, -10, 75, -11, -12, -13, 72, -14, -15, -16, -17, 69, -17, -18, -19, -20, 21, -20, -21, -22, -23, 63, -23, -24, -25, -26, -27, -28, -29, -30, -31, -32, 60, -26, -27, -28, -29, -30, -31, -32, -32]
        );
    }

    #[test]
    fn heat_map7() {
        assert_eq!(
            heatmap("foo/bar+quux/fizz.buzz/frobnicate/frobulate", &['/'], &mut Vec::new()),
            &mut vec![37, -49, -50, -51, 35, -51, -52, -53, 32, -54, -55, -56, -57, 36, -50, -51, -52, -53, -12, -53, -54, -55, -56, 37, -49, -50, -51, -52, -53, -54, -55, -56, -57, -58, 77, -9, -10, -11, -12, -13, -14, -15, -15]
        );
    }

    #[test]
    fn heat_map7a() {
        assert_eq!(
            heatmap("foo/bar+quux/fizz.buzz", &['/'], &mut Vec::new()),
            &mut vec![41, -45, -46, -47, 39, -47, -48, -49, 36, -50, -51, -52, -53, 78, -8, -9, -10, -11, 30, -11, -12, -12]
        );
    }

    #[test]
    fn heat_map8() {
        assert_eq!(
            heatmap("foo/bar+quux/fizz.buzz//frobnicate/frobulate", &['/'], &mut Vec::new()),
            &mut vec![35, -51, -52, -53, 33, -53, -54, -55, 30, -56, -57, -58, -59, 34, -52, -53, -54, -55, -14, -55, -56, -57, -58, -50, 36, -50, -51, -52, -53, -54, -55, -56, -57, -58, -59, 76, -10, -11, -12, -13, -14, -15, -16, -16]
        );
    }

    #[test]
    fn heat_map9() {
        assert_eq!(
            heatmap("foo/bar+quux/fizz.buzz//frobnicate/frobulate", &['/', 'u'], &mut Vec::new()),
            &mut vec![27, -59, -60, -61, 25, -61, -62, -63, 22, -64, -59, -58, -58, 28, -58, -59, -60, -61, -20, -61, -56, -56, -56, -55, 31, -55, -56, -57, -58, -59, -60, -61, -62, -63, -64, 72, -14, -15, -16, -17, -52, -52, -52, -51]
        );
    }

    #[test]
    fn heat_map10() {
        assert_eq!(
            heatmap("foo/barQuux/fizzBuzz//frobnicate/frobulate", &[], &mut Vec::new()),
            &mut vec![80, -6, -7, -8, 77, -9, -10, 74, -12, -13, -14, -15, 71, -15, -16, -17, 68, -18, -19, -20, -21, -22, 65, -21, -22, -23, -24, -25, -26, -27, -28, -29, -30, 62, -24, -25, -26, -27, -28, -29, -30, -30]
        );
    }

    #[test]
    fn heat_map11() {
        assert_eq!(
            heatmap("foo//bar", &[], &mut Vec::new()),
            &mut vec![83, -3, -4, -5, -6, 80, -6, -6]
        );
    }

    #[test]
    fn heat_map12() {
        assert_eq!(
            heatmap("foo//bar", &['/'], &mut Vec::new()),
            &mut vec![41, -45, -46, -47, -46, 79, -7, -7]
        );
    }

    #[test]
    fn heat_map13() {
        assert_eq!(
            heatmap("contrib/apr/atomic/unix/s390.c", &[], &mut Vec::new()),
            // // Result from elisp that considers numbers uppercase an thus finds two words in "s390"
            // &mut vec![79, -7, -8, -9, -10, -11, -12, -13, 76, -10, -11, -12, 73, -13, -14, -15, -16, -17, -18, 70, -16, -17, -18, -19, 67, 64, -22, -23, -24, 17]
            &mut vec![79, -7, -8, -9, -10, -11, -12, -13, 76, -10, -11, -12, 73, -13, -14, -15, -16, -17, -18, 70, -16, -17, -18, -19, 67, -19, -20, -21, -22, 20]
        );
    }

    #[test]
    fn bigger1() {
        let v: &[i32] = &[1, 2, 3, 4, 5];
        assert_eq!(bigger(2, v), &[3, 4, 5]);
    }

    #[test]
    fn bigger2() {
        let v: &[i32] = &[1, 2, 3, 4, 5];
        assert_eq!(bigger(0, v), v);
    }

    #[test]
    fn bigger3() {
        let v: &[i32] = &[1, 2, 3, 4, 5];
        assert_eq!(bigger(4, v), &[5]);
    }

    #[test]
    fn bigger4() {
        let v: &[i32] = &[1, 2, 3, 4, 5];
        let res: &[i32] = &[];
        assert_eq!(bigger(5, v), res);
    }

    #[test]
    fn bigger5() {
        let v: &[i32] = &[1, 2, 4, 5];
        assert_eq!(bigger(3, v), &[4, 5]);
    }

    #[test]
    fn fuzzy_match1() {
        let m = fuzzy_match_test(
            FOOBAR_HEATMAP,
            "foo",
            "foobar"
        );
        assert_eq!(m, Match { score: 214, positions: vec![0, 1, 2] });
    }

    #[test]
    fn fuzzy_match2() {
        let m = fuzzy_match_test(
            FOOBAR_HEATMAP,
            "fo",
            "foobar"
        );
        assert_eq!(m, Match { score: 142, positions: vec![0, 1] });
    }

    #[test]
    fn fuzzy_match3() {
        let m = fuzzy_match_test(
            FOOBAR_HEATMAP,
            "oob",
            "foobar"
        );
        assert_eq!(m, Match { score: 126, positions: vec![1, 2, 3] });
    }

    #[test]
    fn fuzzy_match4() {
        let m = fuzzy_match_test(
            FOOBAR_HEATMAP,
            "ooba",
            "foobar"
        );
        assert_eq!(m, Match { score: 211, positions: vec![1, 2, 3, 4] });
    }

    #[test]
    fn fuzzy_match5() {
        let m = fuzzy_match_test(
            FOOBAR_HEATMAP,
            "or",
            "foobar"
        );
        assert_eq!(m, Match { score: - 7, positions: vec![1, 5] });
    }

    #[test]
    fn fuzzy_match6() {
        let m = fuzzy_match_test(
            FOOBAR_HEATMAP,
            "x",
            "foobar"
        );
        assert_eq!(m, no_match());
    }

    #[test]
    fn fuzzy_match7() {
        let m = fuzzy_match_test(
            FOOBAR_HEATMAP,
            "fooxbar",
            "foobar"
        );
        assert_eq!(m, no_match());
    }

    #[test]
    fn fuzzy_match8() {
        let m = fuzzy_match_test(
            &(0..100).map(|_| 1).collect::<Vec<Heat>>(),
            "aaaaaaaaaa",
            &(0..100).map(|_| 'a').collect::<String>(),
        );
        assert_eq!(m, Match { score: 865, positions: (90..100).collect() });
    }

    #[test]
    fn fuzzy_match9() {
        let m = fuzzy_match_test(
            &(0..200).map(|_| 1).collect::<Vec<Heat>>(),
            "aaaaaaaaaa",
            &(0..200).map(|_| 'a').collect::<String>(),
        );
        assert_eq!(m, Match { score: 865, positions: (190..200).collect() });
    }

    #[test]
    fn fuzzy_match10() {
        let needle = "cat.c";
        let haystack = "sys/dev/acpica/Osd/OsdTable.c";
        let mut h = Vec::new();
        let heatmap = heatmap(haystack, &[], &mut h);
        let m = fuzzy_match_test(
            heatmap,
            needle,
            haystack,
        );
        assert_eq!(m, Match { score: 142, positions: vec![12, 13, 22, 27, 28] });
    }

    #[test]
    fn fuzzy_match11() {
        let needle = "s";
        let haystack = "*scratch*";
        let mut h = Vec::new();
        let heatmap = heatmap(haystack, &[], &mut h);
        let m = fuzzy_match_test(
            heatmap,
            needle,
            haystack,
        );
        let expected_idx = 1;
        assert_eq!(m, Match { score: heatmap[expected_idx as usize], positions: vec![expected_idx] });
    }

    #[test]
    fn fuzzy_match_cache_reuse() {
        let mut reuse = ReuseState::new();
        let m1 = fuzzy_match(
            "foo",
            "foobar",
            &[],
            &mut reuse,
        );
        let m2 = fuzzy_match(
            "fo",
            "foobar",
            &[],
            &mut reuse,
        );
        let m3 = fuzzy_match(
            "oob",
            "foobar",
            &[],
            &mut reuse,
        );
        assert_eq!(m1, Match { score: 214, positions: vec![0, 1, 2] });
        assert_eq!(m2, Match { score: 142, positions: vec![0, 1] });
        assert_eq!(m3, Match { score: 126, positions: vec![1, 2, 3] });
    }
}
