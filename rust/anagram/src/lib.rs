use std::collections::HashSet;

pub fn anagrams_for<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
  // HashSet::from_iter(
  //   possible_anagrams
  //     .into_iter()
  //     .filter(|&&w| matches(word, w) )
  // )
  let mut h = HashSet::new();
  for w in possible_anagrams {
    if matches(word, w) { h.insert(*w); }
  }
  h
}

// a `match` b = a' /= b' && sort a == sort b
fn matches(a: &str, b: &str) -> bool {
  let a = a.to_lowercase();
  let b = b.to_lowercase();
  let mut aa = a.to_lowercase().chars().collect::<Vec<char>>();
  aa.sort();
  let mut bb = b.to_lowercase().chars().collect::<Vec<char>>();
  bb.sort();
  a != b && aa == bb
}
