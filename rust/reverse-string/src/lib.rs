use std::str::Chars;
use std::iter::Rev;

pub fn reverse(input: &str) -> String {
  let chars: Rev<Chars> = input.chars().rev();
  let mut s = String::new();
  for c in chars {
    s.push(c);
  }
  s
}
