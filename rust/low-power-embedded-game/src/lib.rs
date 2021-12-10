// This stub file contains items which aren't used yet; feel free to remove this module attribute
// to enable stricter warnings.
#![allow(unused)]
use std::iter::Filter;

pub fn divmod(dividend: i16, divisor: i16) -> (i16, i16) {
    (dividend / divisor, dividend % divisor)
}

pub fn evens<T>(iter: impl Iterator<Item=T>) -> impl Iterator<Item=T> {
  let mut b: bool = false;
  // Question 1
  //
  // The inferred type of `f` seems to make type checking fail if I
  // let-bind `f` whereas if I inline it, typechecking succeeds. What
  // gives?
  //
  //   let f = move |_| { b = !b; b };
  //   iter.filter(f)
  //
  // Question 2
  //
  // Is there a compound assignment statement that I could replace
  //   b != b; b
  // with?
  iter.filter(move |_| { b = !b; b })
}

pub struct Position(pub i16, pub i16);
impl Position {
  pub fn manhattan(&self) -> i16 {
    // Question 3
    //
    // Is it possible to destructure the argument in the arg-list?
    let Position(a,b) = self;
    // Question 4
    //
    // Is there a polymorphic abs-function. In haskell `abs` has type:
    //
    //   Num n => n -> n
    //
    // Where `Num` is a class. Classes are similar to traits.
    i16::abs(*a) + i16::abs(*b)
  }
}
