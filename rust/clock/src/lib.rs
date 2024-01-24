use std::fmt;
use std::ops::Div;

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Eq)]
pub struct Clock { value: i32 }

impl Clock {
  pub fn from_minutes(minutes: i32) -> Self {
    Clock { value : minutes.rem_euclid(60 * 24) }
  }

  pub fn new(hours: i32, minutes: i32) -> Self {
    Clock::from_minutes(hours * 60 + minutes)
  }

  pub fn add_minutes(&self, minutes: i32) -> Self {
    Clock::from_minutes(self.value + minutes)
  }
}

impl fmt::Display for Clock {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let (h, m) = divmod(self.value, 60);
    write!(f, "{:02}:{:02}", h, m)
  }
}

// fn divmod<T:Div<Output = T> + Rem<Output = T> + Copy>(t: T, u: T) -> (T, T) {
fn divmod(t: i32, u:i32) -> (i32, i32) {
  (t.div(u), t.rem_euclid(u))
}
// 1. Does rust have divmod
//
// 2. The readme tells me to derive `fmt::Display`, yet it seems I
// also needed to derive `Debug`, `PartialEq` and `Eq`. Is this how I
// was supposed to do it
//
// 3. I wanted to write a generic divmod function, but then it turns
// out that `rem_euclid` doesn't come from a trait like `div` does, so
// how would I write such a function?
