enum List<T> {
  Nil,
  Cons { head: T, tail: Box<List<T>> }
}

fn filter<F, T>(f: F, l: List<T>) -> List<T> where
  F: Fn(T) -> bool, T: Copy { match l {
    List::Nil => List::Nil,
    List::Cons { head: n, tail: ns } => {
      if f(n) { return List::Cons{ head: n, tail: ns} }
      filter(f, *ns)
    }
  }
}

fn go(l: List<u32>) -> List<u32> {
  match l {
    List::Cons { head: n, tail: ns } =>
      List::Cons { head: n, tail: Box::new(filter(|m| m % n != 0, *ns)) },
    List::Nil => panic!()
  }
}

fn index<T>(l: List<T>, n: u32) -> T {
  match l {
    List::Nil => panic!(),
    List::Cons { head: x, tail: xs } => {
      if n == 0 { return x }
      index(*xs, n-1)
    }
  }
}
pub fn nth(n: u32) -> u32 {
  index(go(List::Cons { head: 2, tail: Box::new(List::Nil) }), n)
}
