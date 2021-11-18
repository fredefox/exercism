export type Maybe<A> = { just: A } | { nothing: null };

const nothing: <A>() => Maybe<A> = () => ({ nothing: null });

export type Deque<A> = Maybe<{ prev: Deque<A>; val: A; next: Deque<A> }>;

export const mkDeque: <A>() => Deque<A> = () => ({ nothing: null });

export const pop: <A>(d: Deque<A>) => Maybe<A> = (d) => {
  if ("nothing" in d) {
    return nothing();
  } else {
    const {
      just: { prev, val, next },
    } = d;
    if ("nothing" in prev) {
    } else {
      prev.just.next = next;
    }
    return { just: val };
  }
};

export const push: <A>(d: Deque<A>, v: A) => void = (d, v) => {
  if ("nothing" in d) {
    delete d.nothing;
    (d as any).just = { prev: mkDeque(), curr: v, next: mkDeque() };
  } else {
    const {
      just: { prev, val, next },
    } = d;
    prev.
    d.just = {};
  }
};
