type Nd<a> = null | { prev: Deque<a>, value: a, next: Deque<a> };
  
type Deque<a> = { head: Nd<a> };

type Maybe<a> = { just: a } | null;

const mkDeque = <a>(): Deque<a> => ({ head: null });

const pop = <a>(q: Deque<a>): Maybe<a> => {
  if(q.head === null) { return null; }
  const { prev, value, next } = q.head;
  if(next.head !== null) {
    next.head.prev = prev;
  }
  q.head = next.head;
  return { just: value };
};

const push = <a>(q: Deque<a>, a: a): void => {
  if(q.head == null) {
    q.head = { prev: mkDeque<a>(), value: a, next: mkDeque<a>() };
    return;
  }
  const { prev, value, next } = q.head;
  const newNext = { head: { prev: mkDeque<a>(), value, next } };
  q.head = { prev, value: a, next: newNext }
  if(next.head !== null) {
    next.head.prev = q;
  }
};

const main = () => {
  const q = mkDeque<number>();
  push(q, 2)
  push(q, 3);
  console.log(q, q.head.next);
  console.log(pop(q), pop(q), pop(q), pop(q));
  console.log(q);
};

main();
