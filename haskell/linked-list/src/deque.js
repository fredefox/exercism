const mkDeque = () => ({ head: null });
const pop = (q) => {
    if (q.head === null) {
        return null;
    }
    const { prev, value, next } = q.head;
    if (next.head !== null) {
        next.head.prev = prev;
    }
    q.head = next.head;
    return { just: value };
};
const push = (q, a) => {
    if (q.head == null) {
        q.head = { prev: mkDeque(), value: a, next: mkDeque() };
        return;
    }
    const { prev, value, next } = q.head;
    const newNext = { head: { prev: mkDeque(), value, next } };
    q.head = { prev, value: a, next: newNext };
    if (next.head !== null) {
        next.head.prev = q;
    }
};
const main = () => {
    const q = mkDeque();
    push(q, 2);
    push(q, 3);
    console.log(q, q.head.next);
    console.log(pop(q), pop(q), pop(q), pop(q));
    console.log(q);
};
main();
