package queue;
// Hoare logic notation
// P - precondition
// Q - postcondition
// I - invariant

// Model: a[1...n]
// I: 0 <= n && a[i] != null, for i in (1 <= i <= n)
// Let immutable(j): a'[i] = a[i], for i in (1 <= i <= j)

public interface Queue {

    // P: obj != null
    // Q: immutable(n)
    //    && n' = n + 1
    //    && a'[n'] = obj
    void enqueue(Object obj);

    // P: n > 0
    // Q: R = a[1]
    //    && n' = n - 1
    //    && a'[i] = a[i + 1], for i in (1 <= i <= n')
    Object dequeue();

    // P: n > 0
    // Q: res = a[1]
    //    && n' = n
    //    && immutable(n)
    Object element();

    // P: true
    // Q: res = n
    //    && n' = n
    //    && immutable(n)
    int size();

    // P: true
    // Q: n = 0
    void clear();

    // P: true
    // Q: res = (n == 0)
    //    && n' = n
    //    && immutable(n)
    boolean isEmpty();

    // P: 0 <= index < size()
    // Q: R = a[n - index]
    Object get(int index);

    // P: 0 <= index < size() && obj != null
    // Q: a'[n - index] = obj && immutable(n)
    void set(int index, Object obj);

    // P: true
    // Q: res = '[' string(a[1]) ', ' ... ', ' string(a[n]) ']'
    String toStr();
}

