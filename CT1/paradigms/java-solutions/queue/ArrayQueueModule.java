package queue;

import java.util.Arrays;
import java.util.Objects;

// Hoare logic notation
// P - precondition
// Q - postcondition
// I - invariant

// Model: a[1...n]
// I: 0 <= n && a[i] != null, for i in (1 <= i <= n)
// Let immutable(j): a'[i] = a[i], for i in (1 <= i <= j)
public class ArrayQueueModule {
    private static Object[] elements = new Object[8];
    private static int head = -1;
    private static int size = 0;

    // P: obj != null
    // Q: immutable(n)
    //    && n' = n + 1
    //    && a'[n'] = obj
    public static void enqueue(Object obj) {
        assert obj != null;
        capacityHandler();
        if (isEmpty()) {
            head = 0;
        }
        int tail = (head + size) % elements.length;
        elements[tail] = obj;
        ++size;
    }

    // P: n > 0
    // Q: R = a[1]
    //    && n' = n - 1
    //    && a'[i] = a[i + 1], for i in (1 <= i <= n')
    public static Object dequeue() {
        assert !isEmpty();
        Object obj = elements[head];
        if (size == 1) {
            clear();
        } else {
            head = (head + 1) % elements.length;
            --size;
        }
        return obj;
    }

    // P: n > 0
    // Q: res = a[1]
    //    && n' = n
    //    && immutable(n)
    public static Object element() {
        assert !isEmpty();
        return elements[head];
    }

    // P: true
    // Q: res = n
    //    && n' = n
    //    && immutable(n)
    public static int size() {
        return size;
    }

    // P: true
    // Q: n = 0
    public static void clear() {
        Arrays.fill(elements, null);
        head = -1;
        size = 0;
    }

    // P: true
    // Q: res = (n == 0)
    //    && n' = n
    //    && immutable(n)
    public static boolean isEmpty() {
        return size == 0;
    }

    // P: 0 <= index < size()
    // Q: R = a[n - index]
    public static Object get(int index) {
        Objects.checkIndex(index, size);
        int actualIndex = (head + size - 1 - index) % elements.length;
        return elements[actualIndex];
    }

    // P: 0 <= index < size() && obj != null
    // Q: a'[n - index] = obj && immutable(n)
    public static void set(int index, Object obj) {
        assert obj != null;
        Objects.checkIndex(index, size);
        int actualIndex = (head + size - 1 - index) % elements.length;
        elements[actualIndex] = obj;
    }

    // P: true
    // Q: res = '[' string(a[1]) ', ' ... ', ' string(a[n]) ']'
    public static String toStr() {
        if (isEmpty()) {
            return "[]";
        }
        StringBuilder sb = new StringBuilder("[");
        for (int i = 0; i < size; i++) {
            if (i > 0) {
                sb.append(", ");
            }
            sb.append(elements[(head + i) % elements.length]);
        }
        sb.append("]");
        return sb.toString();
    }

    // P: true
    // Q: immutable(n)
    //    && n' = n
    private static void capacityHandler() {
        if (size == elements.length) {
            Object[] newElements = new Object[2 * elements.length];
            for (int i = 0; i < size; i++) {
                newElements[i] = elements[(head + i) % elements.length];
            }
            elements = newElements;
            head = 0;
        }
    }
}