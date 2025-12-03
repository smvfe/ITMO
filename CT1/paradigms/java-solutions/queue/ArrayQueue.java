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
public class ArrayQueue extends AbstractQueue {
    private Object[] elements;
    private int head;

    public ArrayQueue() {
        elements = new Object[8];
        head = -1;
        size = 0;
    }

    // P: obj != null
    // Q: immutable(n)
    //    && n' = n + 1
    //    && a'[n'] = obj
    protected void enqueueI(Object obj) {
        capacityHandler();
        if (isEmptyI()) {
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
    protected void dequeueI() {
        if (size == 1) {
            clearI();
        } else {
            head = (head + 1) % elements.length;
            --size;
        }
    }

    // P: n > 0
    // Q: res = a[1]
    //    && n' = n
    //    && immutable(n)
    protected Object elementI() {
        return elements[head];
    }

    // P: true
    // Q: res = n
    //    && n' = n
    //    && immutable(n)
    protected int sizeI() {
        return size;
    }

    // P: true
    // Q: n = 0
    protected void clearI() {
        Arrays.fill(elements, null);
        head = -1;
        size = 0;
    }

    // P: true
    // Q: res = (n == 0)
    //    && n' = n
    //    && immutable(n)
    protected boolean isEmptyI() {
        return size == 0;
    }

    // P: 0 <= index < size()
    // Q: R = a[n - index]
    public Object getI(int index) {
        Objects.checkIndex(index, size);
        int actualIndex = (head + size - 1 - index) % elements.length;
        return elements[actualIndex];
    }

    // P: 0 <= index < size() && obj != null
    // Q: a'[n - index] = obj && immutable(n)
    public void setI(int index, Object obj) {
        assert obj != null;
        Objects.checkIndex(index, size);
        int actualIndex = (head + size - 1 - index) % elements.length;
        elements[actualIndex] = obj;
    }

    // P: true
    // Q: res = '[' string(a[1]) ', ' ... ', ' string(a[n]) ']'
    public String toStrI() {
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
    private void capacityHandler() {
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