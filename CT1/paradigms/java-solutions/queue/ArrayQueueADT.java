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
public class ArrayQueueADT {
    private Object[] elements;
    private int head;
    private int size;

    public ArrayQueueADT() {
        elements = new Object[8];
        head = -1;
        size = 0;
    }

    // P: queue != null && obj != null
    // Q: immutable(n)
    //    && n' = n + 1
    //    && a'[n'] = obj
    public static void enqueue(ArrayQueueADT queue, Object element) {
        assert element != null;
        capacityHandler(queue);
        if (queue.head == -1) {
            queue.head = 0;
        }
        int tail = (queue.head + queue.size) % queue.elements.length;
        queue.elements[tail] = element;
        ++queue.size;
    }

    // P: n > 0 && queue != null
    // Q: R = a[1]
    //    && n' = n - 1
    //    && a'[i] = a[i + 1], for i in (1 <= i <= n')
    public static Object dequeue(ArrayQueueADT queue) {
        assert !isEmpty(queue);
        Object element = queue.elements[queue.head];
        if (queue.size == 1) {
            queue.head = -1;
        } else {
            queue.head = (queue.head + 1) % queue.elements.length;
        }
        queue.size--;
        return element;
    }

    // P: n > 0 && queue != null
    // Q: res = a[1]
    //    && n' = n
    //    && immutable(n)
    public static Object element(ArrayQueueADT queue) {
        assert !isEmpty(queue);
        return queue.elements[queue.head];
    }

    // P: queue != null
    // Q: res = n
    //    && n' = n
    //    && immutable(n)
    public static int size(ArrayQueueADT queue) {
        return queue.size;
    }

    // P: true
    // Q: n = 0
    public static void clear(ArrayQueueADT queue) {
        Arrays.fill(queue.elements, null);
        queue.head = -1;
        queue.size = 0;
    }

    // P: queue != null
    // Q: res = (n == 0)
    //    && n' = n
    //    && immutable(n)
    public static boolean isEmpty(ArrayQueueADT queue) {
        return queue.size == 0;
    }

    // P: 0 <= index < n-1
    // Q: res = a[n - index]
    public static Object get(ArrayQueueADT queue, int index) {
        Objects.checkIndex(index, size(queue));
        int actualIndex = (queue.head + queue.size - 1 - index) % queue.elements.length;
        return queue.elements[actualIndex];
    }

    // P: 0 <= index < n-1
    //    && obj != null
    //    && queue != null
    // Q: a'[n - index] = obj
    //    && immutable(n)
    public static void set(ArrayQueueADT queue, int index, Object obj) {
        assert obj != null;
        Objects.checkIndex(index, size(queue));
        int actualIndex = (queue.head + queue.size - 1 - index) % queue.elements.length;
        queue.elements[actualIndex] = obj;
    }

    // P: queue != null
    // Q: res = '[' string(a[1]) ', ' ... ', ' string(a[n]) ']'
    public static String toStr(ArrayQueueADT queue) {
        if (isEmpty(queue)) {
            return "[]";
        }
        StringBuilder sb = new StringBuilder("[");
        for (int i = 0; i < size(queue); i++) {
            if (i > 0) {
                sb.append(", ");
            }
            sb.append(queue.elements[(queue.head + i) % queue.elements.length]);
        }
        sb.append("]");
        return sb.toString();
    }

    // P: queue != null
    // Q: immutable(n)
    //    && n' = n
    private static void capacityHandler(ArrayQueueADT queue) {
        if (queue.size == queue.elements.length) {
            Object[] newElements = new Object[2 * queue.elements.length];
            for (int i = 0; i < queue.size; i++) {
                newElements[i] = queue.elements[(queue.head + i) % queue.elements.length];
            }
            queue.elements = newElements;
            queue.head = 0;
        }
    }
}