package queue;

import java.util.Objects;

public class TestQueue {
    public static void fill(Queue queue, String prefix) {
        for (int i = 0; i < 10; i++) {
            queue.enqueue(prefix + i);
        }
    }

    public static void dump(Queue queue) {
        while (!queue.isEmpty()) {
            System.out.println(queue.size() + " " + queue.dequeue());
        }
    }

    public static void testConsistency(Queue queue1, Queue queue2) {
        fill(queue1, "q1_");
        fill(queue2, "q1_");

        while (!queue1.isEmpty() && !queue2.isEmpty()) {
            Object elem1 = queue1.dequeue();
            Object elem2 = queue2.dequeue();
            assert Objects.equals(elem1, elem2) : "Elements do not match: " + elem1 + " != " + elem2;
        }

        assert queue1.isEmpty() && queue2.isEmpty() : "Queues are not empty at the same time";
    }

    public static void testWrapAround(Queue queue1, Queue queue2) {
        fill(queue1, "wrap_");
        fill(queue2, "wrap_");

        dump(queue1);
        dump(queue2);

        fill(queue1, "wrap_");
        fill(queue2, "wrap_");

        dump(queue1);
        dump(queue2);
    }

    public static void testInterleavedOperations(Queue queue1, Queue queue2) {
        queue1.enqueue("first");
        queue2.enqueue("first");

        queue1.enqueue("second");
        queue2.enqueue("second");

        assert Objects.equals(queue1.element(), queue2.element()) : "Elements do not match";

        queue1.dequeue();
        queue2.dequeue();

        queue1.enqueue("third");
        queue2.enqueue("third");

        assert Objects.equals(queue1.element(), queue2.element()) : "Elements do not match";

        dump(queue1);
        dump(queue2);
    }

    public static void testCapacityExpansion(Queue queue1, Queue queue2) {
        for (int i = 0; i < 15; i++) {
            queue1.enqueue("elem_" + i);
            queue2.enqueue("elem_" + i);
        }

        dump(queue1);
        dump(queue2);
    }

    public static void main(String[] args) {
        Queue linkedQueue = new LinkedQueue();
        Queue arrayQueue = new ArrayQueue();

        System.out.println("Testing consistency:");
        testConsistency(linkedQueue, arrayQueue);

        System.out.println("\nTesting wrap-around:");
        testWrapAround(linkedQueue, arrayQueue);

        System.out.println("\nTesting interleaved operations:");
        testInterleavedOperations(linkedQueue, arrayQueue);

        System.out.println("\nTesting capacity expansion:");
        testCapacityExpansion(linkedQueue, arrayQueue);
    }
}