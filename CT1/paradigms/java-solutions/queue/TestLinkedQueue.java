package queue;

public class TestLinkedQueue {
    public static void storeIntoQueue(LinkedQueue queue, String prefix) {
        for (int i = 0; i < 7; i++) {
            queue.enqueue(prefix + i);
        }
    }

    public static void printResult(LinkedQueue queue) {
        while (!queue.isEmpty()) {
            System.out.println(queue.size() + " " + queue.dequeue());
        }
    }

    public static void testWrapAround() {
        LinkedQueue queue = new LinkedQueue();
        storeIntoQueue(queue, "wrap_");
        printResult(queue);
        storeIntoQueue(queue, "wrap_");
        printResult(queue);
    }

    public static void testInterleavedOperations() {
        LinkedQueue queue = new LinkedQueue();
        queue.enqueue("first");
        queue.enqueue("second");
        System.out.println("Element: " + queue.element());
        queue.dequeue();
        queue.enqueue("third");
        System.out.println("Element: " + queue.element());
        printResult(queue);
    }

    public static void testCapacityExpansion() {
        LinkedQueue queue = new LinkedQueue();
        for (int i = 0; i < 15; i++) {
            queue.enqueue("elem_" + i);
        }
        printResult(queue);
    }

    public static void main(String[] args) {
        System.out.println("Testing wrap-around:");
        testWrapAround();

        System.out.println("\nTesting interleaved operations:");
        testInterleavedOperations();

        System.out.println("\nTesting capacity expansion:");
        testCapacityExpansion();
    }
}

