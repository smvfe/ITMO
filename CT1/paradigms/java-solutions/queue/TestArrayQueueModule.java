package queue;

public class TestArrayQueueModule {
    public static void storeIntoQueue(String prefix) {
        for (int i = 0; i < 15; i++) {
            ArrayQueueModule.enqueue(prefix + i);
        }
    }

    public static void printResult() {
        while (!ArrayQueueModule.isEmpty()) {
            System.out.println(
                    ArrayQueueModule.size() + " " +
                            ArrayQueueModule.element() + " " +
                            ArrayQueueModule.dequeue()
            );
        }
    }

    public static void testPeek() {
        storeIntoQueue("peek_");
        System.out.println("Initial queue: " + ArrayQueueModule.toStr());
        System.out.println("Peek element: " + ArrayQueueModule.element());
        printResult();
    }

    public static void testMixedOperations() {
        ArrayQueueModule.enqueue("first");
        System.out.println("Queue after enqueues: " + ArrayQueueModule.toStr());
        System.out.println("Dequeue: " + ArrayQueueModule.dequeue());
        ArrayQueueModule.enqueue("third");
        System.out.println("Queue after mixed operations: " + ArrayQueueModule.toStr());
        printResult();
    }

    public static void testBoundaryConditions() {
        storeIntoQueue("boundary_");
        System.out.println("Queue before boundary test: " + ArrayQueueModule.toStr());
        for (int i = 0; i < 15; i++) {
            System.out.println("Dequeue: " + ArrayQueueModule.dequeue());
        }
        System.out.println("Queue after boundary test: " + ArrayQueueModule.toStr());
    }

    public static void main(String[] args) {
        System.out.println("Testing peek:");
        testPeek();

        System.out.println("\nTesting mixed operations:");
        testMixedOperations();

        System.out.println("\nTesting boundary conditions:");
        testBoundaryConditions();
    }
}