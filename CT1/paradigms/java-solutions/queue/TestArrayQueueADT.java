package queue;

public class TestArrayQueueADT {
    public static void storeIntoQueue(ArrayQueueADT queue, String prefix) {
        for (int i = 0; i < 7; i++) {
            ArrayQueueADT.enqueue(queue, prefix + i);
        }
    }

    public static void printResult(ArrayQueueADT queue) {
        while (!ArrayQueueADT.isEmpty(queue)) {
            System.out.println(
                    ArrayQueueADT.size(queue) + " " +
                            ArrayQueueADT.element(queue) + " " +
                            ArrayQueueADT.dequeue(queue)
            );
        }
    }

    public static void testGetAndSet(ArrayQueueADT queue) {
        storeIntoQueue(queue, "test_");
        System.out.println("Initial queue: " + ArrayQueueADT.toStr(queue));
        System.out.println("Get element at index 2 (from tail): " + ArrayQueueADT.get(queue, 2));
        ArrayQueueADT.set(queue, 2, "modified");
        printResult(queue);
    }

    public static void testClear(ArrayQueueADT queue) {
        storeIntoQueue(queue, "clear_");
        System.out.println("Queue before clear: " + ArrayQueueADT.toStr(queue));
        ArrayQueueADT.clear(queue);
        System.out.println("Queue after clear: " + ArrayQueueADT.toStr(queue));
    }

    public static void main(String[] args) {
        ArrayQueueADT queue1 = new ArrayQueueADT();
        ArrayQueueADT queue2 = new ArrayQueueADT();

        System.out.println("Testing storeIntoQueue and printResult:");
        storeIntoQueue(queue1, "s1_");
        storeIntoQueue(queue2, "s2_");
        printResult(queue1);
        printResult(queue2);

        System.out.println("\nTesting get and set:");
        testGetAndSet(new ArrayQueueADT());

        System.out.println("\nTesting clear:");
        testClear(new ArrayQueueADT());
    }
}