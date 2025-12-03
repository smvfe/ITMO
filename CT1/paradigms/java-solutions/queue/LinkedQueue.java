package queue;

import java.util.Objects;

public class LinkedQueue extends AbstractQueue {
    private Node head, tail;

    public LinkedQueue() {
        head = null;
        tail = null;
        size = 0;
    }

    protected void enqueueI(Object obj) {
        Node node = new Node(obj);
        if (isEmpty()) {
            head = node;
        } else {
            tail.prev = node;
        }
        tail = node;
        ++size;
    }

    protected void dequeueI() {
        head = head.prev;
        --size;
    }

    protected Object elementI() {
        return head.value;
    }

    protected void clearI() {
        tail = null;
        head = null;
        size = 0;
    }

    protected int sizeI() {
        return size;
    }

    protected boolean isEmptyI() {
        return size == 0;
    }

    protected void setI(int index, Object obj) {
        Objects.checkIndex(index, size);
        Node current = tail;
        for (int i = 0; i < index; i++) {
            current = current.prev;
        }
        current.value = obj;
    }

    protected Object getI(int index) {
        Objects.checkIndex(index, size);
        Node current = tail;
        for (int i = 0; i < index; i++) {
            current = current.prev;
        }
        return current.value;
    }

    protected String toStrI() {
        StringBuilder sb = new StringBuilder("[");
        Node current = head;
        while (current != null) {
            if (current != head) {
                sb.append(", ");
            }
            sb.append(current.value);
            current = current.prev;
        }
        sb.append("]");
        return sb.toString();
    }

    private static class Node {
        private Object value;
        private Node prev;

        public Node(Object value) {
            assert value != null;
            this.value = value;
            this.prev = null;
        }
    }

}
