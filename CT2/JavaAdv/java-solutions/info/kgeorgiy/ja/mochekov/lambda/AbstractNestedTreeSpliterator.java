package info.kgeorgiy.ja.mochekov.lambda;

import java.util.*;
import java.util.function.Consumer;

public abstract class AbstractNestedTreeSpliterator<T, N> implements Spliterator<T> {
    protected final Deque<N> stack = new ArrayDeque<>();
    protected Iterator<T> currentIterator = null;

    protected AbstractNestedTreeSpliterator(N root) {
        if (root != null) {
            stack.push(root);
        }
    }

    @Override
    public boolean tryAdvance(Consumer<? super T> action) {
        if (currentIterator != null && currentIterator.hasNext()) {
            action.accept(currentIterator.next());
            return true;
        }

        while (!stack.isEmpty()) {
            N node = stack.pop();
            List<T> list = extractListIfLeaf(node);
            if (list != null && !list.isEmpty()) {
                currentIterator = list.iterator();
                action.accept(currentIterator.next());
                return true;
            } else {
                pushChildren(node);
            }
        }

        return false;
    }

    @Override
    public Spliterator<T> trySplit() {
        if (stack.isEmpty()) {
            return null;
        }

        N node = stack.poll();
        if (node != null) {
            return createSpliterator(node);
        }
        return null;
    }

    @Override
    public void forEachRemaining(Consumer<? super T> action) {
        if (currentIterator != null) {
            currentIterator.forEachRemaining(action);
            currentIterator = null;
        }

        while (!stack.isEmpty()) {
            N node = stack.pop();
            List<T> list = extractListIfLeaf(node);
            if (list != null) {
                list.forEach(action);
            } else {
                pushChildren(node);
            }
        }
    }

    protected abstract List<T> extractListIfLeaf(N node);

    protected abstract void pushChildren(N node);

    protected abstract Spliterator<T> createSpliterator(N node);

    @Override
    public long estimateSize() {
        return Long.MAX_VALUE;
    }

    @Override
    public int characteristics() {
        return ORDERED;
    }
}