package info.kgeorgiy.ja.mochekov.lambda;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Spliterator;
import java.util.function.Consumer;

public abstract class AbstractTreeSpliterator<T, N> implements Spliterator<T> {
    protected final Deque<N> stack = new ArrayDeque<>();

    protected AbstractTreeSpliterator(N root) {
        if (root != null) {
            stack.push(root);
        }
    }

    @Override
    public boolean tryAdvance(Consumer<? super T> action) {
        if (stack.isEmpty()) {
            return false;
        }
        N node = stack.pop();
        if (isLeaf(node)) {
            action.accept(extractValue(node));
            return true;
        } else if (isBranch(node)) {
            pushChildren(node);
            return tryAdvance(action);
        }
        return false;
    }

    @Override
    public Spliterator<T> trySplit() {
        if (stack.isEmpty()) {
            return null;
        }

        N node = stack.peek();
        if (node == null) {
            return null;
        }

        if (isBranch(node)) {
            return splitNodeAndReturnSpliterator();
        }

        return null;
    }

    protected abstract boolean isLeaf(N node);

    protected abstract boolean isBranch(N node);

    protected abstract T extractValue(N node);

    protected abstract void pushChildren(N node);

    protected abstract Spliterator<T> splitNodeAndReturnSpliterator();

    @Override
    public long estimateSize() {
        return Long.MAX_VALUE;
    }

    @Override
    public int characteristics() {
        return ORDERED | IMMUTABLE;
    }
}