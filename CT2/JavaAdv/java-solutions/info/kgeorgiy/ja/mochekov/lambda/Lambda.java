package info.kgeorgiy.ja.mochekov.lambda;

import info.kgeorgiy.java.advanced.lambda.HardLambda;
import info.kgeorgiy.java.advanced.lambda.Trees;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;

@SuppressWarnings("unused")
public class Lambda implements HardLambda {

    @Override
    public <T> Spliterator<T> binaryTreeSpliterator(Trees.Binary<T> tree) {
        return new AbstractTreeSpliterator<>(tree) {
            @Override
            protected boolean isLeaf(Trees.Binary<T> node) {
                return node instanceof Trees.Leaf;
            }

            @Override
            protected boolean isBranch(Trees.Binary<T> node) {
                return node instanceof Trees.Binary.Branch;
            }

            @Override
            protected T extractValue(Trees.Binary<T> node) {
                return ((Trees.Leaf<T>) node).value();
            }

            @Override
            protected void pushChildren(Trees.Binary<T> node) {
                Trees.Binary.Branch<T> branch = (Trees.Binary.Branch<T>) node;
                if (branch.right() != null) stack.push(branch.right());
                if (branch.left() != null) stack.push(branch.left());
            }

            @Override
            protected Spliterator<T> splitNodeAndReturnSpliterator() {
                Trees.Binary<T> node = stack.pop();
                Trees.Binary.Branch<T> branch = (Trees.Binary.Branch<T>) node;

                if (branch.left() != null) {
                    if (branch.right() != null) {
                        stack.push(branch.right());
                    }
                    return binaryTreeSpliterator(branch.left());
                } else if (branch.right() != null) {
                    stack.push(branch.right());
                }
                return null;
            }

            @Override
            public int characteristics() {
                return tree instanceof Trees.Leaf ?
                        SUBSIZED | ORDERED | IMMUTABLE | SIZED :
                        SUBSIZED | ORDERED | IMMUTABLE;
            }

            @Override
            public long estimateSize() {
                return tree instanceof Trees.Leaf ? 1 : Long.MAX_VALUE;
            }
        };
    }

    @Override
    public <T> Spliterator<T> sizedBinaryTreeSpliterator(Trees.SizedBinary<T> tree) {
        return new AbstractTreeSpliterator<>(tree) {
            private final int size = tree != null ? tree.size() : 0;

            @Override
            protected boolean isLeaf(Trees.SizedBinary<T> node) {
                return node instanceof Trees.Leaf;
            }

            @Override
            protected boolean isBranch(Trees.SizedBinary<T> node) {
                return node instanceof Trees.SizedBinary.Branch;
            }

            @Override
            protected T extractValue(Trees.SizedBinary<T> node) {
                return ((Trees.Leaf<T>) node).value();
            }

            @Override
            protected void pushChildren(Trees.SizedBinary<T> node) {
                Trees.SizedBinary.Branch<T> branch = (Trees.SizedBinary.Branch<T>) node;
                if (branch.right() != null) stack.push(branch.right());
                if (branch.left() != null) stack.push(branch.left());
            }

            @Override
            protected Spliterator<T> splitNodeAndReturnSpliterator() {
                Trees.SizedBinary<T> node = stack.pop();
                Trees.SizedBinary.Branch<T> branch = (Trees.SizedBinary.Branch<T>) node;

                if (branch.left() != null) {
                    if (branch.right() != null) {
                        stack.push(branch.right());
                    }
                    return sizedBinaryTreeSpliterator(branch.left());
                } else if (branch.right() != null) {
                    stack.push(branch.right());
                }
                return null;
            }

            @Override
            public long estimateSize() {
                return size;
            }

            @Override
            public int characteristics() {
                return SIZED | SUBSIZED | ORDERED | IMMUTABLE;
            }
        };
    }

    @Override
    public <T> Spliterator<T> naryTreeSpliterator(Trees.Nary<T> tree) {
        return new AbstractTreeSpliterator<>(tree) {
            @Override
            protected boolean isLeaf(Trees.Nary<T> node) {
                return node instanceof Trees.Leaf;
            }

            @Override
            protected boolean isBranch(Trees.Nary<T> node) {
                return node instanceof Trees.Nary.Node;
            }

            @Override
            protected T extractValue(Trees.Nary<T> node) {
                return ((Trees.Leaf<T>) node).value();
            }

            @Override
            protected void pushChildren(Trees.Nary<T> node) {
                Trees.Nary.Node<T> branch = (Trees.Nary.Node<T>) node;
                for (int i = branch.children().size() - 1; i >= 0; i--) {
                    stack.push(branch.children().get(i));
                }
            }

            @Override
            protected Spliterator<T> splitNodeAndReturnSpliterator() {
                Trees.Nary<T> node = stack.pop();
                if (node instanceof Trees.Nary.Node<T>(var children) && children.size() > 1) {
                    int mid = children.size() / 2;
                    List<Trees.Nary<T>> firstHalf = new ArrayList<>(children.subList(0, mid));
                    Trees.Nary.Node<T> firstNode = new Trees.Nary.Node<>(firstHalf);

                    List<Trees.Nary<T>> secondHalf = new ArrayList<>(children.subList(mid, children.size()));
                    for (int i = secondHalf.size() - 1; i >= 0; i--) {
                        stack.push(secondHalf.get(i));
                    }

                    return naryTreeSpliterator(firstNode);
                }
                return null;
            }

            @Override
            public int characteristics() {
                return tree instanceof Trees.Leaf ?
                        SUBSIZED | ORDERED | IMMUTABLE | SIZED :
                        SUBSIZED | ORDERED | IMMUTABLE;
            }

            @Override
            public long estimateSize() {
                return tree instanceof Trees.Leaf ? 1 : Long.MAX_VALUE;
            }
        };
    }

    @Override
//    public <T> Spliterator<T> sizedBinaryTreeSpliterator(Trees.SizedBinary<T> tree) {
    public <T> Spliterator<T> nestedBinaryTreeSpliterator(Trees.Binary<List<T>> tree) {
        return new AbstractNestedTreeSpliterator<>(tree) {
            @Override
            protected List<T> extractListIfLeaf(Trees.Binary<List<T>> node) {
                if (node instanceof Trees.Leaf<List<T>>(var list)) {
                    return list;
                }
                return null;
            }

            @Override
            protected void pushChildren(Trees.Binary<List<T>> node) {
                if (node instanceof Trees.Binary.Branch<List<T>>(var left, var right)) {
                    if (right != null) stack.push(right);
                    if (left != null) stack.push(left);
                }
            }

            @Override
            protected Spliterator<T> createSpliterator(Trees.Binary<List<T>> node) {
                return nestedBinaryTreeSpliterator(node);
            }
        };
    }

    @Override
    public <T> Spliterator<T> nestedSizedBinaryTreeSpliterator(Trees.SizedBinary<List<T>> tree) {
        return new AbstractNestedTreeSpliterator<>(tree) {
            @Override
            protected List<T> extractListIfLeaf(Trees.SizedBinary<List<T>> node) {
                if (node instanceof Trees.Leaf<List<T>>(var list)) {
                    return list;
                }
                return null;
            }

            @Override
            protected void pushChildren(Trees.SizedBinary<List<T>> node) {
                if (node instanceof Trees.SizedBinary.Branch<List<T>>(var left, var right, var size)) {
                    if (right != null) stack.push(right);
                    if (left != null) stack.push(left);
                }
            }

            @Override
            protected Spliterator<T> createSpliterator(Trees.SizedBinary<List<T>> node) {
                return nestedSizedBinaryTreeSpliterator(node);
            }
        };
    }

    @Override
    public <T> Spliterator<T> nestedNaryTreeSpliterator(Trees.Nary<List<T>> tree) {
        return new AbstractNestedTreeSpliterator<>(tree) {
            @Override
            protected List<T> extractListIfLeaf(Trees.Nary<List<T>> node) {
                if (node instanceof Trees.Leaf<List<T>>(var list)) {
                    return list;
                }
                return null;
            }

            @Override
            protected void pushChildren(Trees.Nary<List<T>> node) {
                if (node instanceof Trees.Nary.Node<List<T>>(var children)) {
                    for (int i = children.size() - 1; i >= 0; i--) {
                        stack.push(children.get(i));
                    }
                }
            }

            @Override
            protected Spliterator<T> createSpliterator(Trees.Nary<List<T>> node) {
                return nestedNaryTreeSpliterator(node);
            }
        };
    }

    private <T, A> Collector<T, ?, Optional<T>> positionCollector(
            Supplier<A> supplier,
            BiConsumer<A, T> accumulator,
            BinaryOperator<A> combiner,
            Function<A, Optional<T>> finisher) {
        return Collector.of(supplier, accumulator, combiner, finisher);
    }

    @Override
    public <T> Collector<T, ?, Optional<T>> first() {
        return positionCollector(
                () -> new Object() {
                    T value = null;
                    boolean found = false;
                },
                (a, t) -> {
                    if (!a.found) {
                        a.value = t;
                        a.found = true;
                    }
                },
                (a1, a2) -> a1.found ? a1 : a2,
                a -> Optional.ofNullable(a.value)
        );
    }

    @Override
    public <T> Collector<T, ?, Optional<T>> last() {
        return positionCollector(
                () -> new Object() {
                    T value = null;
                },
                (a, t) -> a.value = t,
                (a1, a2) -> {
                    if (a2.value != null) a1.value = a2.value;
                    return a1;
                },
                a -> Optional.ofNullable(a.value)
        );
    }

    @Override
    public <T> Collector<T, ?, Optional<T>> middle() {
        return positionCollector(
                (Supplier<List<T>>) ArrayList::new,
                List::add,
                (left, right) -> {
                    left.addAll(right);
                    return left;
                },
                list -> list.isEmpty() ? Optional.empty() : Optional.of(list.get(list.size() / 2))
        );
    }

    @Override
    public <T> Collector<T, ?, Optional<T>> kth(final int k) {
        return positionCollector(
                () -> new Object() {
                    T value = null;
                    int count = 0;
                },
                (a, element) -> {
                    if (a.count == k) {
                        a.value = element;
                    }
                    a.count++;
                },
                (a, a1) -> {
                    if (a.count <= k && k < a.count + a1.count) {
                        a.value = a1.value;
                    }
                    a.count += a1.count;
                    return a;
                },
                a -> (k >= 0 && a.count > k) ?
                        Optional.ofNullable(a.value) : Optional.empty()
        );
    }

    private Collector<CharSequence, ?, String> commonFix(boolean isPrefix) {
        // :NOTE: Collectors.reducing()
        return Collector.of(
                () -> new Object() {
                    final StringBuilder fix = new StringBuilder();
                    boolean first = true;
                },
                (a, seq) -> {
                    if (a.first) {
                        a.fix.append(seq);
                        a.first = false;
                    } else {
                        if (isPrefix) {
                            updateCommonPrefix(a.fix, seq);
                        } else {
                            updateCommonSuffix(a.fix, seq);
                        }
                    }
                },
                (a1, a2) -> {
                    if (a1.first) return a2;
                    if (a2.first) return a1;

                    if (isPrefix) {
                        updateCommonPrefix(a1.fix, a2.fix);
                    } else {
                        updateCommonSuffix(a1.fix, a2.fix);
                    }
                    return a1;
                },
                a -> a.fix.toString()
        );
    }

    @Override
    public Collector<CharSequence, ?, String> commonPrefix() {
        return commonFix(true);
    }

    @Override
    public Collector<CharSequence, ?, String> commonSuffix() {
        return commonFix(false);
    }

    private void updateCommonPart(StringBuilder sb, CharSequence seq, boolean isPrefix) {
        if (sb.isEmpty() || seq.isEmpty()) {
            sb.setLength(0);
            return;
        }

        int sbLen = sb.length();
        int seqLen = seq.length();
        int commonLength = 0;

        if (isPrefix) {
            int minLen = Math.min(sbLen, seqLen);
            while (commonLength < minLen && sb.charAt(commonLength) == seq.charAt(commonLength)) {
                commonLength++;
            }
            sb.setLength(commonLength);
        } else {
            while (commonLength < sbLen && commonLength < seqLen &&
                    sb.charAt(sbLen - 1 - commonLength) == seq.charAt(seqLen - 1 - commonLength)) {
                commonLength++;
            }
            if (commonLength > 0) {
                String suffix = sb.substring(sbLen - commonLength, sbLen);
                sb.setLength(0);
                sb.append(suffix);
            } else {
                sb.setLength(0);
            }
        }
    }

    private void updateCommonPrefix(StringBuilder prefix, CharSequence other) {
        updateCommonPart(prefix, other, true);
    }

    private void updateCommonSuffix(StringBuilder sb, CharSequence seq) {
        updateCommonPart(sb, seq, false);
    }

    @Override
    public <T> Collector<T, ?, List<T>> head(int k) {
        return Collector.of(
                (Supplier<ArrayList<T>>) ArrayList::new,
                (list, element) -> {
                    if (list.size() < k) {
                        list.add(element);
                    }
                },
                (list1, list2) -> {
                    ArrayList<T> result = new ArrayList<>(list1);
                    for (int i = 0; i < list2.size() && result.size() < k; i++) {
                        result.add(list2.get(i));
                    }
                    return result;
                },
                ArrayList::new,
                Collector.Characteristics.IDENTITY_FINISH
        );
    }

    @Override
    public <T> Collector<T, ?, List<T>> tail(int k) {
        return Collector.of(
                (Supplier<Deque<T>>) ArrayDeque::new,
                (deque, element) -> {
                    deque.addLast(element);
                    if (deque.size() > k) {
                        deque.removeFirst();
                    }
                },
                (deque1, deque2) -> {
                    Deque<T> result = new ArrayDeque<>(deque1);
                    for (T element : deque2) {
                        result.addLast(element);
                        if (result.size() > k) {
                            result.removeFirst();
                        }
                    }
                    return result;
                },
                ArrayList::new
        );
    }

}