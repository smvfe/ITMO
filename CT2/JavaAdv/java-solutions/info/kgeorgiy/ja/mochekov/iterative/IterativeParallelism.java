package info.kgeorgiy.ja.mochekov.iterative;

import info.kgeorgiy.java.advanced.iterative.ListIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * Implementation of parallel list operations.
 * Processes lists by dividing them into chunks and processing each chunk in a separate thread.
 */
public class IterativeParallelism implements ListIP {
    private final ParallelMapper mapper;

    public IterativeParallelism() {
        this(null);
    }

    public IterativeParallelism(final ParallelMapper mapper) {
        this.mapper = mapper;
    }

    /**
     * Returns the index of the maximum element in the list according to the given comparator.
     *
     * @param threads number of concurrent threads.
     * @param values values to find maximum in.
     * @param comparator value comparator.
     * @param <T> value type.
     *
     * @throws InterruptedException if executing thread was interrupted.
     * @throws NoSuchElementException if no values are given.
     */
    @Override
    public <T> int argMax(final int threads, final List<T> values, final Comparator<? super T> comparator)
            throws InterruptedException {
        return parallelExecutor(
                threads,
                values,
                -1,
                // :NOTE: copy-paste
                // :NOTE: ??
                (_, range) -> getMax(values, comparator, IntStream.range(range.from, range.to).boxed(), range.from),
                stream -> getMax(values, comparator, stream.filter(idx -> idx != -1), -1)
        );
    }

    private static <T> Integer getMax(
            final List<T> values,
            final Comparator<? super T> comparator,
            final Stream<Integer> integerStream,
            final int other
    ) {
        return integerStream
                .max(Comparator.comparing(values::get, comparator))
                .orElse(other);
    }

    /**
     * Returns the index of the minimum element in the list according to the given comparator.
     *
     * @param threads number of concurrent threads.
     * @param values values to find minimum in.
     * @param comparator value comparator.
     * @param <T> value type.
     *
     * @return index of the first minimum in given values.
     *
     * @throws InterruptedException if executing thread was interrupted.
     * @throws NoSuchElementException if no values are given.
     */
    @Override
    public <T> int argMin(final int threads, final List<T> values, final Comparator<? super T> comparator) throws InterruptedException {
        return argMax(threads, values, comparator.reversed());
    }

    /**
     * Returns the first index of an element satisfying the given predicate.
     *
     * @param threads   number of concurrent threads.
     * @param values    values to test.
     * @param predicate test predicate.
     * @param <T>       value type.
     *
     * @return index of the first value satisfying the predicate, or {@code -1}, if there are none.
     *
     * @throws InterruptedException if executing thread was interrupted.
     */
    @Override
    public <T> int indexOf(final int threads, final List<T> values, final Predicate<? super T> predicate) throws InterruptedException {
        return searchIndex(threads, values, predicate, false);
    }

    /**
     * Returns the last index of an element satisfying the given predicate.
     *
     * @param threads   number of concurrent threads.
     * @param values    values to test.
     * @param predicate test predicate.
     * @param <T>       value type.
     *
     * @return index of the last value satisfying the predicate, or {@code -1}, if there are none.
     *
     * @throws InterruptedException if executing thread was interrupted.
     */
    @Override
    public <T> int lastIndexOf(final int threads, final List<T> values, final Predicate<? super T> predicate) throws InterruptedException {
        return searchIndex(threads, values, predicate, true);
    }

    /**
     * Returns the sum of indices of elements satisfying the given predicate.
     *
     * @param threads number of concurrent threads.
     * @param values values to test.
     * @param predicate test predicate.
     * @param <T> value type.
     *
     * @return sum of the indices of values satisfying the predicate.
     *
     * @throws InterruptedException if executing thread was interrupted.
     */
    @Override
    public <T> long sumIndices(final int threads, final List<? extends T> values, final Predicate<? super T> predicate)
            throws InterruptedException {
        return parallelExecutor(
                threads,
                values,
                0L,
                (_, range)
                        -> IntStream.range(range.from, range.to)
                        .filter(j -> predicate.test(values.get(j)))
                        .asLongStream().sum(),
                stream -> stream.mapToLong(Long::longValue).sum()
        );
    }

    /**
     * Returns indices of elements satisfying the given predicate in ascending order.
     *
     * @param threads number of concurrent threads.
     * @param values values to join.
     *
     * @return indices of the values matching predicate in ascending order.
     *
     * @throws InterruptedException if executing thread was interrupted.
     */
    @Override
    public <T> int[] indices(final int threads, final List<? extends T> values, final Predicate<? super T> predicate) throws InterruptedException {
        return this.<T, List<Integer>, int[]>parallelExecutor(
                threads,
                values,
                new ArrayList<>(),
                (vals, range) ->
                    IntStream.range(range.from, range.to).filter(j -> predicate.test(vals.get(j)))
                            .boxed()
                            .toList(),
                stream -> stream.flatMap(List::stream).mapToInt(Integer::intValue).toArray()
        );
    }

    /**
     * Filters the list, keeping only elements that satisfy the given predicate.
     *
     * @param threads number of concurrent threads.
     * @param values values to filter.
     * @param predicate filter predicate.
     *
     * @return list of values satisfying given predicate. Order of values is preserved.
     *
     * @throws InterruptedException if executing thread was interrupted.
     */
    @Override
    public <T> List<T> filter(final int threads, final List<? extends T> values, final Predicate<? super T> predicate)
            throws InterruptedException {
        final BiFunction2<T, List<T>> task = createTask(stream -> stream.filter(predicate));
        return parallelExecutor(threads, values, new ArrayList<>(), task,
                stream -> stream.flatMap(List::stream).toList());
    }

    /**
     * Maps each element in the list using the provided function.
     *
     * @param threads number of concurrent threads.
     * @param values values to map.
     * @param f mapper function.
     *
     * @return list of values mapped by given function.
     *
     * @throws InterruptedException if executing thread was interrupted.
     */
    @Override
    public <T, U> List<U> map(final int threads, final List<? extends T> values, final Function<? super T, ? extends U> f) throws InterruptedException {
        if (f == null) {
            throw new NullPointerException("Mapper function cannot be null");
        }

        final BiFunction2<T, List<U>> task = createTask(stream -> stream.map(f));
        return parallelExecutor(threads, values, new ArrayList<>(), task,
                stream -> stream.flatMap(List::stream).toList());
    }

    // :NOTE: remove me
    private static class ParallelExecutionContext<T, R> {
        List<Range> ranges;

        ParallelExecutionContext(int threads, final List<? extends T> values, final R initialValue) {
            threads = Math.min(threads, values.size());
            ranges = splitRanges(values.size(), threads);
        }

        public List<R> execute(
                final List<? extends T> values,
                final BiFunction<List<? extends T>, Range, R> task
        ) throws InterruptedException {
            final List<R> results = new ArrayList<>(Collections.nCopies(ranges.size(), null));
            final List<Thread> threads = IntStream.range(0, this.ranges.size())
                    .<Runnable>mapToObj(i -> () -> results.set(i, task.apply(values, ranges.get(i))))
                    .map(Thread::new).toList();
            threads.forEach(Thread::start);

            joinThreads(threads);
            return results;
        }

        private static void joinThreads(final List<Thread> threads) throws InterruptedException {
            InterruptedException thrownException = null;
            for (final Thread thread : threads) {
                while (thread.isAlive()) {
                    try {
                        thread.join();
                    } catch (final InterruptedException e) {
                        if (thrownException == null) {
                            thrownException = e;
                        } else {
                            thrownException.addSuppressed(e);
                        }
                    }
                }
            }

            if (thrownException != null) {
                throw thrownException;
            }
        }
    }

    private static List<Range> splitRanges(final int size, final int parts) {
        final List<Range> ranges = new ArrayList<>();
        final int actualThreads = Math.min(parts, size);

        final int chunkSize = size / parts;
        final int chunkRemain = size % parts;
        int start = 0;
        for (int i = 0; i < actualThreads; i++) {
            final int end = start + chunkSize + (i < chunkRemain ? 1 : 0);
            ranges.add(new Range(start, end));
            start = end;
        }
        return ranges;
    }

    private <T, R, U> U parallelExecutor(
            final int threads,
            final List<? extends T> values,
            final R initialValue,
            final BiFunction<List<? extends T>, Range, R> task,
            final Function<Stream<R>, U> streamCombiner
    ) throws InterruptedException {
        final ParallelExecutionContext<T, R> context = new ParallelExecutionContext<>(threads, values, initialValue);
        final List<R> results = mapper != null
                ? mapper.map(range -> task.apply(values, range), context.ranges)
                : context.execute(values, task);
        return streamCombiner.apply(results.stream());
    }

    private record Range(int from, int to) {}

    private interface BiFunction2<T, R> extends BiFunction<List<? extends T>, Range, R> {
        @Override
        R apply(List<? extends T> values, Range range);
    }

    private static <T, R> BiFunction2<T, List<R>> createTask(
            final Function<Stream<T>, Stream<R>> processor) {
        return (vals, range) ->
                processor.apply(IntStream.range(range.from, range.to)
                                .mapToObj(vals::get))
                        .collect(Collectors.toCollection(ArrayList::new));
    }

    private <T> int searchIndex(
            final int threads,
            final List<? extends T> values,
            final Predicate<? super T> predicate,
            final boolean isLastIndex) throws InterruptedException {

        return parallelExecutor(threads, values, -1,
                (_, range) -> {
                    IntStream stream = IntStream.range(range.from, range.to);
                    if (isLastIndex) {
                        stream = stream.map(i -> range.to - 1 - (i - range.from));
                    }
                    return stream
                            .filter(j -> predicate.test(values.get(j)))
                            .findFirst()
                            .orElse(-1);
                },
                stream -> stream
                        .filter(idx -> idx != -1)
                        .reduce(isLastIndex ? Integer::max : Integer::min)
                        .orElse(-1));
    }
}