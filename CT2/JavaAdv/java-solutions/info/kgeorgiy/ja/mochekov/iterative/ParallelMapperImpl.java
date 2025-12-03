package info.kgeorgiy.ja.mochekov.iterative;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

/**
 * Implementation of {@link ParallelMapper}
 * Maps function over lists.
 */
public class ParallelMapperImpl implements ParallelMapper {
    private final List<Thread> workers;
    private final ArrayDeque<Runnable> tasks;
    private volatile boolean closed;

    public ParallelMapperImpl(int threads) {
        workers = new ArrayList<>();
        tasks = new ArrayDeque<>();
        closed = false;

        for (int i = 0; i < threads; i++) {
            Thread worker = new Thread(() -> {
                try {
                    while (!Thread.interrupted()) {
                        final Runnable task = takeTask();
                        if (task == null) {
                            return;
                        }
                        task.run();
                    }
                } catch (InterruptedException ignored) {
                }
            });
            workers.add(worker);
            worker.start();
        }
    }

    /**
     * Maps function {@code f} over specified {@code items}.
     * Mapping for each item is performed in parallel.
     *
     * @throws InterruptedException if calling thread was interrupted
     */
    @Override
    public <T, R> List<R> map(Function<? super T, ? extends R> f, List<? extends T> items) throws InterruptedException {
        if (closed) {
            throw new IllegalStateException("ParallelMapper unavailable");
        }

        final jobWarden<R> jobWarden = new jobWarden<>(items.size());
        for (int i = 0; i < items.size(); i++) {
            final int index = i;
            final T item = items.get(i);
            synchronized (tasks) {
                tasks.addLast(() -> {
                    try {
                        R result = f.apply(item);
                        jobWarden.complete(index, result);
                    } catch (Throwable e) {
                        jobWarden.setException(e);
                    }
                });
                tasks.notifyAll();
            }
        }

        return jobWarden.await();
    }

    /**
     * Stops all threads. All unfinished mappings throw exception.
     */
    @Override
    public void close() {
        synchronized (tasks) {
            closed = true;
            tasks.notifyAll();
        }

        for (Thread worker : workers) {
            try {
                worker.join();
            } catch (InterruptedException ignored) {
                Thread.currentThread().interrupt();
            }
        }
    }

    private Runnable takeTask() throws InterruptedException {
        synchronized (tasks) {
            while (tasks.isEmpty() && !closed) {
                tasks.wait();
            }
            if (closed) {
                return null;
            }
            return tasks.pollFirst();
        }
    }

    private static class jobWarden<R> {
        private final List<R> results;
        private int remaining;
        private Throwable exception;

        public jobWarden(int size) {
            this.results = new ArrayList<>(Collections.nCopies(size, null));
            this.remaining = size;
        }

        public synchronized void complete(int index, R result) {
            results.set(index, result);
            remaining--;
            if (remaining == 0) {
                notifyAll();
            }
        }

        public synchronized void setException(Throwable e) {
            if (exception == null) {
                exception = e;
            } // :NOTE: желательно собирать весь stack trace ошибок, не только первую
            notifyAll();
        }

        public synchronized List<R> await() throws InterruptedException {
            while (remaining > 0 && exception == null) {
                wait();
            }

            if (exception != null) {
                if (exception instanceof RuntimeException) {
                    throw (RuntimeException) exception;
                } else if (exception instanceof Error) {
                    throw (Error) exception;
                }
            }
            return results;
        }
    }
}