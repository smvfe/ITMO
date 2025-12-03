package info.kgeorgiy.ja.mochekov.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Implementation of the web crawler that downloads web pages in parallel.
 * This crawler traverses the web starting from a specified URL up to a specified depth,
 * with the ability to filter out hosts containing specific substrings.
 */
public class WebCrawler implements NewCrawler {
    private final int perHost;
    private final Downloader downloader;
    private final ExecutorService downloadPool;
    private final ExecutorService extractPool;

    /**
     * Creates a new WebCrawler instance with the specified parameters.
     *
     * @param downloader the downloader to fetch web pages content
     * @param downloaders number of threads for parallel page downloads
     * @param extractors number of threads for parallel link extraction
     * @param perHost maximum number of concurrent downloads from a single host
     */
    public WebCrawler(Downloader downloader, int downloaders, int extractors, int perHost) {
        this.downloader = downloader;
        this.downloadPool = Executors.newFixedThreadPool(downloaders);
        this.extractPool = Executors.newFixedThreadPool(extractors);
        this.perHost = perHost;
    }

    /**
     * Entry point for the web crawler.
     * Starts the process of crawling web pages starting from the specified URL up to the specified depth.
     *
     * @param args command line arguments in the format:
     * <ul>
     * <li>args[0] - starting page URL</li>
     * <li>args[1] - crawling depth</li>
     * <li>args[2] - number of downloader threads</li>
     * <li>args[3] - number of link extractor threads</li>
     * <li>args[4] - maximum number of concurrent downloads per host</li>
     * </ul>
     */
    public static void main(String[] args) {
        String[] parsedArgs = parseArgs(args);
        if (parsedArgs == null) {
            return;
        }

        String url = parsedArgs[0];
        int depth = Integer.parseInt(parsedArgs[1]);
        int downloaders = Integer.parseInt(parsedArgs[2]);
        int extractors = Integer.parseInt(parsedArgs[3]);
        int perHost = Integer.parseInt(parsedArgs[4]);

        try (Crawler crawler =
                     new WebCrawler(new CachingDownloader(1), downloaders, extractors, perHost)) {
            crawler.download(url, depth);
        } catch (IOException e) {
            System.err.println("Error arisen: " + e.getMessage());
        }
    }

    /**
     * Downloads website up to specified depth.
     *
     * @param url start URL.
     * @param depth download depth.
     * @param excludes hosts containing one of given substrings are ignored.
     * @return download result.
     */
    @Override
    public Result download(String url, int depth, List<String> excludes) {
        Set<String> visited = ConcurrentHashMap.newKeySet();
        Set<String> downloaded = ConcurrentHashMap.newKeySet();
        ConcurrentMap<String, IOException> errors = new ConcurrentHashMap<>();
        ConcurrentMap<String, HostDownloader> hostDownloaders = new ConcurrentHashMap<>();

        Phaser phaser = new Phaser(1);
        Queue<String> currentLevel = new ConcurrentLinkedQueue<>();
        currentLevel.add(url);

        for (int currentDepth = 1; currentDepth <= depth; currentDepth++) {
            Queue<String> nextLevel = new ConcurrentLinkedQueue<>();
            for (String currentUrl : currentLevel) {
                if (visited.add(currentUrl)) {
                    try {
                        // :NOTE: URLUtils.getHost()
                        String host = URLUtils.getHost(currentUrl);
                        if (isHostNotExcluded(host, excludes)) {
                            phaser.register();
                            downloadPage(currentUrl, host, downloaded, errors, nextLevel, phaser,
                                    currentDepth < depth, excludes, hostDownloaders);
                        }
                    } catch (MalformedURLException e) {
                        errors.put(currentUrl, e);
                    }
                }
            }
            phaser.arriveAndAwaitAdvance();
            currentLevel = nextLevel;
        }

        hostDownloaders.clear();
        return new Result(new ArrayList<>(downloaded), errors);
    }

    /**
     * Downloads website up to specified depth.
     *
     * @param url start <a href="http://tools.ietf.org/html/rfc3986">URL</a>.
     * @param depth download depth.
     * @return download result.
     */
    @Override
    public Result download(String url, int depth) {
        return download(url, depth, null);
    }

    /**
     * Closes this crawler, freeing any allocated resources.
     */
    @Override
    public void close() {
        downloadPool.shutdown();
        extractPool.shutdown();
        try {
            if (!downloadPool.awaitTermination(10, TimeUnit.SECONDS)) {
                downloadPool.shutdownNow();
            }

            if (!extractPool.awaitTermination(10, TimeUnit.SECONDS)) {
                extractPool.shutdownNow();
            }
        } catch (InterruptedException e) {
            downloadPool.shutdownNow();
            extractPool.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }

    private boolean isHostNotExcluded(String host, List<String> excludes) {
        if (excludes == null || excludes.isEmpty()) {
            return true;
        }
        for (String exclude : excludes) {
            if (exclude != null && host.contains(exclude)) {
                return false;
            }
        }
        return true;
    }

    private void downloadPage(String url, String host, Set<String> downloaded,
                              Map<String, IOException> errors, Queue<String> nextLevel,
                              Phaser phaser, boolean isExtractingLinks, List<String> excludes,
                              ConcurrentMap<String, HostDownloader> hostDownloaders) {
        HostDownloader hostDownloader = hostDownloaders.computeIfAbsent(host,
                _ -> new HostDownloader(perHost));
        hostDownloader.submit(() -> {
            try {
                Document doc = downloader.download(url);
                downloaded.add(url);

                if (!isExtractingLinks) {
                    return;
                }
                phaser.register();
                extractPool.submit(() -> {
                    try {
                        List<String> links = doc.extractLinks();
                        for (String link : links) {
                            try {
                                String linkHost = URLUtils.getHost(link);
                                if (isHostNotExcluded(linkHost, excludes)) {
                                    nextLevel.add(link);
                                }
                            } catch (MalformedURLException ignored) {
                            }
                        }
                    } catch (IOException e) {
                        errors.put(url, e);
                    } finally {
                        phaser.arriveAndDeregister();
                    }
                });
            } catch (IOException e) {
                errors.put(url, e);
            } finally {
                phaser.arriveAndDeregister();
                hostDownloader.release();
            }
        });
    }

    private static String[] parseArgs(String[] args) {
        if (args == null || args.length < 1 || args.length > 5) {
            System.err.println("Usage: WebCrawler url [depth [downloads [extractors [perHost]]]]");
            return null;
        }

        String url = args[0];
        int depth = args.length > 1 ? Integer.parseInt(args[1]) : 1;
        int downloaders = args.length > 2 ? Integer.parseInt(args[2]) : 1;
        int extractors = args.length > 3 ? Integer.parseInt(args[3]) : 1;
        int perHost = args.length > 4 ? Integer.parseInt(args[4]) : 1;

        return new String[]{url, String.valueOf(depth), String.valueOf(downloaders),
                String.valueOf(extractors), String.valueOf(perHost)};
    }

    // :NOTE: acquire в основном потоке
    private class HostDownloader {
        private final Queue<Runnable> taskQueue = new ConcurrentLinkedQueue<>();
        private final AtomicInteger runningTasks = new AtomicInteger(0);
        private final int maxTasks;

        HostDownloader(int perHost) {
            this.maxTasks = perHost;
        }

        void submit(Runnable task) {
            taskQueue.add(task);
            scheduleNext();
        }

        private void scheduleNext() {
            int current = runningTasks.get();
            if (current >= maxTasks) {
                return;
            }

            Runnable nextTask = taskQueue.poll();
            if (nextTask == null) {
                return;
            }
            if (runningTasks.compareAndSet(current, current + 1)) {
                downloadPool.submit(() -> {
                    try {
                        nextTask.run();
                    } finally {
                        runningTasks.decrementAndGet();
                        scheduleNext();
                    }
                });
            } else {
                taskQueue.add(nextTask);
                scheduleNext();
            }
        }

        void release() {
            scheduleNext();
        }
    }
}