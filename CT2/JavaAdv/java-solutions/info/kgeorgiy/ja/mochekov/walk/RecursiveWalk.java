package info.kgeorgiy.ja.mochekov.walk;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitOption;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.Objects;

public class RecursiveWalk extends SimpleFileVisitor<Path> {
    private final MessageDigest digest;
    private final Charset using_charset;
    private BufferedWriter writer;
    private Integer RecDepth;

    private static final int BUFFER_SIZE = 1 << 16;
    private static final int HASH_BYTE_SIZE = 8;

    public RecursiveWalk(MessageDigest digest, Charset charset, Integer depth) {
        this.digest = Objects.requireNonNull(digest, "MessageDigest cannot be null");
        this.using_charset = Objects.requireNonNull(charset, "Charset cannot be null");
        this.RecDepth = depth;
    }

    public RecursiveWalk(MessageDigest digest, Integer depth) {
        this(digest, StandardCharsets.UTF_8, depth);
    }

    public RecursiveWalk(Charset charset, Integer depth) {
        this(getDefaultDigest(), charset, depth);
    }

    public RecursiveWalk(Integer depth) {
        this(getDefaultDigest(), StandardCharsets.UTF_8, depth);
    }

    public RecursiveWalk() {
        this(getDefaultDigest(), StandardCharsets.UTF_8, Integer.MAX_VALUE);
    }

    public static void main(String[] args) {
        RecursiveWalk walker = new RecursiveWalk();

        if (!validateArgs(args)) {
            return;
        }

        try {
            walker.travel(args[0], args[1]);
        } catch (IOException e) {
            System.err.println(String.format("Main_Error: %s",  e.getMessage()));
        }

    }

    @Override
    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
        writing(file.toString());
        return FileVisitResult.CONTINUE;
    }

    public void travel(String inputSource, String outputSource) throws IOException {
        Path inputFile, outputFile;
        try {
            inputFile = Path.of(inputSource);
            outputFile = Path.of(outputSource);

            Path parent = outputFile.getParent();
            if (parent != null) {
                try {
                    Files.createDirectories(parent);
                } catch (IOException e) {
                    System.err.println(String.format("Travel_Cannot_create_directories: %s", e.getMessage()));
                    return;
                } catch (SecurityException e) {
                    System.err.println(String.format("Travel_Security_Error: %s", e.getMessage()));
                    return;
                }   
            }
            
        } catch (InvalidPathException e) {
            System.err.println(String.format("Travel_Invalid_ path_Error: %s",e.getMessage()));
            return;
        }

        try (
            BufferedReader input = Files.newBufferedReader(inputFile, using_charset);
            BufferedWriter output = Files.newBufferedWriter(outputFile, using_charset);
        ) {
            this.writer = output;

            String line;
            while ((line = input.readLine()) != null) {
                try {
                    Path visitingPaths = Paths.get(line);
                    Files.walkFileTree(visitingPaths, EnumSet.noneOf(FileVisitOption.class),  RecDepth, this);
                } catch (InvalidPathException | IOException e) {
                    writing(line);
                } catch (IllegalArgumentException e) {
                    System.err.println(String.format("Travel_Illegal_Argument_Error: %s", e.getMessage()));
                    return;
                }
            }

        } catch (IOException e) {
            System.err.println(String.format("Travel_IO_Error: %s",  e.getMessage()));
        } catch (SecurityException e) {
            System.err.println(String.format("Travel_Security_Error: %s",  e.getMessage()));
        }

    }

    private void writing(String line) {
        try {
            String hash = getFileHash(line);
            writer.write(String.format("%s %s", hash, line) + System.lineSeparator());
        } catch (IOException e) {
            System.err.println(String.format("Writing_Error: %s", e.getMessage()));
        }
    }

    private String getFileHash(String path) {
        try {
            Path file;
            try {
                file = Paths.get(path);
            } catch (InvalidPathException e) {
                return getNullHash();
            }

            if (!Files.isRegularFile(file)) {
                return getNullHash();
            }

            byte[] buffer = new byte[BUFFER_SIZE];
            try (InputStream inputStream = new BufferedInputStream(Files.newInputStream(file))) {
                int bytesRead;
                while ((bytesRead = inputStream.read(buffer)) != -1) {
                    digest.update(buffer, 0, bytesRead);
                }
            } catch (SecurityException | IOException e) {
                return getNullHash();
            }

            byte[] hashBytes = digest.digest();
            byte[] lastBytes = Arrays.copyOfRange(hashBytes, 0, HASH_BYTE_SIZE);

            StringBuilder hexHash = new StringBuilder();
            for (byte b : lastBytes) {
                hexHash.append(String.format("%02x", b));
            }

            return hexHash.toString();

        } finally {
            digest.reset();
        }
    }

    private static String getNullHash() {
        return "0".repeat(HASH_BYTE_SIZE << 1);
    }

    protected static boolean validateArgs(String[] args) {
        if (args == null || args.length != 2 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("Usage: java RecursiveWalk <input file> <output file>");
            return false;
        }
        return true;
    }

    private static MessageDigest getDefaultDigest() {
        try {
            return MessageDigest.getInstance("SHA-256");
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException("SHA-256 algorithm not available", e);
        }
    }

}