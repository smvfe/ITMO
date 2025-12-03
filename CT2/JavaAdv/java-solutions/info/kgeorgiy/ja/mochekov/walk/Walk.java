package info.kgeorgiy.ja.mochekov.walk;

import java.io.IOException;

public class Walk extends RecursiveWalk {
    public static void main(String[] args) {
        RecursiveWalk walker = new RecursiveWalk(0);

        if (!validateArgs(args)) {
            return;
        }

        try {
            walker.travel(args[0], args[1]);
        } catch (IOException e) {
            System.err.println(String.format("Main_Error: %s",  e.getMessage()));
        }
    }
}
