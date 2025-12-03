package info.kgeorgiy.ja.mochekov.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class HelloUDPClient implements HelloClient {
    private static final int SOCKET_TIMEOUT = 200; // мс

    @Override
    public void run(String host, int port, String prefix, int requests, int threads) {
        InetAddress address;
        try {
            address = InetAddress.getByName(host);
        } catch (UnknownHostException e) {
            System.err.println("Не удалось разрешить хост: " + e.getMessage());
            return;
        }

        ExecutorService executor = Executors.newFixedThreadPool(threads);

        for (int i = 0; i < threads; i++) {
            final int threadId = i;
            executor.submit(() -> {
                try (DatagramSocket socket = new DatagramSocket()) {
                    socket.setSoTimeout(SOCKET_TIMEOUT);

                    byte[] receiveBuffer = new byte[socket.getReceiveBufferSize()];
                    DatagramPacket receivePacket = new DatagramPacket(receiveBuffer, receiveBuffer.length);

                    for (int requestId = 0; requestId < requests; requestId++) {
                        String request = prefix + requestId + "_" + threadId;
                        sendRequest(socket, request, address, port, receivePacket);
                    }
                } catch (SocketException e) {
                    System.err.println("Ошибка сокета: " + e.getMessage());
                }
            });
        }

        shutdownAndAwaitTermination(executor);
    }

    private void sendRequest(DatagramSocket socket, String request, InetAddress address, int port,
                             DatagramPacket receivePacket) {
        byte[] sendBuffer = request.getBytes(StandardCharsets.UTF_8);
        DatagramPacket sendPacket = new DatagramPacket(sendBuffer, sendBuffer.length, address, port);

        boolean received = false;
        while (!Thread.currentThread().isInterrupted() && !received) {
            try {
                System.out.println("Отправка: " + request);
                socket.send(sendPacket);
                socket.receive(receivePacket);

                String response = new String(receivePacket.getData(), 0, receivePacket.getLength(), StandardCharsets.UTF_8);
                received = response.contains(request);

                if (received) {
                    System.out.println("Получено: " + response);
                }
            } catch (IOException e) {
                System.err.println("Ошибка при отправке/получении " + request + ": " + e.getMessage());
            }
        }
    }

    private void shutdownAndAwaitTermination(ExecutorService executor) {
        executor.shutdown();
        try {
            if (!executor.awaitTermination(60, TimeUnit.SECONDS)) {
                executor.shutdownNow();
                if (!executor.awaitTermination(60, TimeUnit.SECONDS)) {
                    System.err.println("Пул потоков не завершил работу");
                }
            }
        } catch (InterruptedException ie) {
            executor.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }

    public static void main(String[] args) {
        if (args == null || args.length != 5) {
            System.out.println("Использование: HelloUDPClient <хост> <порт> <префикс> <запросов> <потоков>");
            return;
        }

        try {
            String host = args[0];
            int port = Integer.parseInt(args[1]);
            String prefix = args[2];
            int requests = Integer.parseInt(args[3]);
            int threads = Integer.parseInt(args[4]);

            new HelloUDPClient().run(host, port, prefix, requests, threads);
        } catch (NumberFormatException e) {
            System.err.println("Неверный формат числа: " + e.getMessage());
        }
    }
}