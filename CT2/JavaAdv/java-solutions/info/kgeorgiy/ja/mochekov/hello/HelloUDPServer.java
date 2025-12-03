package info.kgeorgiy.ja.mochekov.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class HelloUDPServer implements HelloServer {
    private DatagramSocket socket;
    private ExecutorService executor;
    private Thread receiverThread;
    private volatile boolean running;

    @Override
    public void start(int port, int threads) {
        if (socket != null) {
            return;
        }

        try {
            socket = new DatagramSocket(port);
            executor = Executors.newFixedThreadPool(threads);
            running = true;

            receiverThread = new Thread(() -> {
                while (!Thread.currentThread().isInterrupted() && running) {
                    try {
                        byte[] buffer = new byte[socket.getReceiveBufferSize()];
                        DatagramPacket packet = new DatagramPacket(buffer, buffer.length);
                        socket.receive(packet);

                        executor.submit(() -> processPacket(packet));
                    } catch (IOException e) {
                        if (!socket.isClosed() && running) {
                            System.err.println("Ошибка при получении пакета: " + e.getMessage());
                        }
                    }
                }
            });

            receiverThread.start();
        } catch (SocketException e) {
            System.err.println("Не удалось запустить сервер: " + e.getMessage());
        }
    }

    private void processPacket(DatagramPacket packet) {
        String request = new String(packet.getData(), 0, packet.getLength(), StandardCharsets.UTF_8);
        String response = "Hello, " + request;

        byte[] responseData = response.getBytes(StandardCharsets.UTF_8);
        DatagramPacket responsePacket = new DatagramPacket(
                responseData,
                responseData.length,
                packet.getAddress(),
                packet.getPort()
        );

        try {
            socket.send(responsePacket);
        } catch (IOException e) {
            if (!socket.isClosed() && running) {
                System.err.println("Ошибка отправки ответа: " + e.getMessage());
            }
        }
    }

    @Override
    public void close() {
        running = false;

        if (receiverThread != null) {
            receiverThread.interrupt();
            try {
                receiverThread.join();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
            receiverThread = null;
        }

        if (executor != null) {
            executor.shutdown();
            try {
                if (!executor.awaitTermination(5, TimeUnit.SECONDS)) {
                    executor.shutdownNow();
                    if (!executor.awaitTermination(5, TimeUnit.SECONDS)) {
                        System.err.println("Пул потоков не завершил работу");
                    }
                }
            } catch (InterruptedException e) {
                executor.shutdownNow();
                Thread.currentThread().interrupt();
            }
            executor = null;
        }

        if (socket != null && !socket.isClosed()) {
            socket.close();
            socket = null;
        }
    }

    public static void main(String[] args) {
        if (args == null || args.length != 2) {
            System.out.println("Использование: HelloUDPServer <порт> <потоки>");
            return;
        }

        try {
            int port = Integer.parseInt(args[0]);
            int threads = Integer.parseInt(args[1]);

            try (HelloServer server = new HelloUDPServer()) {
                server.start(port, threads);
                System.out.println("Сервер запущен на порту " + port + " с " + threads + " потоками");
                System.out.println("Нажмите Enter для остановки");
                System.in.read();
            } catch (IOException e) {
                System.err.println("Ошибка сервера: " + e.getMessage());
            }
        } catch (NumberFormatException e) {
            System.err.println("Неверный формат числа: " + e.getMessage());
        }
    }
}