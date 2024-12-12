package ru.itmo.wp.servlet;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

public class StaticServlet extends HttpServlet {
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String uri = request.getRequestURI();

        // сплит +
        String[] paths = uri.split("\\+");
        List<File> files = new ArrayList<>();

        // файлы и их существование
        for (String path : paths) {
            File file = new File(getServletContext().getRealPath("/static/" + path));
            if (!file.isFile()) {
                response.sendError(HttpServletResponse.SC_NOT_FOUND);
                return; // елси чего, что должно быть нет, то оно не позволит отобразить всё как хотят
            }
            files.add(file);
        }

        // Определяем MIME-тип по первому файлу
        String mimeType = getServletContext().getMimeType(files.get(0).getName());
        response.setContentType(mimeType);

        // Записываем содержимое всех файлов в ответ
        try (OutputStream outputStream = response.getOutputStream()) {
            for (File file : files) {
                Files.copy(file.toPath(), outputStream);
            }
        }
    }
}
