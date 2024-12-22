package ru.itmo.wp.servlet;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;

public class StaticServlet extends HttpServlet {
    // дефолт путь
    private static final String SRC_STATIC_PATH = "src/main/webapp/static";

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String uri = request.getRequestURI();

        // ищем в src
        File srcFile = new File(SRC_STATIC_PATH, uri);
        if (srcFile.isFile()) {
            serveFile(response, srcFile);
            return;
        }

        // иначе идём в стандарт
        File realPathFile = new File(getServletContext().getRealPath("/static" + uri));
        if (realPathFile.isFile()) {
            serveFile(response, realPathFile);
        } else {
            response.sendError(HttpServletResponse.SC_NOT_FOUND);
        }
    }

    private void serveFile(HttpServletResponse response, File file) throws IOException {
        response.setContentType(getServletContext().getMimeType(file.getName()));
        try (OutputStream outputStream = response.getOutputStream()) {
            Files.copy(file.toPath(), outputStream);
        }
    }
}
