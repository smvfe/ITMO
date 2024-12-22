package ru.itmo.wp.servlet;

import com.google.gson.Gson;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MessageServlet extends HttpServlet {
    private final List<Map<String, String>> messages = new ArrayList<>();

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        response.setContentType("application/json");

        String uri = request.getRequestURI();
        response.setCharacterEncoding("UTF-8");
        
        switch (uri) {
            case "/message/auth":
                toAuth(request, response);
                return;
            case "/message/findAll":
                toFindAll(response);
                return;
            case "/message/add":
                toAdd(request, response);
                return;
            default:
                response.sendError(HttpServletResponse.SC_NOT_FOUND, "Unknown request: " + uri);
        }
    }

    // аутентификация
    private void toAuth(HttpServletRequest request, HttpServletResponse response) throws IOException {
        HttpSession session = request.getSession();
        String user = request.getParameter("user");

        if (user != null && !user.trim().isEmpty()) {
            session.setAttribute("user", user);
        }

        String currentUser = (String) session.getAttribute("user");
        if (currentUser == null) {
            currentUser = "";
        }

        String json = new Gson().toJson(currentUser);
        response.getWriter().print(json);
        response.getWriter().flush();
    }

    // получение всех сообщений
    private void toFindAll(HttpServletResponse response) throws IOException {
        if (messages.isEmpty()) {
            response.getWriter().print("[]");
            response.getWriter().flush();
            return;
        }

        String json = new Gson().toJson(messages);
        response.getWriter().print(json);
        response.getWriter().flush();
    }

    // добавление сообщения
    private void toAdd(HttpServletRequest request, HttpServletResponse response) throws IOException {
        HttpSession session = request.getSession();
        String user = (String) session.getAttribute("user");

        if (user == null || user.trim().isEmpty()) {
            response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "User is not authenticated");
            return;
        }

        String text = request.getParameter("text");
        if (text == null || text.trim().isEmpty()) {
            response.sendError(HttpServletResponse.SC_BAD_REQUEST, "Message text is required");
            return;
        }

        Map<String, String> message = new HashMap<>();
        message.put("user", user);
        message.put("text", text);
        messages.add(message);

        String json = new Gson().toJson("{}");
        response.getWriter().print(json);
        response.getWriter().flush();
    }
}
