package ru.itmo.wp.servlet;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.IOException;

public class CaptchaServlet extends HttpServlet {
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        HttpSession session = request.getSession();
        int expectedValue = (Integer) session.getAttribute("captchaValue");
        String captchaResponse = request.getParameter("captchaResponse");

        // Проверка ответа пользователя
        if (captchaResponse != null && Integer.parseInt(captchaResponse) == expectedValue) {
            // Если ответ верный
            session.setAttribute("captchaPassed", true);
            response.sendRedirect(request.getContextPath()); // Перенаправление на исходную страницу
        } else {
            // Если ответ неверный, повторно показываем CAPTCHA
            session.removeAttribute("captchaPassed"); // Удаляем, чтобы заново вызвать каптчу
            response.sendRedirect(request.getRequestURI()); // Повторный запрос к текущему URI
        }
    }
}

