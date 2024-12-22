package ru.itmo.wp.servlet;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.IOException;
import java.util.Random;

public class CaptchaFilter implements Filter {

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {
        // Инициализация фильтра (при необходимости)
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
            throws IOException, ServletException {
        HttpServletRequest httpRequest = (HttpServletRequest) request;
        HttpServletResponse httpResponse = (HttpServletResponse) response;
        HttpSession session = httpRequest.getSession();

        // Проверка, прошла ли сессия каптчу
        Boolean captchaPassed = (Boolean) session.getAttribute("captchaPassed");

        if (captchaPassed == null || !captchaPassed) {
            // Генерация случайного числа от 100 до 999
            Random random = new Random();
            int captchaValue = random.nextInt(900) + 100;
            session.setAttribute("captchaValue", captchaValue);

            // Отправка CAPTCHA формы пользователю
            httpResponse.setContentType("text/html");
            httpResponse.getWriter().println("<html><body>");
            httpResponse.getWriter().println("<h3>Введите число для проверки:</h3>");
            httpResponse.getWriter().println("<img src='data:image/png;base64," + ImageUtils.generateCaptchaImage(captchaValue) + "'/>");
            httpResponse.getWriter().println("<form method='post' action='captcha'>");
            httpResponse.getWriter().println("<input type='text' name='captchaResponse'/>");
            httpResponse.getWriter().println("<input type='submit' value='Отправить'/>");
            httpResponse.getWriter().println("</form>");
            httpResponse.getWriter().println("</body></html>");
            return; // Прерываем дальнейшую обработку запроса
        }

        // Если каптча пройдена, продолжаем обработку запроса
        chain.doFilter(request, response);
    }

    @Override
    public void destroy() {
        // Освобождение ресурсов (если нужно)
    }
}


