package ru.itmo.wp.web.page;

import ru.itmo.wp.model.service.UserService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import ru.itmo.wp.model.repository.impl.EventRepositoryImpl;
import ru.itmo.wp.model.domain.Event;
import ru.itmo.wp.model.domain.User;
import java.util.Map;

@SuppressWarnings({"unused"})
public class LogoutPage extends Page {
    private final UserService userService = new UserService();

    private void action(HttpServletRequest request, Map<String, Object> view) {
        User user = (User) request.getSession().getAttribute("user");
        request.getSession().removeAttribute("user");

        Event event = new Event();
        event.setUserId(user.getId());
        event.setType(Event.EventType.LOGOUT);
        userService.addEvent(user, event);

        request.getSession().setAttribute("message", "Good bye. Hope to see you soon!");
        throw new RedirectException("/index");
    }
}
