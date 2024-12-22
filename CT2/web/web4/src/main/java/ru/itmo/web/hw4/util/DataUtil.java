package ru.itmo.web.hw4.util;

import ru.itmo.web.hw4.model.Color;
import ru.itmo.web.hw4.model.Post;
import ru.itmo.web.hw4.model.User;

import javax.servlet.http.HttpServletRequest;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

public class DataUtil {
    private static final List<User> USERS = Arrays.asList(
            new User(1, "MikeMirzayanov", "Mike Mirzayanov", Color.GREEN),
            new User(6, "pashka", "Pavel Mavrin", Color.BLUE),
            new User(9, "geranazavr555", "Georgiy Nazarov", Color.GREEN),
            new User(11, "tourist", "Gennady Korotkevich", Color.RED)
    );

    private static final List<Post> POSTS = Arrays.asList(
            new Post(1, "Codeforces is here!", "Today we celebrate start of the new era of programming!", 1),
            new Post(2, "JetBrains is now official sponsor of Codeforces", "Today we announce that JetBrains is now in partnership with Codeforces! Let's make our industry better and better.", 6),
            new Post(3, "OMG NOW I AM NUMBER ONE", "Just reached the top of the list on CF! Going after another medal of ICPC!", 11),
            new Post(4, "Codeforces is now closed...", "Just temporary. Codeforces, the popular competitive programming platform, is currently undergoing a major refactoring. The goal is to improve code maintainability, enhance performance, and introduce new features more efficiently. This initiative aims to streamline the platform's architecture, making it more robust and scalable. Users can expect a smoother experience and faster response times as the refactoring progresses. ", 9)
    );

    public static void addData(HttpServletRequest request, Map<String, Object> data) {
        data.put("users", USERS);
        data.put("posts", POSTS);

        for (User user : USERS) {
            if (Long.toString(user.getId()).equals(request.getParameter("logged_user_id"))) {
                data.put("user", user);
            }
        }

        String currentPage = request.getRequestURI();
        data.put("currentPage", currentPage); // no.2
    }

}
