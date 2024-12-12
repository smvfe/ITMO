package ru.itmo.wp.model.repository;

import ru.itmo.wp.model.domain.User;

import java.util.List;

public interface UserRepository {
    User find(long id);

    User findByLogin(String login);

    // mod 2
    User findByEmail(String email);
    // mod 3
    User findByLoginAndPasswordSha(String loginOrEmail, String passwordSha);

    List<User> findAll();
    // mod 4
    long findCount();

    void save(User user, String passwordSha);
}
