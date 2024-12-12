package ru.itmo.wp.model.repository;

import ru.itmo.wp.model.database.DatabaseUtils;
import ru.itmo.wp.model.domain.Event;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.RepositoryException;
import ru.itmo.wp.model.repository.EventRepository;
import ru.itmo.wp.model.repository.UserRepository;

import javax.sql.DataSource;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public interface BasicRepository {
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

    void save(Event event);

}
