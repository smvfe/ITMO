package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.database.DatabaseUtils;
import ru.itmo.wp.model.domain.BaseEntity;
import ru.itmo.wp.model.exception.RepositoryException;

import javax.sql.DataSource;
import java.sql.*;

public abstract class BasicRepositoryImpl<T extends BaseEntity> {
    protected final DataSource dataSource = DatabaseUtils.getDataSource();

    protected void saveEntity(String insertQuery, Object[] params, T entity) {
        try (Connection connection = dataSource.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(insertQuery, Statement.RETURN_GENERATED_KEYS)) {
                for (int i = 0; i < params.length; i++) {
                    statement.setObject(i + 1, params[i]);
                }
                if (statement.executeUpdate() != 1) {
                    throw new RepositoryException("Can't save " + entity.getClass().getSimpleName());
                } else {
                    ResultSet generatedKeys = statement.getGeneratedKeys();
                    if (generatedKeys.next()) {
                        entity.setId(generatedKeys.getLong(1));
                    } else {
                        throw new RepositoryException(String.format("Can't save %s [no autogenerated fields]", entity.getClass().getSimpleName()));
                    }
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't save entity.", e);
        }
    }
}