package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.database.DatabaseUtils;
import ru.itmo.wp.model.domain.Event;
import ru.itmo.wp.model.repository.EventRepository;

import javax.sql.DataSource;

public class EventRepositoryImpl extends BasicRepositoryImpl<Event> implements EventRepository {
    private final DataSource DATA_SOURCE = DatabaseUtils.getDataSource();
    private final String EVENT_INSERT = "INSERT INTO Event (userId, type, creationTime) VALUES (?, ?, NOW())";

    public void save(Event event) {
        Object[] params = {event.getUserId(), event.getType().name()};
        saveEntity(EVENT_INSERT, params, event);
    }
}