package ru.itmo.wp.model.domain;

import java.io.Serializable;
import java.util.Date;

public class User implements BaseEntity {
    private Long id;
    private String login;
    private Date creationTime;
    private String email;

    @Override
    public Long getId() {
        return id;
    }

    @Override
    public void setId(Long id) {
        this.id = id;
    }

    // mod 2
    public String getEmail() {
        return email;
    }
    // mod 2
    public void setEmail(String email) {
        this.email = email;
    }

    public String getLogin() {
        return login;
    }

    public void setLogin(String login) {
        this.login = login;
    }

    public Date getCreationTime() {
        return creationTime;
    }

    public void setCreationTime(Date creationTime) {
        this.creationTime = creationTime;
    }
}
