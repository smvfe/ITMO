package ru.itmo.wp.form;

import javax.validation.constraints.*;

public class NoticeCredentials {
    @NotNull
    @NotBlank
    @Size(max = 255)
    private String content;

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }
}