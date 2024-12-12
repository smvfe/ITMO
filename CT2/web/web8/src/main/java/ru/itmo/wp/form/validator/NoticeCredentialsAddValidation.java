package ru.itmo.wp.form.validator;

import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import ru.itmo.wp.form.NoticeCredentials;

@Component
public class NoticeCredentialsAddValidation implements Validator {
    public boolean supports(Class<?> clazz) {
        return NoticeCredentials.class.equals(clazz);
    }

    public void validate(Object target, Errors errors) {
        NoticeCredentials noticeForm = (NoticeCredentials) target;
        if (noticeForm.getContent() == null || noticeForm.getContent().trim().isEmpty()) {
            // errors.rejectValue("content", "content.empty", "Content cannot be empty");
        } else if (noticeForm.getContent().length() > 255) {
            errors.rejectValue("content", "content.tooLong", "Content is too long");
        }
    }
}