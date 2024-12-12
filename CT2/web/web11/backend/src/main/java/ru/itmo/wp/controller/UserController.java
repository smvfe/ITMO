package ru.itmo.wp.controller;

import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.*;
import ru.itmo.wp.domain.User;
import ru.itmo.wp.exception.ValidationException;
import ru.itmo.wp.form.UserCredentials;
import ru.itmo.wp.service.UserService;

import javax.validation.Valid;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@RestController
@RequestMapping("/api")
public class UserController {

    private final UserService userService;

    public UserController(UserService userService) {
        this.userService = userService;
    }

    @GetMapping("/users")
    public List<User> getAllUsers() {
        return userService.findAll();
    }

    @PostMapping("/users")
    public User register(@RequestBody @Valid UserCredentials userCredentials, BindingResult bindingResult) {
        Pattern pattern = Pattern.compile("^[a-zA-Z ]+$");
        Matcher matcher = pattern.matcher(userCredentials.getLogin());

        if (userService.findByLoginAndPassword(userCredentials.getLogin(), userCredentials.getPassword()) != null) {
            bindingResult.reject("login.exists", "Login already exists");
        }
        if (userCredentials.getLogin().length() < 2 || userCredentials.getLogin().length() > 20) {
            bindingResult.reject("login.length", "Login length should be between 2 and 20");
        }
        if (!matcher.matches()) {
            bindingResult.reject("login.regex", "Login should contain only alphanumeric characters");
        }
        if (userCredentials.getPassword().length() < 4) {
            bindingResult.reject("password.length", "Password length should be at least 4");
        }
        if (bindingResult.hasErrors()) {
            throw new ValidationException(bindingResult);
        }
        return userService.register(userCredentials);
    }
}
