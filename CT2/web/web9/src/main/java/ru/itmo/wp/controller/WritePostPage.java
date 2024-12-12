package ru.itmo.wp.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import ru.itmo.wp.domain.Post;
import ru.itmo.wp.domain.Role;
import ru.itmo.wp.security.AnyRole;
import ru.itmo.wp.service.PostService;
import ru.itmo.wp.service.UserService;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;
import java.util.*;
import java.util.regex.Pattern;

@Controller
public class WritePostPage extends Page {
    private final PostService postService;
    private final UserService userService;

    public WritePostPage(PostService postService, UserService userService) {
        this.postService = postService;
        this.userService = userService;
    }

    @AnyRole({Role.Name.WRITER, Role.Name.ADMIN})
    @GetMapping("/writePost")
    public String writePostGet(Model model) {
        model.addAttribute("post", new Post());
        return "WritePostPage";
    }

    @AnyRole({Role.Name.WRITER, Role.Name.ADMIN})
    @PostMapping("/writePost")
    public String writePostPost(@RequestParam("tagArray") String tagArray,
                                @Valid @ModelAttribute("post") Post post,
                                BindingResult bindingResult,
                                HttpSession httpSession) {
        if (bindingResult.hasErrors()) {
            return "WritePostPage";
        }
        
        List<String> tagNames = new ArrayList<>(Arrays.asList(tagArray.split("\\s+")));
        Pattern pattern = Pattern.compile("^[a-zA-Z]+$");
        if (!tagArray.trim().isEmpty()) {
            for (String tagName : tagNames) {
                if (!pattern.matcher(tagName).matches()) {
                    bindingResult.rejectValue("tags", "tagArray", "Tags must be simple words with Latin letters only.");
                    putMessage(httpSession, "Uncorrect tags");
                    return "WritePostPage";
                }
            }
        }

        postService.saveTags(post, tagNames);
        userService.writePost(getUser(httpSession), post);
        putMessage(httpSession, "You published new post");

        return "redirect:/myPosts";
    }
}