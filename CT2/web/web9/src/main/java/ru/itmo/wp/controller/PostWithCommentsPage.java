package ru.itmo.wp.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.*;
import ru.itmo.wp.domain.Comment;
import ru.itmo.wp.domain.Post;
import ru.itmo.wp.security.Guest;
import ru.itmo.wp.service.PostService;
import ru.itmo.wp.service.UserService;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
public class PostWithCommentsPage extends Page {
    private final PostService postService;

    public PostWithCommentsPage(PostService postService) {
        this.postService = postService;
    }

    @Guest
    @GetMapping("/post/{id}")
    public String viewPost(@PathVariable("id") String id, Model model) {
        long id_long;
        try {
            id_long = Long.parseLong(id);
        } catch (NumberFormatException e) {
            return "redirect:/";
        }
        Post post = postService.findById(id_long);
        if (post == null) {
            return "PostWithCommentsPageq";
        }
        model.addAttribute("post", post);
        model.addAttribute("comments", post.getComments());
        model.addAttribute("newComment", new Comment());
        return "PostWithCommentsPage";
    }

    @PostMapping("/post/{id}")
    public String addComment(@PathVariable("id") long id,
                             @Valid @ModelAttribute("newComment") Comment newComment,
                             BindingResult bindingResult,
                             HttpSession httpSession) {
        if (bindingResult.hasErrors()) {
            return "redirect:/post/" + id;
        }

        Post post = postService.findById(id);
        if (post == null) {
            return "redirect:/";
        }

        postService.writeComment(getUser(httpSession), post, newComment);
        putMessage(httpSession, "Your comment has been added");

        return "redirect:/post/" + id;
    }
}