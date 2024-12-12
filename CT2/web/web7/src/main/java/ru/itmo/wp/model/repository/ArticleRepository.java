package ru.itmo.wp.model.repository;

import ru.itmo.wp.model.domain.Article;

import java.util.List;

public interface ArticleRepository {
    List<Article> findAll();
    Article find(Long id);
    void save(Article article);
}
