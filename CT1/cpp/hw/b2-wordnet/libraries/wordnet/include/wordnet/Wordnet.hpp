#ifndef WORDNET_WORDNET_HPP
#define WORDNET_WORDNET_HPP

#include <iosfwd>
#include <queue>
#include <set>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

class ShortestCommonAncestor;
class Digraph {
public:
    void edge(int from, int to);
    std::unordered_map<int, std::set<int>> graph;
};

class WordNet {
private:
    std::unordered_map<std::string, std::set<unsigned>> words;
    std::unordered_map<int, std::string> glossary;
    ShortestCommonAncestor* crawler;
    Digraph hypernyms;

public:
    WordNet(std::istream& synsets, std::istream& hypernyms);

    ~WordNet();

    /**
     * Simple proxy class used to enumerate nouns.
     *
     * Usage example:
     *
     * WordNet wordnet{...};
     * ...
     * for (const std::string & noun : wordnet.nouns()) {
     *     // ...
     * }
     */
    class Nouns {
    public:
        class iterator {
        public:
            using iterator_category = std::forward_iterator_tag;
            using value_type        = std::string;
            using difference_type   = std::ptrdiff_t;
            using pointer           = value_type*;
            using reference         = value_type&;
            iterator();
            iterator(iterator& other);
            iterator(const iterator& other);
            iterator(std::unordered_map<std::string, std::set<unsigned>> const&, bool isEnd);

            iterator& operator=(const iterator& other);
            bool operator==(const iterator& other) const;
            bool operator!=(const iterator& other) const;
            iterator& operator++();
            iterator operator++(int);
            reference operator*();

        private:
            std::unordered_map<std::string, std::set<unsigned>>::const_iterator it;
        };

        explicit Nouns(std::unordered_map<std::string, std::set<unsigned>> const& words);

        iterator begin() const;
        iterator end() const;
        std::unordered_map<std::string, std::set<unsigned>> const& words;
    };

    // lists all nouns stored in WordNet
    Nouns nouns() const;

    // returns 'true' iff 'word' is stored in WordNet
    bool is_noun(const std::string& word) const;

    // returns gloss of "shortest common ancestor" of noun1 and noun2
    std::string sca(const std::string& noun1, const std::string& noun2) const;

    // calculates distance between noun1 and noun2
    unsigned distance(const std::string& noun1, const std::string& noun2) const;
};

class Outcast {
private:
    WordNet& wnet;

public:
    explicit Outcast(WordNet& wordnet);

    // returns outcast word
    std::string outcast(const std::set<std::string>& nouns);
};

class ShortestCommonAncestor {
    Digraph& dgraph;
    friend WordNet;
    explicit ShortestCommonAncestor(Digraph& dgraph);

    // calculates length of shortest common ancestor path from node with id 'v' to
    // node with id 'w'
    unsigned length(unsigned v, unsigned w);

    // returns node id of shortest common ancestor of nodes v and w
    unsigned ancestor(unsigned v, unsigned w);

    // calculates length of shortest common ancestor path from node subset
    // 'subset_a' to node subset 'subset_b'
    unsigned length_subset(const std::set<unsigned>& subset_a, const std::set<unsigned>& subset_b);

    // returns node id of shortest common ancestor of node subset 'subset_a' and
    // node subset 'subset_b'
    unsigned ancestor_subset(const std::set<unsigned>& subset_a, const std::set<unsigned>& subset_b);

    std::unordered_map<unsigned int, int> ancestor_set(unsigned v);

    std::pair<int, int> ancestor_info(std::unordered_map<unsigned int, int> first_path, unsigned int second);

    std::pair<int, int> subset_sercher(const std::set<unsigned>& subset_a, const std::set<unsigned>& subset_b);
};

#endif  // WORDNET_WORDNET_HPP
