#include "wordnet/Wordnet.hpp"

#include <iostream>
#include <limits>
#include <sstream>

std::vector<std::string> tokenize(const std::string& input_str, char separator) {
    std::istringstream parser(input_str);
    std::vector<std::string> segments;
    std::string segment;

    while (std::getline(parser, segment, separator)) {
        if (!segment.empty()) {
            segments.push_back(segment);
        }
    }

    return segments;
}

void Digraph::edge(int source_vertex, int target_vertex) {
    graph[source_vertex].insert(target_vertex);
}

WordNet::~WordNet() {
    delete crawler;
}

WordNet::WordNet(std::istream& synset_data, std::istream& hypernym_data) {
    std::string data_line;
    while (std::getline(hypernym_data, data_line)) {
        auto link_data = tokenize(data_line, ',');

        if (link_data.size() <= 1)
            continue;

        int synset_id = std::stoi(link_data[0]);
        for (size_t i = 1; i < link_data.size(); ++i) {
            hypernyms.edge(synset_id, std::stoi(link_data[i]));
        }
    }

    while (std::getline(synset_data, data_line)) {
        auto comma_pos1 = data_line.find(',');
        if (comma_pos1 == std::string::npos)
            continue;

        auto node_id    = std::stoi(data_line.substr(0, comma_pos1));
        auto comma_pos2 = data_line.find(',', comma_pos1 + 1);

        if (comma_pos2 != std::string::npos) {
            glossary[node_id] = data_line.substr(comma_pos2 + 1);

            auto terms_section = data_line.substr(comma_pos1 + 1, comma_pos2 - comma_pos1 - 1);
            for (const auto& term : tokenize(terms_section, ' ')) {
                words[term].insert(node_id);
            }
        }
    }

    crawler = new ShortestCommonAncestor(hypernyms);
}

bool WordNet::is_noun(const std::string& term) const {
    return words.contains(term);
}

WordNet::Nouns WordNet::nouns() const {
    return Nouns(words);
}

std::string WordNet::sca(const std::string& term1, const std::string& term2) const {
    auto common_anc_id = crawler->ancestor_subset(words.at(term1), words.at(term2));
    return glossary.at(common_anc_id);
}

unsigned WordNet::distance(const std::string& term1, const std::string& term2) const {
    return crawler->length_subset(words.at(term1), words.at(term2));
}

WordNet::Nouns::iterator::iterator()                      = default;
WordNet::Nouns::iterator::iterator(iterator& other)       = default;
WordNet::Nouns::iterator::iterator(const iterator& other) = default;

WordNet::Nouns::iterator::iterator(std::unordered_map<std::string, std::set<unsigned>> const& word_map, bool at_end) {
    this->it = at_end ? word_map.end() : word_map.begin();
}

WordNet::Nouns::iterator& WordNet::Nouns::iterator::operator=(const iterator& other) {
    this->it = other.it;
    return *this;
}

WordNet::Nouns::iterator& WordNet::Nouns::iterator::operator++() {
    ++it;
    return *this;
}

WordNet::Nouns::iterator WordNet::Nouns::iterator::operator++(int) {
    auto temp = *this;
    ++it;
    return temp;
}

bool WordNet::Nouns::iterator::operator==(const iterator& other) const {
    return it == other.it;
}

bool WordNet::Nouns::iterator::operator!=(const iterator& other) const {
    return it != other.it;
}

WordNet::Nouns::iterator::value_type& WordNet::Nouns::iterator::operator*() {
    return const_cast<value_type&>(it->first);
}

WordNet::Nouns::Nouns(std::unordered_map<std::string, std::set<unsigned>> const& word_map) : words(word_map) {}

WordNet::Nouns::iterator WordNet::Nouns::begin() const {
    return iterator(words, false);
}

WordNet::Nouns::iterator WordNet::Nouns::end() const {
    return iterator(words, true);
}

ShortestCommonAncestor::ShortestCommonAncestor(Digraph& graph) : dgraph(graph) {}

std::unordered_map<unsigned int, int> ShortestCommonAncestor::ancestor_set(unsigned int start_vertex) {
    std::unordered_map<unsigned int, int> dist_map;
    dist_map[start_vertex] = 0;

    std::unordered_set<unsigned int> curr_level{start_vertex};
    std::unordered_set<unsigned int> next_level;
    int depth = 1;

    while (!curr_level.empty()) {
        for (auto vertex : curr_level) {
            for (auto adj_vertex : dgraph.graph[vertex]) {
                if (dist_map.find(adj_vertex) == dist_map.end()) {
                    next_level.insert(adj_vertex);
                    dist_map[adj_vertex] = depth;
                }
            }
        }

        curr_level = std::move(next_level);
        next_level.clear();
        ++depth;
    }

    return dist_map;
}

std::pair<int, int> ShortestCommonAncestor::ancestor_info(std::unordered_map<unsigned int, int> src_distances,
                                                          unsigned int dest_vertex) {
    int best_dist = std::numeric_limits<int>::max();
    int best_node = -1;

    std::unordered_set curr_level{dest_vertex};
    std::unordered_set<unsigned int> next_level;
    int depth = 0;

    while (!curr_level.empty() && depth < best_dist) {
        for (auto vertex : curr_level) {
            if (src_distances.count(vertex) > 0) {
                int path_len = src_distances[vertex] + depth;
                if (path_len < best_dist) {
                    best_dist = path_len;
                    best_node = vertex;
                }
            }

            for (auto adj_vertex : dgraph.graph[vertex]) {
                next_level.insert(adj_vertex);
            }
        }

        curr_level = std::move(next_level);
        next_level.clear();
        ++depth;
    }

    return {best_node, best_dist};
}

std::pair<int, int> ShortestCommonAncestor::subset_sercher(const std::set<unsigned int>& set_a,
                                                           const std::set<unsigned int>& set_b) {
    int result_node   = 0;
    int shortest_path = std::numeric_limits<int>::max();

    for (auto vertex_a : set_a) {
        auto ancestor_map = ancestor_set(vertex_a);
        for (auto vertex_b : set_b) {
            auto result = ancestor_info(ancestor_map, vertex_b);
            if (result.second < shortest_path) {
                shortest_path = result.second;
                result_node   = result.first;
            }
        }
    }

    return {result_node, shortest_path};
}

unsigned ShortestCommonAncestor::length(unsigned int src, unsigned int dst) {
    return ancestor_info(ancestor_set(src), dst).second;
}

unsigned ShortestCommonAncestor::ancestor(unsigned int src, unsigned int dst) {
    return ancestor_info(ancestor_set(src), dst).first;
}

unsigned ShortestCommonAncestor::length_subset(const std::set<unsigned int>& set_a,
                                               const std::set<unsigned int>& set_b) {
    return subset_sercher(set_a, set_b).second;
}

unsigned ShortestCommonAncestor::ancestor_subset(const std::set<unsigned int>& set_a,
                                                 const std::set<unsigned int>& set_b) {
    return subset_sercher(set_a, set_b).first;
}

Outcast::Outcast(WordNet& wordnet) : wnet(wordnet) {}

std::string Outcast::outcast(const std::set<std::string>& nouns) {
    std::unordered_map<std::string, int> calculations;
    for (auto noun : nouns) {
        calculations[noun] = 0;
    }
    for (auto first : nouns) {
        for (auto second : nouns) {
            if (first != second) {
                int len = wnet.distance(first, second);
                calculations[first] += len;
                calculations[second] += len;
            }
        }
    }

    std::string ans = "";
    int max_len     = 0;
    for (auto p : calculations) {
        if (p.second > max_len) {
            max_len = p.second;
            ans     = p.first;
        } else if (p.second == max_len) {
            ans = "";
        }
    }
    return ans;
}