#pragma once
#include <memory>
#include <stdexcept>
#include <cassert>
#include <string>
#include <sstream>
#include <iterator>
#include <vector>
#include <utility>

namespace details
{

template<class KEY, class VALUE>
struct node
{
    node(const KEY &key, const VALUE &val)
        : parent_(nullptr)
        , left_(nullptr)
        , right_(nullptr)
        , key_(key)
        , value_(val)
    {}

    ~node() noexcept
    {}

    node(node &&src) = default;
    node& operator=(node &&src) = default;

    node(const node &src) = delete;
    node& operator=(const node &src) = delete;

    void putLeft(std::unique_ptr<node> &&nptr) noexcept
    {
        left_ = std::move(nptr);
    }

    std::unique_ptr<node> takeLeft() noexcept
    {
        return std::move(left_);
    }

    void putRight(std::unique_ptr<node> &&nptr) noexcept
    {
        right_ = std::move(nptr);
    }

    std::unique_ptr<node> takeRight() noexcept
    {
        return std::move(right_);
    }

    node*& parent() noexcept
    {
        return parent_;
    }

    node* left() noexcept
    {
        return left_.get();
    }

    node* right() noexcept
    {
        return right_.get();
    }

    KEY& key() noexcept
    {
        return key_;
    }

    VALUE &value() noexcept
    {
        return value_;
    }

private:
    node* parent_;
    std::unique_ptr<node> left_;
    std::unique_ptr<node> right_;
    KEY key_;
    VALUE value_;
};

} // namespace details

// TODO bst_key_exists_exception: std::logic_error
// TODO bst_key_absent_exception: std::logic_error
struct bst_key_exists_exception: std::logic_error {
    bst_key_exists_exception(): std::logic_error("ERROR: Key exists") {}
};

struct bst_key_absent_exception: std::logic_error {
    bst_key_absent_exception(): std::logic_error("ERROR: Key absent") {}
};




// KEY should have operator <, <<, () constructor
template<class KEY, class VALUE>
struct bin_search_tree
{
    bin_search_tree() = default;

    // throws bst_key_exists_exception
    void insert(const KEY &key, const VALUE &val);

    // throws bst_key_exists_exception
    template<class KIT, class VIT>
    void insert(KIT kbegin, KIT kend, VIT vbegin);

    bool contains(KEY key) noexcept;

    // throws bst_key_absent_exception
    VALUE& find(const KEY &key);
private:
    typedef details::node<KEY, VALUE> node;
    node* find_nearest_node(const KEY &key);
    void reinsert_node(std::unique_ptr<node> &&node_ptr) noexcept;
    void erase_node(node *node) noexcept;
    static bool should_go_right(node *parent, const KEY &key);

    std::unique_ptr<node> root_ptr_;
};

template<class KEY, class VALUE>
auto bin_search_tree<KEY, VALUE>::find_nearest_node(const KEY &key) -> node*
{
    if (!root_ptr_)
        return nullptr;

    node *cur_node = root_ptr_.get();
    for(;;)
    {
        if (cur_node->key() == key)
            break;

        node *next_child = should_go_right(cur_node, key)
            ? cur_node->right()
            : cur_node->left();

        if (!next_child)
            break;
        cur_node = next_child;
    }
    assert(cur_node);
    return cur_node;
}

template<class KEY, class VALUE>
bool bin_search_tree<KEY, VALUE>::should_go_right(node *parent, const KEY &key) {
    return key > parent->key();
};

template<class KEY, class VALUE>
void bin_search_tree<KEY, VALUE>::erase_node(node *enode) noexcept
{
    node *parent = enode->parent();
    std::unique_ptr<node> left_ptr = enode->takeLeft();
    std::unique_ptr<node> right_ptr = enode->takeRight();

    if (parent)
    {
        if (should_go_right(parent, enode->key()))
        {
            parent->putRight(nullptr);
        }
        else
        {
            parent->putLeft(nullptr);
        }
    }
    else
    {
        root_ptr_ = nullptr;
    }
    reinsert_node(std::move(left_ptr));
    reinsert_node(std::move(right_ptr));
}


template<class KEY, class VALUE>
void bin_search_tree<KEY, VALUE>::insert(const KEY &key, const VALUE &val) {
    // Must create nodes before inserting
    // If exception occured we need to erase nodes, which already inserted
}

template<class KEY, class VALUE>
template<class KIT, class VIT>
void bin_search_tree<KEY, VALUE>::insert(KIT kbegin, KIT kend, VIT vbegin) {
    for (auto cur = kbegin, val = vbegin; cur != kend; ++cur, ++val) {
        insert(*cur, *val);
    }
}

template<class KEY, class VALUE>
bool bin_search_tree<KEY, VALUE>::contains(KEY key) noexcept {
    node* nearest = find_nearest_node(key);
    return nearest != nullptr && nearest->key() == key;
}

template<class KEY, class VALUE>
VALUE& bin_search_tree<KEY, VALUE>::find(const KEY &key) {
    node* nearest = find_nearest_node(key);
    if (nearest != nullptr && nearest->key() == key) {
        return nearest->value();
    } else {
        throw bst_key_absent_exception();
    }
}

template<class KEY, class VALUE>
void bin_search_tree<KEY, VALUE>::reinsert_node(std::unique_ptr<node> &&node_ptr) noexcept {
    node* parent = find_nearest_node(node_ptr->key());

    if (parent == nullptr) {
        root_ptr_ = std::move(node_ptr);
        return;
    }

    if (should_go_right(parent, node_ptr->key())) {
        parent->putRight(std::move(node_ptr));
    } else {
        parent->putLeft(std::move(node_ptr));
    }

    node_ptr->parent() = parent;
}

