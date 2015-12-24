#pragma once

#include <cstddef>

namespace containers {
    template<typename T>
    class au_list {
    public:
        typedef T value_type;
        typedef value_type& reference;
        typedef value_type const & const_reference;
        typedef value_type* pointer;
        typedef value_type const* const_pointer;
        typedef size_t size_type;

        class node {
            friend class au_list;
        public:
            node();
            reference get_value();
            const_reference get_value() const;
            void set_value(const_reference);
            node* get_prev();
            node* get_prev() const;
            node* get_next();
            node* get_next() const;
        private:
            value_type value;
            node* next;
            node* prev;
        };


        au_list();
        ~au_list();
        au_list(const au_list& other);
        node* begin();
        node* begin() const;
        node* end();
        node* end() const;
        bool empty() const;
        size_type size() const;

        node* insert(node* position, const_reference val);
        template<typename V>
        node*insert(node* position, V* first, V *last);

        node* erase(node* position);
        node* erase(node* first, node* last);
        void clear();

    private:
        node* _dummy;
        node* _first;
        node* _last;
        size_type _size;

        void remove_all();
        void init();
    };
}

namespace containers {

    // node class implementation

    template<typename value_type>
    au_list<value_type>::node::node() : value(value_type()), next(nullptr), prev(nullptr) {
    }

    template<typename value_type>
    auto au_list<value_type>::node::get_value() const -> const_reference {
        return value;
    }

    template<typename value_type>
    auto au_list<value_type>::node::get_prev() -> node * {
        return prev;
    }

    template<typename value_type>
    auto au_list<value_type>::node::get_prev() const -> node * {
        return prev;
    }

    template<typename value_type>
    auto au_list<value_type>::node::get_next() -> node * {
        return next;
    }

    template<typename value_type>
    auto au_list<value_type>::node::get_next() const -> node * {
        return next;
    }

    template<typename value_type>
    void au_list<value_type>::node::set_value(const_reference rvalue) {
        value = rvalue;
    }

    template<typename value_type>
    auto au_list<value_type>::node::get_value() -> reference {
        return value;
    }

    // au_list class implementation
    template<typename value_type>
    void au_list<value_type>::init() {
        _dummy = new node();
        _first = _dummy;
        _last = _dummy;
        _dummy->next = _dummy;
        _dummy->prev = _dummy;
        _size = 0;
    }


    template<typename value_type>
    void au_list<value_type>::remove_all() {
        node *cur = _first;
        node *next = nullptr;
        while (cur != _dummy) {
            next = cur->get_next();
            delete cur;
            cur = next;
        }
        _first = _dummy;
        _last = _dummy;
        _dummy->next = _dummy;
        _dummy->prev = _dummy;
    }

    template<typename value_type>
    au_list<value_type>::~au_list() {
        remove_all();
        delete _dummy;
    }

    template<typename value_type>
    au_list<value_type>::au_list() {
        init();
    }

    template<typename value_type>
    auto au_list<value_type>::begin() -> node* {
        return _first;
    }

    template<typename value_type>
    auto au_list<value_type>::end() -> node* {
        return _last;
    }

    template<typename value_type>
    auto au_list<value_type>::begin() const -> node* {
        return _first;
    }

    template<typename value_type>
    auto au_list<value_type>::end() const -> node* {
        return _last;
    }

    template<typename value_type>
    bool au_list<value_type>::empty() const {
        return _first == _dummy && _last == _dummy && _size == 0; // just for check
    }

    template<typename value_type>
    auto au_list<value_type>::size() const -> size_type {
        return _size;
    }

    template<typename value_type>
    auto au_list<value_type>::insert(node *position, const_reference val) -> node* {
        node* new_node = new node();
        new_node->set_value(val);

        node* old_prev = position->get_prev();
        position->prev = new_node;
        new_node->next = position;
        new_node->prev = old_prev;
        old_prev->next = new_node;

        if (position == _first) {
            _first = new_node;
        }

        _size += 1;
        return new_node;
    }

    template<typename value_type>
    template<typename V>
    auto au_list<value_type>::insert(node *position, V *first, V *last) -> node* {

        node* to_return = insert(position, value_type(*first));
        first++;
        while (first != last) {
            insert(position, value_type(*first));
            first++;
        }
        insert(position, value_type(*last));
        return to_return;
    }

    template<typename value_type>
    au_list<value_type>::au_list(const au_list& other) {
        init();
        node *b = other._first;
        while (b != other._last) {
            insert(end(), b->value);
            b = b->next;
        }
    }

    template<typename value_type>
    void au_list<value_type>::clear() {
        remove_all();
    }

    template<typename value_type>
    auto au_list<value_type>::erase(node* position) -> node* {
        if (position == _dummy)
            return nullptr; // ...
        position->prev->next = position->get_next();
        position->next->prev = position->get_prev();

        if (position == _first) {
            _first = position->get_next();
        }

        _size -= 1;

        node* to_return = position->get_next();
        delete position;
        return to_return;
    }

    template<typename value_type>
    auto au_list<value_type>::erase(node *first, node *last) -> node* {
        while (first != last) {
            erase(first);
            first = first->get_next();
        }
        return last;
    }

}