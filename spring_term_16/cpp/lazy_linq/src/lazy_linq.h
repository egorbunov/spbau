#pragma once

#include <vector>
#include <functional>
#include <algorithm>
#include <iterator>
#include <memory>

#include <iostream>

namespace lazy_linq 
{
    namespace {
        /**
         * Enumerator base class. 
         * Enumerator is smart iterator, which is stored by enumerable
         */
        template<class T>
        struct enumerator : std::iterator<std::forward_iterator_tag, T>
        {
            virtual ~enumerator() {}
            virtual T get_current() const = 0;
            virtual bool move_next() = 0;
            virtual void reset() = 0;
            virtual enumerator<T>* clone() const = 0;
        };

        /**
         * Enumerator based on vector
         */
        template<class T>
        struct base_enumerator : enumerator<T>
        {
            base_enumerator(const std::vector<T>& data)
            : pdata{std::make_shared<std::vector<T>>(data)}
            , cur_it{pdata->end()}
            , next_it{pdata->begin()} 
            {}

            // special (copy, assign, destruct) stuff
            base_enumerator(const base_enumerator&) = default;
            base_enumerator(base_enumerator&&) = default;
            base_enumerator& operator=(const base_enumerator&) = default;
            base_enumerator& operator=(base_enumerator&&) = default;
            ~base_enumerator() = default;

            T get_current() const override
            {
                return *cur_it;
            }

            bool move_next() override
            {
                cur_it = next_it;
                if (cur_it == pdata->end()) {
                    return false;
                }
                next_it++;
                return true;
            }

            void reset() override
            {
                cur_it = pdata->end();
                next_it = pdata->begin();
            }

            enumerator<T>* clone() const override
            {
                return new base_enumerator(*pdata);
            }
        private:
            std::shared_ptr<std::vector<T>> pdata;
            typename std::vector<T>::iterator cur_it;
            typename std::vector<T>::iterator next_it;
        };

        /**
         * Filtering enumerator
         */
        template<class T>
        struct where_enumerator : enumerator<T>
        {
            where_enumerator(std::shared_ptr<enumerator<T>> pdata_it, std::function<bool(T)> pred)
            : pdata_it(pdata_it)
            , pred(pred)
            {}

            // special (copy, assign, destruct) stuff
            where_enumerator(const where_enumerator&) = default;
            where_enumerator(where_enumerator&&) = default;
            where_enumerator& operator=(const where_enumerator&) = default;
            where_enumerator& operator=(where_enumerator&&) = default;
            ~where_enumerator() = default;

            // implementation
            
            T get_current() const override
            {
                return pdata_it->get_current();
            }

            // TODO: cache already filtered stuff?
            bool move_next() override
            {
                while (pdata_it->move_next()) {
                    if (pred(pdata_it->get_current())) {
                        return true;
                    }
                }
                return false;
            }

            void reset() override
            {
                pdata_it->reset();
            }

            enumerator<T>* clone() const override
            {
                return new where_enumerator(pdata_it, pred);
            }

        private:
                /**
                 * Unfiltered data iterator
                 */
                std::shared_ptr<enumerator<T>> pdata_it;
                std::function<bool(T)> pred;
        };

        template<class T, class U>
        struct select_enumerator : enumerator<T>
        { 
           select_enumerator(std::shared_ptr<enumerator<U>> pdata_it, std::function<T(U)> transform)
            : pdata_it(pdata_it)
            , transform(transform)
            {}

            // special (copy, assign, destruct) stuff
            select_enumerator(const select_enumerator&) = default;
            select_enumerator(select_enumerator&&) = default;
            select_enumerator& operator=(const select_enumerator&) = default;
            select_enumerator& operator=(select_enumerator&&) = default;
            ~select_enumerator() = default;

            // implementation
            
            T get_current() const override
            {
                return transform(pdata_it->get_current());
            }

            // TODO: cache!
            bool move_next() override
            {
                return pdata_it->move_next();
            }

            void reset() override
            {
                pdata_it->reset();
            }

            enumerator<T>* clone() const override
            {
                return new select_enumerator(pdata_it, transform);
            }

        private:
                std::shared_ptr<enumerator<U>> pdata_it;
                std::function<T(U)> transform;
        };
    }

    template<class T>
    struct enumerable 
    {
        using value_type = T;
        using holder_type = std::vector<T>;

        /**
         * friend due to constructor privacy
         */
        template<class U>
        friend enumerable<U> from(const std::vector<U>&);

        /*
          constructors and assign operators ... 
          TODO: reconsider!!!
        */
        enumerable(const enumerable&) = default;
        enumerable(enumerable&&) = default;
        enumerable& operator=(const enumerable&) = default;
        enumerable& operator=(enumerable&&) = default;

        /**
         * Generates vector from this enumerable
         */
        std::vector<T> to_vector() const;

        /**
         * Filters elements with given predicate and copies to new enumerable
         * @param  pred predicate (condition)
         * @return new enumerable with filtered elements
         */
        enumerable where(std::function<bool(T)> pred) const;

        /**
         * Maps enumerable from T to R
         * @param transform funtion for transforming elemnt of T to R
         * @return new enumerable<R>
         */
        template<class R>
        enumerable<R> select(std::function<R(T)> transform) const;

        /**
         * @return number of elements in enumerable
         */
        size_t count();

        /**
         * @param  pred predicate
         * @return      number of elements in enumerable 
         *              which satisfy given predicate
         */
        size_t count(std::function<bool(T)> pred);

        /**
         * @return true, if collection is not empty, else false
         */
        bool any();

        /**
         * @param  pred predicate
         * @return      true if there is more than zero elements which 
         *              satisfy predicate in collection, else false
         */
        bool any(std::function<bool(T)> pred);
    private:
        // to let enumerable<U> creation from enumerable<T> class
        template<class U>
        friend class enumerable;

        // iterator through stored data
        std::shared_ptr<enumerator<T>> pdata_it;

        /**
         * TODO: copy on construction for RAII, is it right?
         */
        enumerable(const enumerator<T>& data_it): pdata_it{data_it.clone()} {}
    };

    /**
     * function, which creates enumerable from vector
     */
    template<class T>
    enumerable<T> from(const std::vector<T>& data) 
    {
        return enumerable<T>(base_enumerator<T>(data));
    }


    // _______________________ enumerable implementation ________________________
    
    template<class T>
    std::vector<T> enumerable<T>::to_vector() const {
        pdata_it->reset();
        std::vector<T> res;
        while (pdata_it->move_next()) {
            res.push_back(pdata_it->get_current());
        }
        return res;
    }

    template<class T>
    enumerable<T> enumerable<T>::where(std::function<bool(T)> pred) const
    {
        return enumerable<T>(where_enumerator<T>(pdata_it, pred));
    }

    template<class T>
    template<class R>
    enumerable<R> enumerable<T>::select(std::function<R(T)> transform) const
    {
        return enumerable<R>(select_enumerator<R, T>(pdata_it, transform));
    }

    template<class T>
    size_t enumerable<T>::count()
    {
        size_t res = 0;
        pdata_it->reset();
        while (pdata_it->move_next()) {
            res += 1;
        }
        return res;
    }

    template<class T>
    size_t enumerable<T>::count(std::function<bool(T)> pred)
    {
        return where(pred).count();
    } 

    template<class T>
    bool enumerable<T>::any()
    {
        pdata_it->reset();
        return pdata_it->move_next();
    }

    template<class T>
    bool enumerable<T>::any(std::function<bool(T)> pred)
    {
        return where(pred).any();
    }
}