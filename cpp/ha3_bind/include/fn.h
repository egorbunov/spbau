#pragma once

#include <iostream>
#include <tuple>
#include <type_traits>
#include <utility>
#include <typeinfo>

namespace fn {
    // Placeholders
    struct placeholder_base_t
    {};

    template<size_t Idx>
    struct placeholder_t : placeholder_base_t
    {
        constexpr static size_t index = Idx;
    };

    const placeholder_t<1> _1;
    const placeholder_t<2> _2;
    const placeholder_t<3> _3;
    const placeholder_t<4> _4;
    const placeholder_t<5> _5;
    const placeholder_t<6> _6;

    template<typename T>
    struct is_placeholder : std::is_base_of<placeholder_base_t, T>
    { };
    // Placeholders end


    /**
     * Bind implementation details
     */
    namespace {
        template<size_t... Indices>
        struct indices_t {};

        /**
         * Recursive indices struct. Used for generation template with
         * with indices arguments: <0, 1, 2, 3, ..., N>
         */
        template<size_t N, size_t... Indices>
        struct build_indices_t : build_indices_t<N-1, N-1, Indices...>
        {};

        template<size_t... Indices>
        struct build_indices_t<0, Indices...> : indices_t<Indices...>
        {};

        /**
         * Binder struct
         */
        template<class F, class Tuple>
        struct binder_t {
        private:
            typename std::decay<F>::type f;
            Tuple saved_args;

            template<class ArgsTuple, class T>
            static auto apply_placeholder(ArgsTuple&& args_tuple, T&& arg)
            -> typename std::enable_if<!is_placeholder<typename std::decay<T>::type>::value, T&&>::type
            {
                return arg;
            }

            // TODO: ugly -1, but...
            template<class ArgsTuple, class T>
            static auto apply_placeholder(ArgsTuple&& args_tuple, T&& arg)
            -> typename std::enable_if<is_placeholder<typename std::decay<T>::type>::value,
                    decltype(std::get<std::decay<T>::type::index - 1>(args_tuple))>::type
            {
                return std::get<std::decay<T>::type::index - 1>(args_tuple);
            }

            template<class ArgTuple, size_t... Indices>
            auto call(ArgTuple&& args_tuple, const indices_t<Indices...>& ignored)
            -> decltype(f(apply_placeholder(args_tuple,
                                            std::get<Indices>(saved_args))...))
            {
                return f(apply_placeholder(args_tuple,
                                           std::get<Indices>(saved_args))...);
            }

        public:
            binder_t(F &&f, Tuple &&saved_args) : f(f), saved_args(saved_args)
            {}

            /**
             * TODO: I can't move that function definition to the head of the struct, because
             * TODO: compiler says, that `call` function (in return type) can't be found. Why?!
             * TODO: How to forward declare functions with auto + decltype?
             */
            template<class... Types>
            auto operator()(Types&&... args)
            -> decltype(call(std::forward_as_tuple(args...), build_indices_t<std::tuple_size<Tuple>::value>{}))
            {
                return call(std::forward_as_tuple(args...), build_indices_t<std::tuple_size<Tuple>::value>{});
            }
        };
    } // BIND IMPLEMENTATION DETAILS END

    /**
     * Analogue for bind c++ standard function
     */
    template<class F, class... Types>
    auto bind(F&& f, Types&&... args) -> binder_t<F, decltype(std::forward_as_tuple(args...))>
    {
        return binder_t<F, decltype(std::forward_as_tuple(args...))>(std::forward<F>(f),
                                                                     std::forward_as_tuple(args...));
    }
}
