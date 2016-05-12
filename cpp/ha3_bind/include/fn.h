#pragma once

#include <iostream>
#include <tuple>
#include <type_traits>
#include <utility>
#include <typeinfo>

// ------------------- BIND --------------------
namespace fn {
    /**
     * Base placeholder type; Used for sfinae in bind implementation.
    */
    struct placeholder_base_t
    {};

    template<size_t Idx>
    struct placeholder_t : placeholder_base_t
    {
        constexpr static size_t index = Idx - 1;
    };

    const placeholder_t<1> _1;
    const placeholder_t<2> _2;
    const placeholder_t<3> _3;
    const placeholder_t<4> _4;
    const placeholder_t<5> _5;
    const placeholder_t<6> _6;
    const placeholder_t<7> _7;
    const placeholder_t<8> _8;
    const placeholder_t<9> _9;
    const placeholder_t<10> _10;

    template<typename T>
    struct is_placeholder : std::is_base_of<placeholder_base_t, T>
    { };

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
        template<class F, class... Types>
        struct binder_t {
        private:
            typedef std::tuple<typename std::decay<Types>::type...> BindedTuple;

            typename std::decay<F>::type f;
            BindedTuple saved_args;

            template<class ArgsTuple, class T>
            static auto apply_placeholder(ArgsTuple&& args_tuple, T&& arg)
            -> typename std::enable_if<!is_placeholder<typename std::decay<T>::type>::value, T&&>::type
            {
                return arg;
            }

            template<class ArgsTuple, class T>
            static auto apply_placeholder(ArgsTuple&& args_tuple, T&& arg)
            -> typename std::enable_if<is_placeholder<typename std::decay<T>::type>::value,
                    decltype(std::get<std::decay<T>::type::index>(args_tuple))>::type
            {
                return std::get<std::decay<T>::type::index>(args_tuple);
            }

            template<class ArgTuple, size_t... Indices>
            auto call(ArgTuple&& args_tuple, const indices_t<Indices...>& ignored)
            -> decltype(f(apply_placeholder(args_tuple,std::get<Indices>(saved_args))...))
            {
                return f(apply_placeholder(args_tuple,
                                           std::get<Indices>(saved_args))...);
            }

        public:
            binder_t(F &&f, Types&&... args) : f(std::forward<F>(f)),
                                               saved_args(std::forward<Types>(args)...)
            { }

            template<class... ArgTypes>
            auto operator()(ArgTypes&&... args)
            -> decltype(std::declval<binder_t<F, Types...>>().call(std::forward_as_tuple(args...), build_indices_t<std::tuple_size<BindedTuple>::value>{}))
            {
                return call(std::forward_as_tuple(args...), build_indices_t<std::tuple_size<BindedTuple>::value>{});
            }
        };
    } // BIND IMPLEMENTATION DETAILS END

    /**
     * Analogue for bind c++ standard function
     */
    template<class F, class... Types>
    auto bind(F&& f, Types&&... args) -> binder_t<F, Types...> {
        return binder_t<F, Types...>(std::forward<F>(f), std::forward<Types>(args)...);
    }
} // END BIND IMPLEMENTATION

namespace fn {
    struct function {
    };
}
