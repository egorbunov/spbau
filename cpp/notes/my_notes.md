### То, что мне в новинку в C++

#### Copy Elision and Return value optimization

* RVO is Copy Elision

#### R-value reference (r-value ссылки)

* Больше тут: http://thbecker.net/articles/rvalue_references/section_01.html

##### Основы

Как что называется (не точно, конечно, ибо точные определения слишком 
громоздки):

* lvalue -- то, что может быть слева (и справа) от знака `=`
* rvalue -- то, что может быть только справа от знака `=`
* lvalue ссылка -- это обычная ссылка с C++: `int&`
* rvalue ссылка -- это ссылка на rvalue объект, т.е. `int&&`

Как раз rvalue-ссылки позволяют избежать лишних копирований.

Вот так выглядит функция `swap` в `c++11`:
```c++
template<class T>
void swap(T& x, T& y) {
    T tmp(std::move(x));
    x = std::move(y);
    y = std::move(tmp);
}
```

Функция `std::move` возвращает rvalue-ссылку на переданный аргумент, что, в случае наличия перегрузок для конструктора копирования и присваивания у типа `T` для rvalue-ссылок вызовет именно их. Такое допустимо в данной функции, т.к. после строки `T tmp(std::move(x));` объект `x` нигде СПРАВА не используется (аналогично для `y` и `tmp`).

##### Особенности

* Когда мы вызываем обычный оператор присваивания `a = b`, то ожидаем, что объект, на который ссылался `a` будет деструктурирован (в целом, при реализации этого оператора через swap-trick так и происходит), но когда мы вызываем move-присваивание: `a = std::move(b)`, то, вообще говоря, не понятно, когда объект, на который раньше ссылался `a` будет деструктурирован (т.к. при реализации через swap в данном случае, деструктор при выходе из оператора не будет вызван, но будет вызван, когда `b` выйдет из скоупа), поэтому **НУЖНО** в move-операторе присваивания предварительно самостоятельно выполнять всю работу по деструктурированию старого объекта...
* Важно понимать: если у rvalue-ссылки есть имя, то это lvalue, а если нет, то rvalue ("**If it has a name, then it's an lvalue.**"):
       ```
       void fun(X&& x) {
          X other_x = x; //X(const X&) called
       }
       X&& gun();
       X x = gun(); // X(X&&) called
       ```
* 

#### `std::decltype` и `auto`


#### `std::decay`


#### `std::cref`

#### Template parameter deduction


#### Perfect Forwarding

Тут самое главное понять проблему. Читай тут http://thbecker.net/articles/rvalue_references/section_01.html пока не пронзишь пространство.

Вот ещё всякие вопросы со стек оверфлоу (полезные):

* http://stackoverflow.com/questions/7779900/why-is-template-argument-deduction-disabled-with-stdforward

Вопрос выше о том, зачем нужно в `forward` форсировать специализацию аргумента вручную с помощью `remove_reference` (можно и другим способом). Вот код, который как бе намекает (нужно аккуратно проследить за тем, какие типы выводит компилятор при подстановке в шаблоны):

```c++
template<typename T>
T&& forward_with_deduction(T&& obj)
{
    return static_cast<T&&>(obj);
}

template<typename T>
T&& my_fwd(typename std::remove_reference<T>::type& obj)
{
    return static_cast<T&&>(obj);
}

void test(int&){
    std::cout << "int &" << std::endl;
}
void test(const int&){
    std::cout << "Cont int &" << std::endl;
}
void test(int&&){
    std::cout << "int &&" << std::endl;
}

template<typename T>
void perfect_forwarder(T&& obj)
{
    test(forward_with_deduction(obj));
}

template<typename T>
void my_perfect_forwarder(T&& obj)
{
    test(my_fwd<T>(obj));
}

int main()
{
    int x;
    const int& y(x);
    int&& z = std::move(x);

    //  All the below call test(int&) or test(const int&) because in perfect_forwarder 'obj' is treated as
    //  an int& or const int& (because it is named) so T in forward_with_deduction is deduced as int&
    //  or const int&. The T&& in static_cast<T&&>(obj) then collapses to int& or const int& - which is not what
    //  we want in the bottom two cases.
    perfect_forwarder(x);
    perfect_forwarder(y);
    perfect_forwarder(std::move(x));
    perfect_forwarder(std::move(y));

    std::cout << "MY: " << std::endl;

    my_perfect_forwarder(x);
    my_perfect_forwarder(y);
    my_perfect_forwarder(std::move(x));
    my_perfect_forwarder(std::move(y));
}

```
