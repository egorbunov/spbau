# Fork/Join

### Fork join pool + parallelStream

* `parallelStream` использует `ForkJoinPool`, причём использует он общий пул (видимо некое статическое поле в классе `ForkJoinPool`)
* Чтобы `parallelStream` использовал другой пул, то нужно сделать `submit()`, куда передать `parallelStream`
* Реализация метода `fork`, видимо, смотрит, в каком потоке она запущена и если это поток некоего `ForkJoinPool`, то она запускается в нём, а иначе в общем