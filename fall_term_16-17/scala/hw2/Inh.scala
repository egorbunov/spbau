trait Base {
    var x = 1
    lazy val y = goo
    def foo: Int
    def goo: Int = x + 2
}

class Inh extends Base {
    def foo: Int = 3
}

object Inh extends Runnable {
    def run: Unit = {}

    private def foo: Int = 4
}