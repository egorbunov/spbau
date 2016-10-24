//Реалзуйте IntArrayBuffer с интерфейсом IntTraversable
trait IntTraversable {
    def isEmpty: Boolean

    def size: Int

    def contains(element: Int): Boolean

    def head: Int

    def tail: IntTraversable

    def ++(traversable: IntTraversable): IntTraversable

    def filter(predicate: Int => Boolean): IntTraversable

    def map(function: Int => Int): IntTraversable

    def flatMap(function: Int => IntTraversable): IntTraversable

    def foreach(function: Int => Unit): Unit
}

class IntArrayBuffer extends IntTraversable {
    private var curSize = 0
    private var buffer = new Array[Int](1)

    private def check(index: Int) =
        if (index >= curSize)
            throw new IndexOutOfBoundsException()

    private def increase(newCapacity: Int) = {
        if (newCapacity > buffer.length) {
            val tmpBuf = new Array[Int](newCapacity)
            Array.copy(buffer, 0, tmpBuf, 0, curSize)
            buffer = tmpBuf
        }
    }

    def apply(index: Int): Int = {
        check(index)
        buffer(index)
    }

    def update(index: Int, element: Int): Unit = {
        check(index)
        buffer(index) = element
    }

    def clear(): Unit = {
        curSize = 0
        buffer = new Array[Int](1)
    }

    def +=(element: Int): IntArrayBuffer = {
        if (buffer.length <= curSize)
            increase(buffer.length * 2)
        buffer(curSize) = element
        curSize += 1
        this
    }

    def ++=(elements: IntTraversable): IntArrayBuffer = {
        var capacity = buffer.length
        while (capacity < elements.size)
            capacity *= 2
        increase(capacity)
        elements.foreach(this += _)
        this
    }

    def remove(index: Int): Int = {
        check(index)
        val deleted = buffer(index)
        for (i <- index + 1 until curSize)
            buffer(i - 1) = buffer(i)
        deleted
    }

    override def isEmpty: Boolean = curSize == 0

    override def size: Int = curSize

    override def contains(element: Int): Boolean = {
        for (i <- 0 until curSize)
            if (buffer(i) == element)
                return true
        false
    }

    override def head: Int = {
        check(0)
        buffer(0)
    }

    override def tail: IntArrayBuffer = {
        var tailArr = new IntArrayBuffer
        tailArr.ensureSize(curSize)
        for (i <- 1 until curSize)
            tailArr += buffer(i)
        tailArr
    }

    override def ++(traversable: IntTraversable): IntArrayBuffer = {
        var newArrayBuf = new IntArrayBuffer
        newArrayBuf ++= this
        newArrayBuf ++= traversable
    }

    protected def ensureSize(size: Int): Unit = increase(size)

    override def filter(predicate: (Int) => Boolean): IntTraversable = {
        var newIntBuf = new IntArrayBuffer
        for (e <- this)
            if (predicate(e))
                newIntBuf += e
        newIntBuf
    }

    override def map(function: (Int) => Int): IntTraversable = {
        var newIntBuf = new IntArrayBuffer
        newIntBuf.ensureSize(curSize)
        foreach(newIntBuf += function(_))
        newIntBuf
    }

    override def flatMap(function: (Int) => IntTraversable): IntTraversable = {
        var newIntBuf = new IntArrayBuffer
        foreach(newIntBuf ++= function(_))
        newIntBuf
    }

    override def foreach(function: (Int) => Unit): Unit = {
        for (i <- 0 until curSize)
            function(buffer(i))
    }
}

object IntArrayBuffer {
    def empty: IntArrayBuffer = new IntArrayBuffer

    def apply(elements: Int*): IntArrayBuffer = {
        var arr = IntArrayBuffer.empty
        arr.ensureSize(elements.size)
        for (e <- elements)
            arr += e
        arr
    }

    def unapplySeq(buffer: IntArrayBuffer): Option[IntArrayBuffer] = {
        var arr = IntArrayBuffer.empty
        arr.ensureSize(buffer.size)
        try {
            buffer.foreach(arr += buffer(_))
            Some(arr)
        } catch {
            case e: IndexOutOfBoundsException => None
        }
    }
}

// ======================== tests ===========================

{
    val arr1 = IntArrayBuffer.empty
    assert(arr1.isEmpty)
    assert(arr1.size == 0)
}

{
    val arr1 = IntArrayBuffer(0, 1, 2, 3)
    assert(arr1.size == 4)
    for (i <- 0 to 3)
        assert(arr1(i) == i)

    val arr2 = IntArrayBuffer.unapplySeq(arr1).get
    assert(arr2.size == 4)
    for (i <- 0 to 3)
        assert(arr2(i) == arr1(i))
}

{
    val arr = IntArrayBuffer(123, 124, 125)
    for (i <- 0 to 2) {
        arr(i) = i
        assert(arr(i) == i)
    }
}

{
    val arr = IntArrayBuffer(0, 1, 2)
    arr.clear()
    assert(arr.isEmpty)
}

{
    var arr = IntArrayBuffer.empty
    val n = 100
    for (i <- 0 to n) {
        arr += i
    }
    for (i <- 0 to n) {
        assert(arr(i) == i)
    }
}

{
    var arr1 = IntArrayBuffer(0, 1, 2)
    var arr2 = IntArrayBuffer(3, 4, 5)
    arr1 ++= arr2
    for (i <- 0 to 5) {
        assert(arr1(i) == i)
    }
}

{
    val arr1 = IntArrayBuffer(0, 1, 100, 2, 3)
    val deleted = arr1.remove(2)
    assert(deleted == 100)
    for (i <- 0 to 3) {
        assert(arr1(i) == i)
    }
}

{
    val arr = IntArrayBuffer(0, 1, 2)
    assert(arr.contains(1))
    assert(!arr.contains(42))
}

{
    val arr = IntArrayBuffer(42, 0, 1, 2)
    assert(arr.head == 42)
    val tail = arr.tail
    for (i <- 0 to 2)
        assert(tail(i) == i)
}

{
    val arr1 = IntArrayBuffer(0, 1, 2)
    val arr2 = IntArrayBuffer(3, 4, 5)
    val arr = arr1 ++ arr2
    for (i <- 0 to 5) {
        assert(arr(i) == i)
    }
}

{
    val arr = IntArrayBuffer(0, 2, 4)
    val m = arr.map(_ / 2)
    val res = IntArrayBuffer.empty
    m.foreach(res += _)
    for (i <- 0 to 2)
        assert(res(i) == i)
}

{
    val arr = IntArrayBuffer(0, 3)
    val m = arr.flatMap((x: Int) => IntArrayBuffer(x, x+1, x+2))
    val res = IntArrayBuffer.empty
    m.foreach(res += _)
    for (i <- 0 to 5)
        assert(res(i) == i)
}
