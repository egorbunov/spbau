sealed trait HList {
    def ::[H](h: H): HCons[H, this.type] = HCons(h, this)
}
case object HNil extends HList
case class HCons[+H, +T <: HList](head: H, tail: T) extends HList

sealed trait ChurchNum
case object Zero extends ChurchNum
case class Suc[T <: ChurchNum](prev: T) extends ChurchNum

object ChurchNum {
    type Zero = Zero.type


    def suc[T <: ChurchNum](prev: T): Suc[T] = Suc(prev)

    final val _0 = Zero
    final val _1: Suc[Zero.type] = suc(Zero)
    final val _2: Suc[Suc[Zero.type]] = suc(_1)
    final val _3: Suc[Suc[Suc[Zero.type]]] = suc(_2)
    final val _4: Suc[Suc[Suc[Suc[Zero.type]]]] = suc(_3)
    final val _5: Suc[Suc[Suc[Suc[Suc[Zero.type]]]]] = suc(_4)
}


object HList {
    type ::[+H, +T <: HList] = HCons[H, T]
    type HNil = HNil.type

    trait Splittable[A <: HList, Idx <: ChurchNum, LRes <: HList, RRes <: HList] {
        def apply(list: A, toSkip: Idx): (LRes, RRes)
    }

    object Splittable {
        import ChurchNum._

        implicit def base[RRes <: HList]: Splittable[RRes, Zero, HNil, RRes]
        = (list: RRes, toSkip: Zero) => (HNil, list)

        implicit def step[H, E, A <: HList, Idx <: ChurchNum, LRes <: HList, RRes <: HList]
        (implicit splittable: Splittable[A, Idx, LRes, RRes])
        : Splittable[H :: A, Suc[Idx], H :: LRes, RRes] =
            (list: H :: A, idx: Suc[Idx]) => {
                val (l, r) = splittable(list.tail, idx.prev)
                (list.head :: l, r)
            }
    }

    /**
      * Splits given list at `idx` position. Possible split indexes are:
      *      0    1    2    3    4    5 ...
      *        a :: b :: c :: d :: e :: ...
      *
      */
    def splitAt[A <: HList, Idx <: ChurchNum, LRes <: HList, RRes <: HList](list: A, idx: Idx)
    (implicit splittable: Splittable[A, Idx, LRes, RRes]): (LRes, RRes) = {
        splittable.apply(list, idx)
    }
}

import ChurchNum._
import HList._
val list = 1 :: 2 :: 3 :: 4 :: 5 :: HNil
splitAt(list, _3)._1
splitAt(list, _3)._2

