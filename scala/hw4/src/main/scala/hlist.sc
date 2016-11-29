// HList = HNil | Cons(T, HList)

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

    // append

    trait Appendable[L <: HList, R <: HList, Res <: HList] {
        def apply(l: L, r: R): Res
    }

    object Appendable {
        implicit def base[R <: HList]: Appendable[HNil, R, R] = new Appendable[HNil, R, R] {
            override def apply(l: HNil, r: R): R = r
        }

        implicit def step[H, L <: HList, R <: HList, Res <: HList](implicit appendable: Appendable[L, R, Res])
        : Appendable[H :: L, R, H :: Res] = {
            new Appendable[H :: L, R, H :: Res] {
                override def apply(l: H :: L, r: R): H :: Res = {
                    l.head :: appendable.apply(l.tail, r)
                }
            }
        }
    }

    def append[L <: HList, R <: HList, Res <: HList](l: L, r: R)(implicit appendable: Appendable[L, R, Res]): Res = {
        appendable(l, r)
    }

    // split

    trait Splittable[A <: HList, Idx <: ChurchNum, LRes <: HList, RRes <: HList] {
        def apply(list: A, toSkip: Idx): (LRes, RRes)
    }

    object Splittable {
        import ChurchNum._

        implicit def base[RRes <: HList]: Splittable[RRes, Zero, HNil, RRes]
        = new Splittable[RRes, Zero, HNil, RRes] {
            override def apply(list: RRes, toSkip: Zero): (HNil, RRes) = (HNil, list)
        }

        implicit def step[H, E, A <: HList, Idx <: ChurchNum, LRes <: HList, RRes <: HList]
        (implicit splittable: Splittable[A, Idx, LRes, RRes])
        : Splittable[H :: A, Suc[Idx], H :: LRes, RRes] =
            new Splittable[H :: A, Suc[Idx], H :: LRes, RRes] {
                override def apply(list: H :: A, idx: Suc[Idx]): (H :: LRes, RRes) = {
                    val (l, r) = splittable(list.tail, idx.prev)
                    return (list.head :: l, r)
                }
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

import HList._
import ChurchNum._
val list = 1 :: 2 :: 3 :: 4 :: 5 :: HNil
splitAt(list, _3)._1
splitAt(list, _3)._2

