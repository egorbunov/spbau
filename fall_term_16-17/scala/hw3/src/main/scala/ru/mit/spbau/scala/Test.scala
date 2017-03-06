package ru.mit.spbau.scala

import ru.mit.spbau.scala.CurrencyRates.FixerIORateGetter
import ru.mit.spbau.scala.ExchangeRatesDsl._
import ru.mit.spbau.scala.DateDsl._

object Test {
    def main(args: Array[String]): Unit = {
        val a: Float = 123.eur to rub on 1--11--2010
        val d: Float = 123.eur to rub
        val b: Float = 100.usd to eur
        val c: Float = 1000.rub to usd on 1--1--2009

        println(s"123  eur = $a rub on 1--11--2010")
        println(s"123  eur = $d rub today")
        println(s"100  usd = $b eur today")
        println(s"1000 rub = $c usd on 1--1-2009")
    }
}
