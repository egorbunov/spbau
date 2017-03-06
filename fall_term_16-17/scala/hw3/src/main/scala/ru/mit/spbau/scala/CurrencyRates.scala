package ru.mit.spbau.scala

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import ru.mit.spbau.scala.ExchangeRatesDsl.Currency

object CurrencyRates {
    trait CurrencyRateGetter {
        def getRate(from: Currency, to: Currency): Float
        def getRate(from: Currency, to: Currency, date: LocalDate): Float
    }

    implicit object DummyRateGetter extends CurrencyRateGetter {
        override def getRate(from: Currency, to: Currency): Float = 0.5f
        override def getRate(from: Currency, to: Currency, date: LocalDate): Float = 0.7f
    }

    implicit object FixerIORateGetter extends CurrencyRateGetter {
        val ratePattern = "(?:.*)rates\":\\{\"[\\w]+\":(.+)\\}\\}".r

        override def getRate(from: Currency, to: Currency): Float = {
            if (from == to) return 1f
            val url = s"http://api.fixer.io/latest?base=${from.label}&symbols=${to.label}"
            val str = scala.io.Source.fromURL(url).mkString
            return str match {
                case ratePattern(rateStr) => rateStr.toFloat
                case _ => throw new RuntimeException("No conversion available =(")
            }
        }

        override def getRate(from: Currency, to: Currency, date: LocalDate): Float = {
            if (from == to) return 1f
            val dateStr = date.format(DateTimeFormatter.ofPattern("yyyy-MM-dd"))
            val url = s"http://api.fixer.io/${dateStr}?base=${from.label}&symbols=${to.label}"
            val str = scala.io.Source.fromURL(url).mkString
            return str match {
                case ratePattern(rateStr) => rateStr.toFloat
                case _ => throw new RuntimeException("No conversion available =(")
            }
        }
    }
}


