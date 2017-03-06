package ru.mit.spbau.scala

import java.time.LocalDate

import ru.mit.spbau.scala.CurrencyRates.CurrencyRateGetter

object ExchangeRatesDsl {
    /**
      * Classes for particular currencies, which are used in exchange;
      * If u add one here it will become available after 'to'
      */
    sealed abstract class Currency(val label: String)
    object eur extends Currency("EUR")
    object rub extends Currency("RUB")
    object usd extends Currency("USD")

    abstract case class CurrencyAmount(amount: Float, currency: Currency) {
        def to(toCurrency: Currency) = Exchange(amount, currency, toCurrency)
    }

    /**
      * Here is particular currency amount builder;
      * To extend set of available convertible currencies
      * add another object inside
      * @param amount number of particular currency units
      */
    implicit class AmountBuilder(amount: Float) {
        object eur extends CurrencyAmount(amount, ExchangeRatesDsl.eur)
        object usd extends CurrencyAmount(amount, ExchangeRatesDsl.usd)
        object rub extends CurrencyAmount(amount, ExchangeRatesDsl.rub)
    }

    case class Exchange(amount: Float, from: Currency, to: Currency) {
        def on(localDate: LocalDate) = DatedExchange(amount, from, to, localDate)
    }

    case class DatedExchange(amount: Float,
                             from: Currency,
                             to: Currency,
                             date: LocalDate)

    implicit def exchange2Float(exchange: Exchange)
                                (implicit rateGetter: CurrencyRateGetter): Float = {
        exchange.amount * rateGetter.getRate(exchange.from, exchange.to)
    }

    implicit def datedExchange2Float(exchange: DatedExchange)
                               (implicit rateGetter: CurrencyRateGetter): Float = {
        exchange.amount * rateGetter.getRate(exchange.from, exchange.to, exchange.date)
    }

}
