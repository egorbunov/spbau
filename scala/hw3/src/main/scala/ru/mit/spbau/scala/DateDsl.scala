package ru.mit.spbau.scala

import java.time.LocalDate


object DateDsl {
    implicit class Day(day: Int) {
        def --(month: Int) = DayMonth(day, month)
    }

    case class DayMonth(day: Int, month: Int) {
        def --(year: Int) = LocalDate.of(year, month, day)
    }
}
