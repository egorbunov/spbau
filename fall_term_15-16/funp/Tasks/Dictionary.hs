module Dictionary where

-- Имея на руках список эквивалентных слов (отношение симметрично,
-- рефлексивно, транзитивно) проверить, эквивалентны ли два слова
-- Использовать State монаду.

type Equivalence = (String, String)
type Dictionary = [Equivalence]

areMultiStepEquivalent :: Dictionary -> String -> String -> Bool
areMultiStepEquivalent = undefined
