module KnapsackFinite where

-- Решить задачу о рюкзаке (набрать рюкзак максимальной ценности
-- не превышающий максимальную вместимость по массе) когда каждый
-- предмет можно класть один раз
-- Использовать State монаду.

type Value = Int
type Weight = Int
data Item = I { weight :: Weight
              , value  :: Value
              }
type Items = [Item]

maxValue :: Items -> Weight -> Value
maxValue = undefined
