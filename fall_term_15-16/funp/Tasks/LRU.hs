module LRU where

-- Определить структуру LRUCache и поддержать для нее операции
-- getE - получает элемент по ключу и обновляет время доступа
-- putE - добавляет элемент по ключу
-- newLRUCache - создает новый LRUCache заданно вместимости
-- необязательна самая эффективная реализация

data LRUCache k v

getE :: Ord k => k -> LRUCache k v -> Maybe v
getE = undefined

putE :: Ord k => k -> v -> LRUCache k v -> LRUCache k v
putE = undefined

newLRUCache :: Ord k => Int -> LRUCache k v
newLRUCache = undefined
