module Seminar9 where

-- Applicative, Monad - difference?

ifC :: Monad m => m Bool -> m a -> m a -> m a
ifC cond t f = do
	comd' <- cond
	if cond' then t
			 else f

ifA :: Applicative f => f Bool -> f a -> f a -> f a
	-- <*> pure...ifA?

-- stack
-- stack new %name% -- creates project template
-- stack install