module MonadTutorial where

import Data.Functor (fmap)
import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else [x*x]
{-
xs >>= (\x -> if even x then ... else ...)
-}