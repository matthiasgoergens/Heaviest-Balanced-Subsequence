> {-# LANGUAGE PostfixOperators #-}
> module Lib
>     ( someFunc
>     ) where

> import Data.Heap

> data OC = A | V


> x :: _
> x = (1 :<)

> someFunc :: IO ()
> someFunc = putStrLn "someFunc"
