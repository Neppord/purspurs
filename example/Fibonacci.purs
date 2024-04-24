module Fibonacci where

import Prelude
import Effect (Effect)

foreign import print :: Int -> Effect Unit

fib :: Int -> Int
fib n =
  if n == 0 then 0
  else if n == 1 then 1
  else fib (n - 1) + fib (n - 2)

main :: Effect Unit
main = print (fib 10)