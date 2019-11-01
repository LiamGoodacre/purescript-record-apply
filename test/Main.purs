module Test.Main where

import Prelude

import Data.Record.Apply (applyRecord)
import Effect (Effect)
import Effect.Class.Console (log, logShow)

-- setup

foo :: {a :: Boolean -> String, b :: Int -> Boolean}
foo = {a: show, b: (_ > 0)}

bar :: {a :: Boolean, b :: Int}
bar = {a: true, b: 0}

-- examples

eg0 ::
  { a :: Boolean, b :: Int } ->
  { a :: String, b :: Boolean }
eg0 x = applyRecord foo x

eg1 :: forall t0 t1.
  { a :: Boolean -> t0, b :: Int -> t1 } ->
  { a :: t0, b :: t1 }
eg1 x = applyRecord x bar

eg2 :: forall t0 t1 t2.
  { io :: {a :: t0 -> String, b :: t1 -> Boolean},
    i  :: {a :: t0, b :: t1} | t2 } ->
  { a :: String, b :: Boolean }
eg2 r = applyRecord r.io r.i

main :: Effect Unit
main = do
  logShow ((applyRecord foo {a: true, b: 1}).a)
