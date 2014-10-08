{-# LANGUAGE OverloadedStrings #-}

module Main where


import Control.Exception
import Database.RethinkDB.NoClash
import qualified Database.RethinkDB as R
import Criterion.Main
import Control.Monad

main :: IO ()
main = do
  h <- prepare
  let test name = bench name . nfIO . void . run' h
  defaultMain [
    test "expr 1" $ num 1,
    test "point get" $ table "bench" # get (num 0)
    ]

prepare :: IO RethinkDBHandle
prepare = do
  h <- fmap (use "bench") $ connect "localhost" 28015 Nothing
  try_ $ run' h $ dbCreate "bench"
  try_ $ run' h $ tableCreate "bench"
  try_ $ run' h $ table "bench" # ex insert ["conflict" := str "replace"] ["id" := num 0]
  return h

try_ :: IO a -> IO (Either SomeException a)
try_ = try
