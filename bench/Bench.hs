{-# LANGUAGE OverloadedStrings #-}

-- TODO: find the cause of the bad performance reported by these benchmarks

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
  let testn n name q = bench ("[" ++ show n ++ "x] " ++ name) . nfIO . (mapM_ next =<<) . sequence . replicate n $ runCursor h q
  defaultMain [
    test "nil" $ expr Null,
    testn 10 "nil" $ expr Null,
    testn 100 "nil" $ expr Null,
    test "point get" $ table "bench" # get (num 0)
    ]

runCursor :: RethinkDBHandle -> ReQL -> IO (Cursor Datum)
runCursor = R.run

prepare :: IO RethinkDBHandle
prepare = do
  h <- fmap (use "bench") $ connect "localhost" 28015 Nothing
  try_ $ run' h $ dbCreate "bench"
  try_ $ run' h $ tableCreate "bench"
  try_ $ run' h $ table "bench" # ex insert ["conflict" := str "replace"] ["id" := num 0]
  return h

try_ :: IO a -> IO (Either SomeException a)
try_ = try
