#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (
  readFile, putStr, putStrLn, takeWhile, hPutStrLn, writeFile,
  unlines, unwords)
import Data.Attoparsec.Text
import Data.Text.IO
import Data.Text (unwords, unlines, pack, unpack)
import System.Exit
import System.IO (stderr)
import Data.Maybe
import Control.Applicative
import Data.Monoid
import Data.List (intersperse)
import Data.Char
import Control.Monad

import Debug.Trace

main = do
  proto <- readFile "ql2.proto"
  case parseOnly protoFile proto of
    Left err -> hPutStrLn stderr ("Error: " <> pack err) >> exitWith ExitSuccess
    Right mod -> do
      writeFile "Database/RethinkDB/Wire.hs" genRaw
      forM_ mod $ \(name, enums) ->
        writeFile (unpack $ "Database/RethinkDB/Wire/" <> name <> ".hs")
        (renderMessage (name, enums))

protoFile = tr "protoFile" $ do
  many message

message = tr "message" $ do
  token "message"
  n <- name
  token "{"
  body <- catMaybes <$> many justEnums
  token "}"
  return (n, body)

justEnums = tr "justEnums" $ choice [
  Just <$> enum,
  const Nothing <$> field,
  const Nothing <$> message
  ]

field = tr "field" $ do
  choice [token "repeated", token "optional", token "extensions"]
  skipWhile (/=';')
  string ";"

enum = tr "enum" $ do
  token "enum"
  n <- name
  token "{"
  d <- many decl
  token "}"
  return (n,d)

decl = tr "decl" $ do
  n <- name
  token "="
  v <- value
  choice [token ";", string ";"]
  return (n,v)

value = tr "value" $ whitespace >> takeWhile (\c -> not (isSpace c) && c /= ';')

name = tr "name" $ whitespace >> takeWhile1 (`elem` alphanum)

alphanum = "_" <> ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']

token s = tr ("token " ++ show s) $ whitespace >> string s

whitespace = do
  many1 $ choice [
    satisfy isSpace >> skipWhile isSpace,
    string "//" >> skipWhile (not . isEndOfLine) ]
  return ()

genRaw = unlines $ [
  "module Database.RethinkDB.Wire where",
  "class WireValue a where",
  "  toWire :: a -> Int",
  "  fromWire :: Int -> Maybe a"
  ]

renderMessage (name, enums) = unlines $ [
  unwords ["module", "Database.RethinkDB.Wire." <> name, "where"],
  "import Prelude (Maybe(..), Int)",
  "import Database.RethinkDB.Wire"
  ] ++ map renderEnum enums

renderEnum (name, decls) = unlines $ [
  unwords $ ["data", name, "="] <> intersperse "|" (map fst decls),
  unwords ["instance WireValue", name, "where"],
  indent $
  (for decls $ \(var, val) -> "toWire " <> var <> " = " <> val) <>
  (for decls $ \(var, val) -> "fromWire " <> val <> " = Just " <> var) <>
  ["fromWire _ = Nothing"]
  ]


indent = unlines . map ("  " <>)

for = flip map

tr s p = p <?> s