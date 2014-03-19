{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import Kite.Lexer
import Kite.Parser
import Text.Show.Pretty

main = do
  input <- getContents
  let result = runAlex input parse
  print result
  case result of
    Right ast -> (putStrLn . ppShow) ast
    Left _ -> fail "Bad things happened"
