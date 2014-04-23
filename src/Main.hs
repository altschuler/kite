{-# LANGUAGE DeriveDataTypeable, RecordWildCards, NoMonomorphismRestriction #-}
module Main where

import Kite.Driver
import qualified Kite.JSEmit as Kjs

import System.Console.CmdArgs
import Control.Monad
import Kite.Preprocessor

data KiteArgs = KiteArgs {
  input :: String,
  eval :: Bool,
  lexOutput :: Bool,
  parOutput :: Bool,
  jsEmit :: Bool,
  debugOutput :: Bool
  } deriving (Data, Typeable, Show)

kiteArgs = cmdArgsMode $ KiteArgs {
  input = "" &= argPos 0 &= typ "file",
  eval = False &= help "Evaluate expression",
  lexOutput = False &= help "Emit lexer output",
  parOutput = False &= help "Emit parser output",
  debugOutput = False &= help "Output debug information",
  jsEmit = False &= help "Emit JavaScript"}
           &= summary "Kite compiler v0.0.1"

main = do
  KiteArgs {..} <- cmdArgsRun kiteArgs

  --inp <- readFile input

  runKite eval debugOutput jsEmit lexOutput parOutput input
