{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell #-}
module Kite.Driver (
  runKite
, lex
, parse
, process
, foundation
) where

import Prelude hiding (lex)

import Text.Show.Pretty
import Data.FileEmbed
import qualified Data.ByteString.Char8 as Ch

import Kite.Lexer
import Kite.Parser
import Kite.TypeCheck
import Kite.Preprocessor
import Kite.Codegen

import Control.Monad
import qualified Kite.CodegenJS as GenJS

lex        = alexScanTokens
parse      = kiteparser
--analyze = typeCheck
process    = preprocess
foundation = $(embedFile "lib/Foundation.kite")

-- ev: eval, db: debug, js: emit js, lx: lex output, pr: parser output
runKite exfnd ev db target lx pr source = do
  p <- if ev then return source else process source
  let p' = if exfnd
           then p
           else Ch.unpack foundation ++ p

  let tokens = lex p'
  when lx (prettyPrint tokens)

  let decls = parse tokens
  when pr (prettyPrint decls)

  let analysis = typeCheck db decls
  case analysis of
    Right _ -> case target of
      JavaScript -> GenJS.codegen decls >>= putStrLn
      LLVM -> putStrLn "Such LLVM"
    Left err -> print err

  where prettyPrint = putStrLn . ppShow
