{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell #-}
module Kite.Driver (
  runKite
, lex
, parse
, preprocess
, preprocessFile
, foundation
) where

import Prelude hiding (lex)

import Text.Show.Pretty
import Data.FileEmbed
import qualified Data.ByteString.Char8 as Ch

import Kite.Lexer
import Kite.Parser
import Kite.Syntax
import Kite.TypeCheck
import Kite.Preprocessor
import Kite.Codegen

import Control.Monad
import qualified Kite.CodegenJS as GenJS

lex        = alexScanTokens
parse      = kiteparser
--analyze = typeCheck
foundation = $(embedFile "lib/Foundation.kite")

-- ev: eval, db: debug, target: compile target, lx: lex output, pr: parser output
runKite noFnd noEmit noTypeCheck desugar eval db target lx pr source = do
  p <- if eval then preprocess source else preprocessFile source
  let p' = if noFnd
           then p
           else Ch.unpack foundation ++ p

  let tokens = lex p'
  when lx (prettyPrint tokens)

  let decls = parse tokens
  when pr (prettyPrint decls)

  when desugar (putStrLn (prettyDecls decls))

  if noTypeCheck
    then unless noEmit $ GenJS.codegen decls >>= putStrLn
    else do let analysis = typeCheck db decls
            case analysis of
              Right _ -> case target of
                JavaScript -> unless noEmit $ GenJS.codegen decls >>= putStrLn
                LLVM -> putStrLn "Such LLVM"
              Left err -> print err

  where prettyPrint = putStrLn . ppShow
