{-# LANGUAGE OverloadedStrings #-}

-- Emit code from Kite AST
--------------------------

module Kite.Emit where

import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST

import Control.Monad.Error

import Kite.CodeGen
import qualified Kite.Parser as P

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

codegenTop :: P.Expr -> LLVM ()
--codegenTop (S.PAssign (PIdentifier nm) (PInteger val)) =
codegenTop _ =
  define double "foo" [] []


-- compile
liftError :: ErrorT String IO a -> IO a
liftError = runErrorT >=> either fail return

codegen :: AST.Module -> P.Expr -> IO AST.Module
codegen modu fns = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn = codegenTop fns
    newast = runLLVM modu modn
