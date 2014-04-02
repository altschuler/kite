{-# LANGUAGE OverloadedStrings #-}

-- Emit code from Kite AST
--------------------------

module Kite.Emit where

import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import Control.Monad.Error
import qualified Data.Map as Map
import Debug.Trace

import Kite.CodeGen
import Kite.Parser


toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

codegenTop :: Expr -> LLVM ()

codegenTop (PAssign (PIdentifier iden) exp) =
  define integer iden [] []

codegenTop expr =
  define integer "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen expr >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

cgen :: Expr -> Codegen AST.Operand

cgen expr = traceShow expr $ error "trace"

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ErrorT String IO a -> IO a
liftError = runErrorT >=> either fail return

codegen :: AST.Module -> Expr -> IO AST.Module
codegen mod (PBlock StandardBlock fns) = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM mod modn
