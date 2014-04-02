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

getArgType arg = case arg of
  PIntegerType -> integer
  PFloatType -> float

wrapArgs :: [Type] -> [(AST.Type, AST.Name)]
wrapArgs = map (\(PTypeArg ty (PIdentifier iden)) -> (getArgType ty, AST.Name iden))

codegenTop :: Expr -> LLVM ()

codegenTop (PAssign (PIdentifier iden) (PFunc (PFuncType args _) body)) =
  define double iden fnargs bls
  where
    fnargs = wrapArgs args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM_ args $ \(PTypeArg ty (PIdentifier iden)) -> do
        var <- alloca  $ getArgType ty
        store var (local (AST.Name iden))
        assign iden var
      cgen body >>= ret

-- codegenTop (PFunc _ _) =
--   define

-- codegenTop (PFunc (PFuncType args _) body) = do
--   define double "hei" fnargs bls
--   where
--     fnargs = wrapArgs args
--     bls = createBlocks $ execCodegen $ do
--       entry <- addBlock entryBlockName
--       setBlock entry
--       forM_ args $ \(PTypeArg ty (PIdentifier iden)) -> do
--         var <- alloca  $ getArgType ty
--         store var (local (AST.Name iden))
--         assign iden var
--       cgen body >>= ret

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

-- cgen (PUnaryOp op a) = cgen $ PCall ("unary" ++ op) [a]

-- cgen (PBinOp "=" (PVar var) val) = do
--   a <- getvar var
--   cval <- cgen val
--   store a cval
--   return cval

-- cgen (PBinOp op a b) =
--   case Map.lookup op binops of
--     Just f  -> do
--       ca <- cgen a
--       cb <- cgen b
--       f ca cb
--     Nothing -> error "No such operator"

cgen (PIdentifier iden) = getvar iden >>= load

cgen (PInteger n) = return $ cons $ C.Int 32 $ toInteger n

-- cgen (PFloat n) = return $ cons $ C.Float (F.Double n)

-- cgen (PCall fn args) = do
--   largs <- mapM cgen args
--   call (externf (AST.Name fn)) largs


cgen expr = return $ cons $ C.Int 32 $ toInteger 1
            --traceShow expr $ error "trace"

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
