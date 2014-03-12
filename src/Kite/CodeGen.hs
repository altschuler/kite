{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Kite.CodeGen where

import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import Control.Applicative
import Control.Monad.State

import LLVM.General.AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST

import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.FloatingPointPredicate as FP

double :: Type
double = FloatingPointType 64 IEEE

type SymbolTable = [(String, Operand)]

data CodegenState =
  CodegenState {
    currentBlock :: Name,
    blocks :: Map.Map Name BlockState,
    symtab :: SymbolTable,
    blockCount :: Int,
    count :: Word,
    names :: Names
    } deriving (Show)

data BlockState =
  BlockState {
    idx :: Int,
    stack :: [Named Instruction],
    term :: Maybe (Named Terminator)
    } deriving (Show)

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
                  deriving (Functor, Applicative, Monad, MonadState CodegenState)

-----------------
--    MODULE
-----------------
newtype LLVM a = LLVM { unLLVM :: State AST.Module a }
               deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn def = do
  defs <- gets moduleDefinitions
  modify (\s -> s { moduleDefinitions = defs ++ [def] })

define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retType label argTypes body = addDefn $
  GlobalDefinition $ functionDefaults {
    name = Name label,
    parameters = ([Parameter ty nm [] | (ty, nm) <- argTypes], False),
    returnType = retType,
    basicBlocks = body
    }

external :: Type -> String -> [(Type, Name)] -> LLVM ()
external retType label argTypes = addDefn $
  GlobalDefinition $ functionDefaults {
    name = Name label,
    parameters = ([Parameter ty nm [] | (ty, nm) <- argTypes], False),
    returnType = retType,
    basicBlocks = []
  }


-------------
-- BLOCKS
-------------

entry :: Codegen Name
entry = gets currentBlock

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

addBlock :: String -> Codegen Name
addBlock blockName = do
  blks <- gets blocks
  ix <- gets blockCount
  nms <- gets names

  let new = emptyBlock ix
      (qname, supply) = uniqueName blockName nms

  modify $ \s -> s {
    blocks = Map.insert (Name qname) new blks,
    blockCount = ix + 1,
    names = supply
    }

  return (Name qname)

-- set a new current block to be worked on
setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

-- add instructions to current block
modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

-- get a fresh name for an instruction
fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

type Names = Map.Map String Int

-- get a unique name for an instruction
uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm, Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix + 1) ns)

instance IsString Name where
  fromString = Name . fromString

local :: Name -> Operand
local = LocalReference

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference


-------------
-- Sym table
-------------
assign :: String -> Operand -> Codegen ()
assign var x = do
  locals <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ locals }

getvar :: String -> Codegen Operand
getvar nm = do
  syms <- gets symtab
  case lookup nm syms of
    Just x -> return x
    Nothing -> error $ "Undefined variable: " ++ show nm

instr :: Instruction -> Codegen Operand
instr ins = do
  freshName <- fresh
  block <- current
  let i = stack block
  let ref = UnName freshName
  modifyBlock $ block { stack = i ++ [ref := ins] }
  return (local ref)

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  block <- current
  modifyBlock $ block { term = Just trm }
  return trm


-------------
-- Arith
-------------
fadd :: Operand -> Operand -> Codegen Operand
fadd l r = instr $ FAdd l r []

fsub :: Operand -> Operand -> Codegen Operand
fsub l r = instr $ FSub l r []

fmul :: Operand -> Operand -> Codegen Operand
fmul l r = instr $ FMul l r []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv l r = instr $ FDiv l r []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call False CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

-- Control flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []
