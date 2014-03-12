module Kite.Driver (process) where

import Kite.CodeGen
import Kite.Emit

import qualified Kite.Parser as P

import qualified LLVM.General.AST as AST

initModule :: AST.Module
initModule = emptyModule "Kite top module"

process = codegen initModule
