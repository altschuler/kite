{-# LANGUAGE TemplateHaskell #-}

module Kite.CodegenJS where

import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import Data.FileEmbed
import qualified Data.ByteString.Char8 as Ch
import Text.Printf

import Kite.Parser
import Kite.Syntax

type Source = String

reserved = ["while", "for", "arguments"]
kitePrefix = "KT_"

runtime = $(embedFile "js/kt_runtime.js")
opNames = [('+', "KT_PLUS"),
           ('-', "KT_MINUS"),
           ('*', "KT_STAR"),
           ('/', "KT_SLASH"),
           ('%', "KT_PERCENT"),
           ('=', "KT_EQ"),
           ('&', "KT_AMP"),
           ('|', "KT_PIPE"),
           ('<', "KT_LT"),
           ('>', "KT_GT"),
           ('!', "KT_EXCL"),
           ('.', "KT_DOT"),
           (':', "KT_COLON"),
           ('\'', "KT_PRIME"),
           ('^', "KT_HAT"),
           ('#', "KT_POUND")]

codegen :: [Decl] -> IO Source
codegen decls = do
  let r' = filter (not . (=='\n')) (Ch.unpack runtime)
  let emitted = foldl (\full (PDecl ide expr) ->
                        let decl = printf "var %s = %s" (safeId ide) (emit expr)
                        in full ++ "\n" ++ decl
                        ) "" decls
  return (r' ++ emitted ++ ";main();")

-- convert a string to a valid js identifier
safeId str =
  let safe = if str `elem` reserved
             then kitePrefix ++ str
             else str
  in concatMap replace safe

  where replace c = fromMaybe [c] (lookup c opNames)

emit :: Expr -> Source

emit PVoid = ""
emit (PInteger val) = show val
emit (PFloat val) = show val
emit (PChar val) = '"' : showLitChar val "\""
emit (PBool val) = if val then "true" else "false"

emit (PIdentifier ide) = safeId ide

emit (PList elems) = printf "[%s]" (emitAll "," elems)

emit (PPair a b) = emit (PList [a, b])

emit (PIf cond conseq alt) =
  printf "KT_if(function() { return %s; })(function() { return %s; })(function() { return %s; })" (emit cond) (emit conseq) (emit alt)

emit (PLambda (PLambdaType param _) body) =
  let PTypeArg _ ide = param
  in printf "(function(%s) {%s})" (emit ide) (emit body)

emit (PBind ide expr) =
  printf "var %s = %s; %s" (safeId ide) (emit expr) (safeId ide)

emit (PBlock exprs) =
  emitAll ";" exprs

emit (PReturn expr) =
  "return " ++ emit expr

emit (PApply expr arg) =
  printf "%s(%s)" (emit expr) (emit arg)

emit (PMatch expr pats) =
  printf "KT_match(%s,%s)" (emit expr) ("[" ++ intercalate "," (map emitPattern pats) ++ "]")

emit (PComprehension expr draws guards) =
  printf "/* !!!YTTAP-SMASH!!! */"

emitPattern (PatPair a b, val) = printf "{ type: 'pair', conseq: function (%s, %s) { return %s } }" a b (emit val)
emitPattern (PatCons hd tl, val) = printf "{ type: 'list', conseq: function (%s, %s) { return %s } }" hd tl (emit val)
emitPattern (PatPrimitive expr, val) = printf "{ type: 'simple', expr:function(){ return %s}, conseq: function () { return %s } }" (emit expr) (emit val)
emitPattern (PatOtherwise, val) = printf "{ type: 'otherwise', conseq: function () { return %s } }" (emit val)

emitAll :: String -> [Expr] -> Source
emitAll delim = intercalate delim . map emit