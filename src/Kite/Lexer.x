{
module Kite.Lexer
( Alex(..)
, AlexPosn(..)
, Token(..)
, alexMonadScan
, runAlex
, alexGetInput
, lexwrap
) where

import Prelude hiding (lex)
}

%wrapper "monad"

$downcase		= a-z
$upcase			= A-Z
$digit			= 0-9
$alpha			= [$downcase $upcase]
$alphaNum		= [$alpha $digit]
$symbols		= [\(\)\{\}\[\]\;\,]

@keywords		= return | import | if | then | else | yolo
@binops	                = "+" | "-" | "/" | "*" | "%" | "==" | "<" | "<=" | ">" | ">=" | "!="
@operators		= "=" | "#" | "->"
@string                 = \" (. # \")* \"
@identifier		= $downcase [$alphaNum \_ \' \! \?]*
@bool			= "True" | "False"
@type			= $upcase [$alphaNum]*
@comment		= "--" .*
@multilineComment	= "{-" ($white | .)* "-}"

kite :-
  $white+		;
  @multilineComment	;
  @comment		;

  @keywords		{ tok LKeyword }
  @operators		{ tok LOperator }
  @binops		{ tok LBinOp }

  $digit+\.$digit+	{ tok LFloat }
  $digit+		{ tok LInteger }
  @bool		        { tok LBool }
  $symbols		{ tok LSymbol }

  @string               { tok LString }
  @identifier		{ tok LIdentifier }
  @type			{ tok LType }

{

data Lexeme
  = LSymbol
  | LIdentifier
  | LType
  | LInteger
  | LFloat
  | LBool
  | LString
  | LKeyword
  | LOperator
  | LBinOp
  | LEOF
  deriving (Show, Eq)

data Token
  = Symbol     Char
  | Identifier String
  | Type       String
  | Integer    Int
  | Float      Float
  | Bool       Bool
  | String     String
  | Keyword    String
  | Operator   String
  | BinOp      String
  | EOF
  deriving (Eq, Show)

tok :: Lexeme -> Int -> AlexAction Token
tok c f (_, _, _, str) len =
  let val = take len str
  in case c of
    LSymbol     ->  return $ Symbol     (head val)
    LIdentifier ->  return $ Identifier val
    LType       ->  return $ Type       val
    LInteger    ->  return $ Integer    (read val :: Int)
    LFloat      ->  return $ Float      (read val :: Float)
    LBool       ->  return $ Bool       (read val :: Bool)
    LString     ->  return $ String     val
    LKeyword    ->  return $ Keyword    val
    LOperator   ->  return $ Operator   val
    LBinOp      ->  return $ BinOp      val
    LEOF        ->  return $ EOF

-- called by parser
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)
alexEOF = return EOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> a) -> AlexAction a
lex f = \(_,_,_,s) i -> return (f (take i s))

-- For constructing tokens that do not depend on
-- the input
lex' :: a -> AlexAction a
lex' = lex . const

}
