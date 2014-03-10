{
module Kite.Lexer where
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

data Lexeme = LSymbol
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

tok :: Lexeme -> AlexInput -> Int -> Alex Token
tok c (pn, _, _, str) len =
  let val = take len str
  in case c of
    LSymbol ->      return $ Symbol     pn (head val)
    LIdentifier ->  return $ Identifier pn val
    LType ->        return $ Type       pn val
    LInteger ->     return $ Integer    pn (read val :: Int)
    LFloat ->       return $ Float      pn (read val :: Float)
    LBool ->        return $ Bool       pn (read val :: Bool)
    LString ->      return $ String     pn val
    LKeyword ->     return $ Keyword    pn val
    LOperator ->    return $ Operator   pn val
    LBinOp ->       return $ BinOp      pn val
    LEOF ->         return $ EOF

alexEOF :: Alex Token
alexEOF = return EOF

data Token = Symbol     AlexPosn Char
           | Identifier AlexPosn String
           | Type       AlexPosn String
           | Integer    AlexPosn Int
           | Float      AlexPosn Float
           | Bool       AlexPosn Bool
           | String     AlexPosn String
           | Keyword    AlexPosn String
           | Operator   AlexPosn String
           | BinOp      AlexPosn String
           | EOF
           deriving (Eq, Show)

-- get the AlexPosn from a token
tok2posn (Symbol     p _) = p
tok2posn (Identifier p _) = p
tok2posn (Type       p _) = p
tok2posn (Integer    p _) = p
tok2posn (Float      p _) = p
tok2posn (String     p _) = p
tok2posn (Bool       p _) = p
tok2posn (Keyword    p _) = p
tok2posn (Operator   p _) = p
tok2posn (BinOp      p _) = p
--tok2posn (EOF        p  ) = p
}
