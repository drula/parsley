{
module Lex where
}

%wrapper "basic" -- FIXME: what is it?

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-
    $white+                     ;
    "{"                         { \s -> TLeftCrBrace }
    "}"                         { \s -> TRightCrBrace }
    ";"                         { \s -> TSemicolon }
    ":"                         { \s -> TColon }
    "struct"                    { \s -> TStruct }
    "uimsbf"                    { \s -> TType s }
    $digit+                     { \s -> TNumber (read s) }
    $alpha [$alpha $digit \_]*  { \s -> TIdent s }

{
data Token = TLeftCrBrace
           | TRightCrBrace
           | TSemicolon
           | TColon
           | TStruct
           | TType String -- FIXME
           | TNumber Int -- FIXME: TInt?
           | TIdent String
    deriving Show
}
