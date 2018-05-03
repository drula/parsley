{
module Lex where
}

%wrapper "basic" -- FIXME: what is it?

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-
    $white+                     ;
    "{"                         { \_ -> TLeftCrBrace }
    "}"                         { \_ -> TRightCrBrace }
    ";"                         { \_ -> TSemicolon }
    ":"                         { \_ -> TColon }
    "["                         { \_ -> TLeftSqBrace }
    "]"                         { \_ -> TRightSqBrace }
    "struct"                    { \_ -> TStruct }
    "uimsbf"                    { \s -> TType s }
    "string"                    { \_ -> TString }
    $digit+                     { \s -> TNumber (read s) }
    $alpha [$alpha $digit \_]*  { \s -> TIdent s }

{
data Token = TLeftCrBrace
           | TRightCrBrace
           | TSemicolon
           | TColon
           | TLeftSqBrace
           | TRightSqBrace
           | TStruct
           | TType String -- FIXME
           | TString
           | TNumber Int -- FIXME: TInt?
           | TIdent String
    deriving Show
}
