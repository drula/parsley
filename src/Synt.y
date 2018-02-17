{
module Synt where
import Lex
}

%name synt
%tokentype { Token }
%error { parseError }

%token
    int     { TNumber $$ }
    ident   { TIdent $$ }
    struct  { TStruct }
    "{"     { TLeftCrBrace }
    "}"     { TRightCrBrace }
    ";"     { TSemicolon }
    ":"     { TColon}
    typ     { TType $$ }

%%

Description : Struct { Description $1 }

Struct : struct ident "{" FieldList "}" { Struct $2 $4 }

-- FIXME: allow extra ";"
FieldList : Field { [$1] }
          | FieldList Field { $2 : $1 } -- FIXME: reverse the list

Field : Type ident ";" { Field $2 $1 }

Type : BitFieldType { Type $1 }
  -- | other types

BitFieldType : NumType ":" int { BitFieldType $1 $3 }

NumType : typ { Uimsbf } -- FIXME

{
parseError :: [Token] -> a
parseError tokens = error $ "Parsing error: " ++ show tokens

data Description = Description Struct
    deriving Show

data Struct = Struct String [Field]
    deriving Show

data Field = Field String Type
    deriving Show

data Type = Type BitFieldType -- | other types
    deriving Show

data BitFieldType = BitFieldType NumType Int
    deriving Show

-- FIXME: use more conventional typenames
data NumType = Uimsbf -- | Simsbf | Bslbf
    deriving Show
}
