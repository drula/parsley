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
          | Field FieldList { $1 : $2 } -- TODO: check complexity

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

data Struct = Struct {
    structName :: String,
    fields     :: [Field]
} deriving Show

data Field = Field {
    fieldName :: String,
    fieldType :: Type
} deriving Show

data Type = Type BitFieldType -- | other types
    deriving Show

data BitFieldType = BitFieldType {
    numType :: NumType,
    bits    :: Int
} deriving Show

-- FIXME: use more conventional typenames
data NumType = Uimsbf -- | Simsbf | Bslbf
    deriving Show
}
