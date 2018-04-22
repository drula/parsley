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

Description : Struct { PDescription $1 }

Struct : struct ident "{" FieldList "}" { PStruct $2 $4 }

-- FIXME: allow extra ";"
FieldList : Field { [$1] }
          | Field FieldList { $1 : $2 } -- TODO: check complexity

Field : Type ident ";" { PField $2 $1 }

Type : BitFieldType { PType $1 }
  -- | other types

BitFieldType : NumType ":" int { PBitFieldType $1 $3 }

NumType : typ { PUimsbf } -- FIXME

{
parseError :: [Token] -> a
parseError tokens = error $ "Parsing error: " ++ show tokens

data PDescription = PDescription PStruct
    deriving Show

data PStruct = PStruct {
    structName :: String,
    fields     :: [PField]
} deriving Show

data PField = PField {
    fieldName :: String,
    fieldType :: PType
} deriving Show

data PType = PType { getType :: PBitFieldType } -- | other types
    deriving Show

data PBitFieldType = PBitFieldType {
    numType :: PNumType,
    bits    :: Int
} deriving Show

-- FIXME: use more conventional typenames
data PNumType = PUimsbf -- | Simsbf | Bslbf
    deriving Show
}
