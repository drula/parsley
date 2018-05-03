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
    "["     { TLeftSqBrace}
    "]"     { TRightSqBrace}
    typ     { TType $$ }
    string  { TString }

%%

Description : Struct { PDescription $1 }

Struct : struct ident "{" FieldList "}" { PStruct $2 $4 }

-- FIXME: allow extra ";"
FieldList : Field { [$1] }
          | Field FieldList { $1 : $2 } -- TODO: check complexity

Field : Type ident ";" { PField $2 $1 }

Type : NumType ":" int { PBitFieldType $1 $3 }
     | string "[" int "]" { PStringType $3 }

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

data PType = PBitFieldType { numType :: PNumType, bits :: Int }
           | PStringType Int
    deriving Show

-- FIXME: use more conventional typenames
data PNumType = PUimsbf -- | Simsbf | Bslbf
    deriving Show
}
