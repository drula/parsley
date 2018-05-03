module CTypes where

-- Lists of type and function definitions (the content of header and source files)
data CContent = CContent [CTypeDef] [CFunction]
    deriving Show

-- Type definition: struct/enum/union name and a list of variable declarations
data CTypeDef = CTypeDef CUserTypeName [CVarDecl]
    deriving Show

-- Variable declaration: type and name
data CVarDecl = CVarDecl CType String
    deriving Show

-- C Type
data CType = CVoidT | CBoolT | CUintT Int | CSizeT -- basic C types: void, bool, intN_t, size_t
           | CCharArrayT Int -- char[n]
           | CUserT CUserTypeName -- user defined type
           | CResultT -- parsley result type
           | CBitStreamT -- bitstream_t
           | CPtrT CType -- pointer to type
           | CConstT CType -- type with const qualifier
    deriving Show

-- User type definition: struct/union/enum name
data CUserTypeName = CStruct String
                -- | CEnum String
    deriving Show

-- Function definition: function header and list of instructions
data CFunction = CFunction CFuncHeader [CInstruction]
    deriving Show

-- Function header: return value type, function name and list of parameters
data CFuncHeader = CFuncHeader CType String [CVarDecl]
    deriving Show

-- Instruction
data CInstruction = CVarD CVarDecl -- variable declaration
                  | CRV CRValue
                  | CAssignment String CRValue -- assignment: var = value
                  | CIfElse CRValue [CInstruction] [CInstruction] -- if-else operator: if (condition) { instructions } else { instructions }
                  | CReturn String -- return statement
                  | CEmpty -- empty string (temporary), TODO: group instructions into blocks
    deriving Show

data CRValue = CJust String
             | CFuncCall String [String] -- function call: function name and list of parameters
    deriving Show

-- Condition
{-data CCondition = CCondition String -- TODO
    deriving Show-}
