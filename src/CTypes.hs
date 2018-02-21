module CTypes where

data CContent = CContent [CTypeDef] [CFunction]
    deriving Show

data CTypeDef = CTypeDef CUserType [CVar]
    deriving Show

data CVar = CVar CType String
    deriving Show

data CType = CVoidT | CBoolT | CUintT Int | CSizeT | CUserT CUserType
           | CPtrT CType
           | CConstT CType
    deriving Show

data CUserType = CStruct String
    deriving Show

data CFunction = CFunction CFuncHeader [CInstruction]
    deriving Show

data CFuncHeader = CFuncHeader CType String [CVar]
    deriving Show

data CInstruction = CInstruction
    deriving Show
