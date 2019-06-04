module Language.ELisp where

data ELit
  = EL_Int    Integer
  | EL_Float  Double
  | EL_String String
  deriving (Eq , Show)

data ESExp
  -- Names and literals
  = ES_Name     String
  | ES_Lit      ELit
  -- Vectors and lists
  | ES_List     [ESExp]
  | ES_Vector   [ESExp]
  -- Quoting primitives
  | ES_Quote    ESExp
  | ES_BQuote   ESExp
  | ES_Comma    ESExp
  | ES_CommaAt  ESExp
  -- Question mark
  | ES_Question ESExp
  deriving (Eq , Show)

