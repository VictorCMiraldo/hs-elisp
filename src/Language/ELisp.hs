module Language.ELisp where

data ELit
  = EL_Int    Integer
  | EL_Float  Double
  | EL_String String
  | EL_Char   Char
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
  -- We'll lift defun to a constructor in the AST
  -- for easier control.
  -- This is breaking ghc when used with generics-mrsop for whatever reason
  -- | ES_Defun String [ESExp] (Maybe String) [ESExp]
  deriving (Eq , Show)

