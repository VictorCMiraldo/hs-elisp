{-# LANGUAGE FlexibleContexts #-}
module Language.ELisp.Parser (parseFile, parseString) where

import           Control.Monad.Identity

import           Data.Char         (isSpace, digitToInt)
import qualified Data.Text         as T
import qualified Data.Text.IO      as T
import qualified Data.List         as L (lookup)
import qualified Text.Parsec       as P
import           Text.Parsec       ((<|>), (<?>))
import qualified Text.Parsec.Token as PT
---------------------
import           Language.ELisp

elispDef :: (Monad m , P.Stream s m Char)
         => PT.GenLanguageDef s u m
elispDef = PT.LanguageDef
  { PT.commentStart    = ""
  , PT.commentEnd      = ""
  , PT.commentLine     = ";"
  , PT.nestedComments  = False
  , PT.identStart      = P.letter   <|> P.oneOf "\\!<>:/+-=~#$%^&*_"
  , PT.identLetter     = P.alphaNum <|> P.oneOf  "!<>:@/+-=~#$%^&*_?"
  , PT.opStart         = P.unexpected "No Op"
  , PT.opLetter        = P.unexpected "No Op Letter"
  , PT.reservedNames   = []
  , PT.reservedOpNames = ["`", "'", "@" , "?"] 
  , PT.caseSensitive   = True
  }

type Parser = P.Parsec T.Text ()

lexer :: PT.GenTokenParser T.Text st Identity
lexer = PT.makeTokenParser elispDef

parens      = PT.parens        lexer
ident       = PT.identifier    lexer
brackets    = PT.brackets      lexer
identifier  = PT.identifier    lexer
integer     = PT.integer       lexer
decimal     = PT.decimal       lexer
float       = PT.float         lexer
hexadecimal = PT.hexadecimal   lexer
octal       = PT.octal         lexer
comma       = PT.comma         lexer
dot         = PT.dot           lexer
quote       = PT.symbol        lexer "'"
backquote   = PT.symbol        lexer "`"
at          = PT.symbol        lexer "@"
question    = PT.symbol        lexer "?"
spaces      = PT.whiteSpace    lexer
reserved    = PT.reserved      lexer
lexeme      = PT.lexeme        lexer

escMap = zip ("().dsabfnrtvC\\\"\'") ("().\0x7F \a\b\f\n\r\t\vC\\\"\'")

--------------------------------------------------
-- I copied the stringLiteral code from Text.Parsec.Token
-- and modified it slightly. ELisp strings support newlines
-- without escaped

stringLit   = lexeme (
                  do{ str <- P.between (P.char '"')
                                       (P.char '"' <?> "end of string")
                                       (P.many stringChar)
                    ; return (foldr (maybe id (:)) "" str)
                    }
                  <?> "literal string")
  where
    stringChar      =   do{ c <- stringLetter; return (Just c) }
                    <|> stringEscape
                    <?> "string character"

    stringLetter    = P.satisfy (\c -> (c == '\n') || ((c /= '"') && (c /= '\\') && (c > '\026')))

    stringEscape    = do{ _ <- P.char '\\'
                        ;     do{ _   <- escapeGap ; return Nothing }
                          <|> do{ esc <- escapeCode; return (Just esc) }
                        }

    escapeGap       = do{ _ <- P.many (P.oneOf " \r\f")
                        ; P.char '\n' <?> "end of string gap"
                        }

    -- escape codes
    escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl
                    <?> "escape code"

    charControl     = do{ _ <- P.char '^'
                        ; code <- P.upper
                        ; return (toEnum (fromEnum code - fromEnum 'A' + 1))
                        }

    charNum         = do{ code <- decimal
                                  <|> do{ _ <- P.char 'o'; number 8 P.octDigit }
                                  <|> do{ _ <- P.char 'x'; number 16 P.hexDigit }
                        ; if code > 0x10FFFF
                          then fail "invalid escape sequence"
                          else return (toEnum (fromInteger code))
                        }

    charEsc         = P.choice (map parseEsc escMap)
                    where
                      parseEsc (c,code)     = do{ _ <- P.char c; return code }

    charAscii       = P.choice (map parseAscii asciiMap)
                    where
                      parseAscii (asc,code) = P.try (do{ _ <- P.string asc; return code })


    -- escape code tables
    asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

    ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                       "FS","GS","RS","US","SP"]
    ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                       "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                       "CAN","SUB","ESC","DEL"]

    ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                       '\EM','\FS','\GS','\RS','\US','\SP']
    ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                       '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                       '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']


    number base baseDigit
        = do{ digits <- P.many1 baseDigit
            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            ; seq n (return n)
            }

---------------------------------------------------

parseELit :: Parser ELit
parseELit =  EL_Int    <$> (integer <|> hexadecimal <|> octal)
         <|> EL_Float  <$> float
         <|> EL_String <$> stringLit
         <|> EL_Char   <$> (question *> charLit)
         <?> "Literal"

charLit :: Parser Char
charLit = do
  c <- P.anyChar
  if c /= '\\'
  then return c
  else do
    next <- P.anyChar
    case L.lookup next escMap of
      Nothing -> fail "invalid escape sequence"
      Just r  -> return r

parseESExps :: Parser [ESExp]
parseESExps = parseESExp1 `P.sepBy` consform
           <?> "ELisp List of SExp"
  where consform = spaces *> P.optional (dot *> spaces)

parseDefun :: Parser ESExp
parseDefun = ES_Defun <$> (lexeme (P.string "defun") *> ident)
                      <*> parens (ident `P.sepBy` spaces)
                      <*> P.option Nothing (Just <$> stringLit)
                      <*> parseESExps

parseESExp1 :: Parser ESExp
parseESExp1 =  ES_Lit      <$> P.try parseELit
           <|> ES_Name     <$> P.try ident
           <|> parens (P.try parseDefun <|> (ES_List <$> parseESExps))
           <|> ES_Vector   <$> brackets parseESExps
           <|> ES_Quote    <$> (quote       *> parseESExp1)
           <|> ES_BQuote   <$> (backquote   *> parseESExp1)
           <|> ES_Comma    <$> P.try (comma *> P.notFollowedBy at *> parseESExp1)
           <|> ES_CommaAt  <$> (comma *> at *> parseESExp1)
           <?> "ELisp SExp"

parseModule :: Parser [ESExp]
parseModule = spaces *> parseESExp1 `P.sepBy1` spaces <* P.eof

--------------------------------------------------

test :: Parser a -> T.Text -> Either P.ParseError a
test p = P.parse p "<test>" 
           
parseString :: String -> Either P.ParseError [ESExp]
parseString str = P.parse parseModule "" (T.pack str)

parseFile :: FilePath -> IO (Either P.ParseError [ESExp])
parseFile file = P.parse parseModule file <$> T.readFile file

x = unlines $
   [ "defun async--transmit-sexp (process sexp)"
   , "  (with-temp-buffer"
   , "    (if async-debug"
   , "        (message \"Transmitting sexp {{{%s}}}\" (pp-to-string sexp)))"
   , "    (async--insert-sexp sexp)"
   , "    (process-send-region process (point-min) (point-max)))" ]

