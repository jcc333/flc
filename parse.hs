module Parse where
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language

import Ast

languageDef =
  emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd     = "*/"
            , Token.commentLine    = "//"
            , Token.identStart     = letter
            , Token.identLetter    = alphaNum
            , Token.reservedNames  =
              [ "assert"
              , "retract"
              , "true"
              , "false"
              ]
              , Token.reservedOpNames = [".", "!", "*", "->", "&"]
            }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
whiteSpace = Token.whiteSpace lexer
semi = Token.semi lexer

symbol :: Parser Term
symbol = identifier >>= \ id -> return $ Symbol id

chainedTerm :: Parser Term
chainedTerm =
  do 
    lhs <- identifier
    op <- char '.' <|> char '!'
    rhs <- term
    return $ case op of
              '.' -> Dot lhs rhs
              '!' -> Bang lhs rhs

term :: Parser Term
term = try chainedTerm <|> symbol

andConj :: Parser Conj
andConj =
  do lhs <- term
     char '&'
     rhs <- term
     return $ And lhs rhs

hoistTerm :: Parser Conj
hoistTerm = term >>= \ t -> return $ HoistTerm t

conj :: Parser Conj
conj = try andConj <|> hoistTerm

hoistConj :: Parser Exp
hoistConj = conj >>= \ c -> return $ HoistConj c

arrow :: Parser Exp
arrow = 
  do lhs <- conj
     reservedOp "->"
     rhs <- conj
     return $ Arrow lhs rhs

elExp :: Parser Exp
elExp = try arrow <|> hoistConj
 
assert :: Parser Stmt
assert = reserved "assert" >> elExp >>= \ e -> return $ Assert e

retract :: Parser Stmt
retract = reserved "retract" >> elExp >>= \ e -> return $ Retract e

statement :: Parser Stmt
statement = assert
          <|> retract
          <|> (elExp >>= \ e -> return $ Query e)

program :: Parser [Stmt]
program = sepBy1 statement semi

str p s = case parse p "" s of
    Left e  -> error $ show e
    Right r -> r

parseString :: String -> [Stmt]
parseString str =
  case parse program "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO [Stmt]
parseFile file =
  do contents  <- readFile file
     case parse program "" contents of
       Left e  -> print e >> fail "parse error"
       Right r -> return r
