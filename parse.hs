module Parse where
import Ast
import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.List hiding (all)
import Prelude hiding (all)
import System.IO
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language

languageDef =
  emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd     = "*/"
            , Token.commentLine    = "//"
            , Token.identStart     = alphaNum <|> oneOf "_"
            , Token.identLetter    = alphaNum <|> oneOf "+_-=,?/'`!@#$%^&*~"
            , Token.reservedNames  =
              [ "assert"
              , "retract"
              , "all"
              ]
              , Token.reservedOpNames = [".", ":", "*", "->", "&"]
            }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
whiteSpace = Token.whiteSpace lexer
semi = Token.semi lexer
braces = Token.braces lexer
brackets = Token.brackets lexer

simpleSymbol = identifier >>= \ id -> return $ Symbol id

complexSymbol = braces $ sepBy1 identifier spaces >>= \ ids -> return $ Symbol $ unwords ids

varSymbol = brackets $ sepBy1 identifier spaces >>= \ ids -> return $ Symbol $ "[" ++ unwords ids ++ "]"

symbol = try complexSymbol <|> simpleSymbol <|> varSymbol

chainedTerm = do 
  Symbol lhs <- symbol
  op <- char '.' <|> char ':'
  rhs <- term
  return $ 
    case op of
      '.' -> Dot lhs rhs
      ':' -> Is lhs rhs

term = whiteSpace >> (try chainedTerm <|> symbol)

andConj = do
  lhs <- term
  char '&'
  rhs <- term
  return $ And lhs rhs

hoistTerm = HoistTerm <$> term

conj = try andConj <|> hoistTerm

hoistConj = HoistConj <$> conj

arrow = do
  lhs <- conj
  reservedOp "->"
  rhs <- conj
  return $ Arrow lhs rhs

elExp = try arrow <|> hoistConj

assert = reserved "assert" >> Assert <$> elExp

retract = reserved "retract" >> Retract <$> elExp

all = reserved "all" >> All <$> elExp

query = Query <$> elExp

statement = assert <|> retract <|> all <|> query

program = sepBy1 statement semi

str p s = case parse p "" s of
    Left e  -> error $ show e
    Right r -> r

parseString str =
  case parse statement "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile file =
  do contents  <- readFile file
     case parse program "" contents of
       Left e  -> print e >> fail "parse error"
       Right r -> return r
