module Lexer where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

import Data.List (nub)

import qualified Utils as U

gimlLexer :: P.TokenParser ()
gimlLexer =
    P.makeTokenParser
    ( emptyDef
      { commentStart    = ""
      , commentEnd      = ""
      , commentLine     = "#"
      , reservedNames   = words $
                          " T TRUE F FALSE NA NULL c table" ++
                          " read.csv read.wsv write.wsv" ++
                          " if then else"
      , reservedOpNames = gimlOps
      , opStart         = oneOf . nub $ head gimlOps
      , opLetter        = oneOf . nub $ concatMap tail gimlOps
      , identStart      = letter <|> char '.'
      , identLetter     = alphaNum <|> oneOf "._"
      }
    )

gimlOps =  words "$ * / + ! % - == != < > <= >= <- -> : = [ ]"
        ++ joinOps
        ++ words "| & || &&"

joinOps =  "***" : U.combinations ["=*", "=", "=*"]

runLex :: Show a => Parser a -> String -> IO ()
runLex p input =
    (`parseTest` input) $ do
        whiteSpace
        x <- p
        eof
        return x

lexNumber = do
    sign <- option id (try (char '-') >> return negate)
    naturalOrFloat >>= return . sign . either fromInteger id

lexString =
    stringLiteral >>= return

lexBool =
    ( ( reservedWords "T TRUE" >> return True )
      <|>
      ( reservedWords "F FALSE" >> return False )
    )

reservedWords = choice . map reserved . words


braces         = glex P.braces
brackets       = glex P.brackets
charLiteral    = glex P.charLiteral
comma          = glex P.comma
commaSep       = glex P.commaSep
commaSep1      = glex P.commaSep1
float          = glex P.float
identifier     = glex P.identifier
integer        = glex P.integer
lexeme         = glex P.lexeme
natural        = glex P.natural
naturalOrFloat = glex P.naturalOrFloat
operator       = glex P.operator
parens         = glex P.parens
reserved       = glex P.reserved
reservedOp     = glex P.reservedOp
semi           = glex P.semi
semiSep        = glex P.semiSep
semiSep1       = glex P.semiSep1
squares        = glex P.squares
stringLiteral  = glex P.stringLiteral
symbol         = glex P.symbol
whiteSpace     = glex P.whiteSpace

glex f         = f gimlLexer
