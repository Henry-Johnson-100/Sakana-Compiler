module Parser.Core
  ( parse',
    generalParsePreserveError,
    getParseError,
    boolParser,
    stringParser,
    numParser,
    idParser,
    dataParser,
    genericKeywordParser,
    genericBracketParser,
    dataTokenParser,
    dataIdParser,
    keywordTokenParser,
    inBracketParser,
    keywordTreeParser,
    idTreeParser,
    dataTreeParser,
    lampreyParser,
    functionDefinitionParser,
    functionCallParser,
    swimParser,
    fishBindParser,
    shoalParser,
    expressionParser,
    globalStatementParser,
    docParser,
    generalParse,
  )
where

import qualified Control.Monad as CMonad
import qualified Data.Char as DChar
import qualified Data.Either as DEither
import qualified Data.Functor.Identity as DFId
import qualified Data.List as DList
import qualified Data.Maybe as DMaybe
{-
For Text.Parsec
Copyright 1999-2000, Daan Leijen; 2007, Paolo Martini. All rights reserved.
-}

import qualified Exception.Base as Exception
import qualified Parser.Syntax as Syntax
import Text.Parsec ((<?>), (<|>))
import qualified Text.Parsec as Prs
import qualified Text.Parsec.Error
import qualified Util.Classes as UC
import Util.General ((.<))
import qualified Util.General as UGen
import Util.Tree ((-<-), (-<=))
import qualified Util.Tree as Tree

type KeywordParser u = Prs.ParsecT [Char] u DFId.Identity Syntax.Keyword

type DataParser u = Prs.ParsecT [Char] u DFId.Identity Syntax.Data

type TokenParser u = Prs.ParsecT [Char] u DFId.Identity Syntax.Token

type TokenSourceParser u = Prs.ParsecT [Char] u DFId.Identity Syntax.TokenSource

type TreeParser u = Prs.ParsecT [Char] u DFId.Identity [Syntax.SyntaxTree]

----Data Parsers--------------------------------------------------------------------------
------------------------------------------------------------------------------------------

boolParser :: DataParser u
boolParser = do
  b <- Prs.string "True" <|> Prs.string "False"
  (return . Syntax.Boolean . readBool) b
  where
    readBool :: String -> Bool
    readBool = read

stringParser :: DataParser u
stringParser = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.char '"'
  string <-
    Prs.manyTill
      ( tryChoices
          [removeInvisibleSpacing, unescapeEscapedSpaces, anyCharAsString]
      )
      (Prs.char '"')
  (return . Syntax.String . concat) string
  where
    anyCharAsString :: Prs.ParsecT [Char] u DFId.Identity [Char]
    anyCharAsString = do
      ch <- Prs.anyChar
      return [ch]
    removeInvisibleSpacing :: Prs.ParsecT [Char] u DFId.Identity [Char]
    removeInvisibleSpacing = do
      tryChoices [Prs.tab, Prs.newline, Prs.crlf]
      return ""
    unescapeEscapedSpaces :: Prs.ParsecT [Char] u DFId.Identity [Char]
    unescapeEscapedSpaces =
      tryChoices
        [unescapeEscapedNewline, unescapeEscapedTab, unescapeEscapedCarriageReturn]
      where
        unescapeEscapedNewline :: Prs.ParsecT [Char] u DFId.Identity [Char]
        unescapeEscapedNewline = do
          Prs.string "\\n"
          return "\n"
        unescapeEscapedTab :: Prs.ParsecT [Char] u DFId.Identity [Char]
        unescapeEscapedTab = do
          Prs.string "\\t"
          return "\t"
        unescapeEscapedCarriageReturn :: Prs.ParsecT [Char] u DFId.Identity [Char]
        unescapeEscapedCarriageReturn = do
          Prs.string "\\r"
          return "\r"

numParser :: DataParser u
numParser = do
  maybeNegation <- (Prs.optionMaybe . Prs.char) '-'
  integerDigits <- Prs.many1 Prs.digit
  maybeDoubleSuffix <- Prs.optionMaybe doubleSuffix
  (return . Syntax.Num . readDouble) $
    composeNum maybeNegation integerDigits maybeDoubleSuffix
  where
    doubleSuffix :: Prs.ParsecT [Char] u DFId.Identity [Char]
    doubleSuffix = do
      decimal <- Prs.char '.'
      remainingDigits <- Prs.many1 Prs.digit
      return (decimal : remainingDigits)
    readDouble :: String -> Double
    readDouble = read
    composeNum :: Maybe Char -> [Char] -> Maybe [Char] -> [Char]
    composeNum mNeg intD mDSuf =
      let neg = DMaybe.maybe [] UGen.listSingleton mNeg
          doubleSuffix = DMaybe.fromMaybe [] mDSuf
       in neg ++ intD ++ doubleSuffix

-- | A special kind of DataParser, since ID's are not evaluable data types, this parser
-- is NOT included in the
--
-- > dataParser :: DataParser u
--
-- function.
idParser :: DataParser u
idParser = do
  accessorPrefixes <- (Prs.many . Prs.try) accessorIdParser
  baseName <- Prs.many1 validIdCharParser
  (return . Syntax.Id . (++) (concat accessorPrefixes)) baseName
  where
    validIdCharParser :: Prs.ParsecT [Char] u DFId.Identity Char
    validIdCharParser =
      tryChoices
        [ Prs.letter,
          Prs.oneOf "!@#$%^&*-=_+,<>/?;:|`~[]{}"
        ]
        <?> "valid ID character: alphanumeric character or symbolic character, \
            \excluding: \' . \' \" \\  \'"
    accessorIdParser :: Prs.ParsecT [Char] u DFId.Identity [Char]
    accessorIdParser = do
      accessorId <- Prs.many1 validIdCharParser
      dot <- Prs.char '.'
      (return . (++) accessorId) [dot]

dataParser :: DataParser u
dataParser =
  tryChoices [numParser, stringParser, boolParser]

----Keyword Parsers-----------------------------------------------------------------------
------------------------------------------------------------------------------------------

genericKeywordParser :: Syntax.Keyword -> KeywordParser u
genericKeywordParser k = do
  kStr <- (Prs.string . map DChar.toLower . show) k
  return k

----Token Parsers-------------------------------------------------------------------------
------------------------------------------------------------------------------------------

genericBracketParser :: Syntax.ScopeType -> Syntax.BracketTerminal -> TokenParser u
genericBracketParser st Syntax.Open = do
  stChar <- (Prs.char . Syntax.fromScopeType) st
  Prs.char '('
  return (Syntax.Bracket st Syntax.Open)
genericBracketParser st Syntax.Close = do
  Prs.char ')'
  stChar <- (Prs.char . Syntax.fromScopeType) st
  return (Syntax.Bracket st Syntax.Open)

dataTokenParser :: TokenParser u
dataTokenParser = do
  d <- dataParser
  (return . Syntax.Data) d

dataIdParser :: TokenParser u
dataIdParser = do
  identification <- idParser
  (return . Syntax.Data) identification

keywordTokenParser :: Syntax.Keyword -> TokenParser u
keywordTokenParser = (=<<) (return . Syntax.Keyword) . genericKeywordParser

----TokenSource Parsers-------------------------------------------------------------------
------------------------------------------------------------------------------------------

tokenInfoParser :: TokenParser u -> TokenSourceParser u
tokenInfoParser tp = do
  pos <- Prs.getPosition
  t <- tp
  let ln = Prs.sourceLine pos
  (return .< Syntax.Source) t ln

----Combinators and Util functions--------------------------------------------------------
------------------------------------------------------------------------------------------

-- | Apply try to a list of parser choices so that if a choice fails,
-- the next choice is tried as if no input has been consumed.
tryChoices ::
  [Prs.ParsecT [Char] u DFId.Identity a] -> Prs.ParsecT [Char] u DFId.Identity a
tryChoices = Prs.choice . (<$>) Prs.try

-- | Takes a parser and ignores the spaces before and after it.
stripSpaces ::
  Prs.ParsecT [Char] u DFId.Identity a -> Prs.ParsecT [Char] u DFId.Identity a
stripSpaces p = do
  Prs.spaces
  p
  Prs.spaces
  p

attachAllBranches :: Tree.Tree a -> [[Tree.Tree a]] -> Tree.Tree a
attachAllBranches = DList.foldl' (-<=)

infixl 9 -<*=

-- | Equal to attachAllBranches.
--
-- Strongly left associative.
--
-- > a -<*= b -<= c -<*= d
--
-- Is the same as
--
-- > ((a -<*= b) -<= c) -<*= d
(-<*=) :: Tree.Tree a -> [[Tree.Tree a]] -> Tree.Tree a
(-<*=) = attachAllBranches

inBracketParser :: Syntax.ScopeType -> TreeParser u -> TreeParser u
inBracketParser st tp = do
  (genericBracketParser st) Syntax.Open <?> ("an opening " ++ show st ++ " bracket")
  Prs.spaces
  parseStruct <- tp
  Prs.spaces
  (genericBracketParser st) Syntax.Close <?> ("a closing " ++ show st ++ " bracket")
  Prs.spaces
  return parseStruct

----Tree Parsers--------------------------------------------------------------------------
------------------------------------------------------------------------------------------

keywordTreeParser :: Syntax.Keyword -> Syntax.ScopeType -> TreeParser u
keywordTreeParser k st = do
  keywordSource <- (tokenInfoParser . keywordTokenParser) k
  let keywordTree = (Tree.tree . Syntax.sourceToSyntaxUnit keywordSource) st
  return [keywordTree]

idTreeParser :: Syntax.ScopeType -> TreeParser u
idTreeParser st = do
  identification <- (tokenInfoParser) dataIdParser
  (return . UGen.listSingleton . Tree.tree . Syntax.sourceToSyntaxUnit identification) st

dataTreeParser :: Syntax.ScopeType -> TreeParser u
dataTreeParser st = do
  d <- (tokenInfoParser) dataTokenParser
  (return . UGen.listSingleton . Tree.tree . Syntax.sourceToSyntaxUnit d) st

-- #TODO
nullBracketParser :: Syntax.ScopeType -> TreeParser u
nullBracketParser st = do
  (genericBracketParser st) Syntax.Open
  Prs.spaces
  (genericBracketParser st) Syntax.Close
  return [UC.defaultValue]

lampreyParser :: Syntax.ScopeType -> TreeParser u
lampreyParser st = do
  implicitKeyword <-
    (Prs.optionMaybe . tokenInfoParser . keywordTokenParser) Syntax.Lamprey
  Prs.spaces
  paramsOrOther <-
    ( Prs.many
        . inBracketParser Syntax.Send
        . lampreyParameterParser
      )
      Syntax.Send
      <?> "valid identifiers, or function definitions"
  Prs.spaces
  value <-
    (inBracketParser Syntax.Return . expressionParser) Syntax.Return
      <?> "a return value expression."
  Prs.spaces
  let lampreyLn = (Syntax.line . DMaybe.fromJust . (=<<) Tree.treeNode . UGen.head') value
      genericLamprey =
        Syntax.SyntaxUnit (Syntax.Keyword Syntax.Lamprey) lampreyLn st
      justLampreyTree =
        ( Tree.tree
            . DMaybe.maybe
              (genericLamprey)
              (flip Syntax.sourceToSyntaxUnit st)
        )
          implicitKeyword
      lampreyTree =
        justLampreyTree -<*= paramsOrOther -<= value
  return [lampreyTree]
  where
    lampreyParameterParser :: Syntax.ScopeType -> TreeParser u
    lampreyParameterParser st =
      tryChoices
        [ functionDefinitionParser st,
          idTreeParser st
        ]

functionDefinitionParser :: Syntax.ScopeType -> TreeParser u
functionDefinitionParser st = do
  fish <- (tokenInfoParser . keywordTokenParser) Syntax.Fish
  Prs.spaces
  functionId <- (idTreeParser) st
  Prs.spaces
  assocLamprey <- (lampreyParser) Syntax.Return
  Prs.spaces
  let fishTR = (Tree.tree . Syntax.sourceToSyntaxUnit fish) st
      idTR = (DMaybe.fromJust . UGen.head') functionId
      fishTree = fishTR -<- (idTR -<= assocLamprey)
  return [fishTree]

functionCallParser :: Syntax.ScopeType -> TreeParser u
functionCallParser st = do
  functionCallId <- (idTreeParser) st
  Prs.spaces
  arguments <-
    (Prs.many . inBracketParser Syntax.Send . expressionParser) Syntax.Send
  Prs.spaces
  let functionCallTree = ((DMaybe.fromJust . UGen.head') functionCallId) -<*= arguments
  return [functionCallTree]

swimParser :: Syntax.ScopeType -> TreeParser u
swimParser st = do
  swimKeyword <- (keywordTreeParser Syntax.Swim) st
  Prs.spaces
  inSendContext <-
    (Prs.many . inBracketParser Syntax.Send) eitherFishBindOrExpr
  Prs.spaces
  returnValue <-
    (inBracketParser Syntax.Return . expressionParser) Syntax.Return
  Prs.spaces
  let swimTreeHead = head swimKeyword
      swimTree = swimTreeHead -<*= inSendContext -<= returnValue
  return [swimTree]
  where
    eitherFishBindOrExpr :: TreeParser u
    eitherFishBindOrExpr =
      -- This is order dependent and I would like it not to be,
      -- but since this is the only place a fishBind can appear,
      -- I guess it's okay for it to be first.
      -- If it's not in this order, the parser will fail.
      tryChoices [fishBindParser, expressionParser Syntax.Send]

fishBindParser :: TreeParser u
fishBindParser = do
  bindId <- (idTreeParser) Syntax.Send
  Prs.spaces
  bindingExpression <-
    (inBracketParser Syntax.Return . expressionParser) Syntax.Return
  Prs.spaces
  let bindIdTr = head bindId
      fishBindTree = bindIdTr -<= bindingExpression
  return [fishBindTree]

shoalParser :: Syntax.ScopeType -> TreeParser u
shoalParser st = do
  shoalKeyword <- (keywordTreeParser Syntax.Shoal) st
  shoalMembers <-
    (Prs.many . inBracketParser Syntax.Send . idTreeParser) Syntax.Send
  let shoalTree = (head shoalKeyword) -<*= shoalMembers
  return [shoalTree]

expressionParser :: Syntax.ScopeType -> TreeParser u
expressionParser st =
  tryChoices
    [ lampreyParser st,
      dataTreeParser st,
      functionCallParser st,
      swimParser st
    ]

globalStatementParser :: TreeParser u
globalStatementParser =
  tryChoices
    [functionDefinitionParser Syntax.Return, shoalParser Syntax.Return]

----Parse---------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

docParser :: Prs.ParsecT [Char] u DFId.Identity Syntax.SyntaxTree
docParser = do
  globalStatements <- Prs.many globalStatementParser
  let mainTree =
        Tree.tree (UC.defaultValue {Syntax.token = Syntax.Data (Syntax.Id "main")})
      docTree = mainTree -<*= globalStatements
  return docTree

generalParse ::
  Eq a => Prs.ParsecT [Char] () DFId.Identity a -> Prs.SourceName -> String -> a
generalParse p srcName src =
  DEither.either
    (Exception.raiseError . flip getParseError src)
    id
    (Prs.parse p srcName src)

generalParsePreserveError ::
  Prs.ParsecT [Char] () DFId.Identity a ->
  Prs.SourceName ->
  String ->
  Either Prs.ParseError a
generalParsePreserveError parser srcName src = Prs.parse parser srcName src

getParseError :: Prs.ParseError -> String -> Exception.Exception
getParseError prsErr srcStr =
  let errLine = (Prs.sourceLine . Prs.errorPos) prsErr
      errColumn = (Prs.sourceColumn . Prs.errorPos) prsErr
      lineContext = (showWindow (errColumn) . flip (!!) (errLine - 1) . lines) srcStr
      errMsg = unlines ["\n", show prsErr, lineContext]
   in Exception.newException Exception.FailedToParse [errLine] errMsg Exception.Fatal

parse' :: Prs.SourceName -> [Char] -> Either Prs.ParseError Syntax.SyntaxTree
parse' srcName src = Prs.parse docParser srcName src

----Reporting-----------------------------------------------------------------------------
------------------------------------------------------------------------------------------

showWindow :: Int -> String -> String
showWindow n = take windowSize . drop (((-) n . div windowSize) 2)
  where
    windowSize = 30