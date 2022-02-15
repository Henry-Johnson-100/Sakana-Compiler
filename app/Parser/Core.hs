{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Use <&>" #-}
module Parser.Core
  ( generalParse,
    generalParsePreserveError,
    valLiteralStringParser,
    valLiteralBoolParser,
    valLiteralCharParser,
    valLiteralDoubleParser,
    valLiteralIntegerParser,
    valLiteralListParser,
    sknValLiteralParser,
    sknKeywordParser,
    sknFlagParser,
    sknIdParser,
    sknTokenBracketParser,
    sknTokenDataParser,
    typeLiteralPrimitiveParser,
  )
where

import qualified Control.Monad as CMonad
import qualified Data.Char as Char
import qualified Data.Either as DEither
import qualified Data.Functor.Identity as DFId
import qualified Data.List
import qualified Data.Maybe as Maybe
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

type CharStreamParser a u = Prs.ParsecT [Char] u DFId.Identity a

type StringParser u = CharStreamParser String u

type SknDataParser u = CharStreamParser Syntax.SknData u

type SknValLiteralParser u = CharStreamParser Syntax.SknValLiteral u

type SknTypeLiteralParser u = CharStreamParser Syntax.SknType u

type SknKeywordParser u = CharStreamParser Syntax.SknKeyword u

type SknFlagParser u = CharStreamParser Syntax.SknFlag u

-- type SknIdParser u = CharStreamParser Syntax.SknId u

type SknTokenParser u = CharStreamParser Syntax.SknToken u

type SknTokenStreamUnitParser u = CharStreamParser Syntax.TokenStreamUnit u

type SknSyntaxUnitParser u = CharStreamParser Syntax.SknSyntaxUnit u

type SknTreeParser u = CharStreamParser [Tree.Tree Syntax.SknSyntaxUnit] u

type SknLabeledTreeParser u =
  CharStreamParser [Syntax.LabeledTree Syntax.SknSyntaxUnit] u

----Value Literal Parsers-----------------------------------------------------------------
------------------------------------------------------------------------------------------
valLiteralBoolParser :: SknValLiteralParser u
valLiteralBoolParser = do
  b <- Prs.string "True" <|> Prs.string "False"
  (return . Syntax.SknVBool . readBool) b
  where
    readBool :: String -> Bool
    readBool = read

valLiteralCharParser :: SknValLiteralParser u
valLiteralCharParser = do
  Prs.char '\''
  char <- Prs.anyChar
  Prs.char '\''
  (return . Syntax.SknVChar) char

valLiteralStringParser :: SknValLiteralParser u
valLiteralStringParser = do
  Prs.char '"'
  string <-
    Prs.manyTill
      ( tryChoices
          [removeInvisibleSpacing, unescapeEscapedSpaces, anyCharAsString]
      )
      (Prs.char '"')
  (return . Syntax.SknVString . concat) string
  where
    anyCharAsString :: CharStreamParser String u
    anyCharAsString = do
      ch <- Prs.anyChar
      return [ch]
    removeInvisibleSpacing :: CharStreamParser String u
    removeInvisibleSpacing = do
      tryChoices [Prs.tab, Prs.newline, Prs.crlf]
      return ""
    unescapeEscapedSpaces :: CharStreamParser String u
    unescapeEscapedSpaces =
      tryChoices
        [unescapeEscapedNewline, unescapeEscapedTab, unescapeEscapedCarriageReturn]
      where
        unescapeEscapedNewline :: CharStreamParser String u
        unescapeEscapedNewline = do
          Prs.string "\\n"
          return "\n"
        unescapeEscapedTab :: CharStreamParser String u
        unescapeEscapedTab = do
          Prs.string "\\t"
          return "\t"
        unescapeEscapedCarriageReturn :: CharStreamParser String u
        unescapeEscapedCarriageReturn = do
          Prs.string "\\r"
          return "\r"

literalNegationParser :: CharStreamParser (Maybe.Maybe Char) u
literalNegationParser = (Prs.optionMaybe . Prs.char) '-'

valLiteralIntegerParser :: SknValLiteralParser u
valLiteralIntegerParser = do
  maybeNegation <- literalNegationParser
  digits <- Prs.many1 Prs.digit
  let integerString = Maybe.maybe digits (flip (:) digits) maybeNegation
  (return . Syntax.SknVInteger . read) integerString

valLiteralDoubleParser :: SknValLiteralParser u
valLiteralDoubleParser = do
  maybeNegation <- (Prs.optionMaybe . Prs.char) '-'
  integerDigits <- Prs.many1 Prs.digit
  doubleDigits <- doubleSuffix
  ( return . Syntax.SknVDouble . read
      . (\x -> Maybe.maybe x (flip (:) x) maybeNegation)
    )
    (integerDigits ++ doubleDigits)
  where
    doubleSuffix :: CharStreamParser String u
    doubleSuffix = do
      decimal <- Prs.char '.'
      remainingDigits <- Prs.many1 Prs.digit
      return (decimal : remainingDigits)

valLiteralPrimitiveParser :: SknValLiteralParser u
valLiteralPrimitiveParser =
  tryChoices
    [ valLiteralBoolParser,
      valLiteralCharParser,
      valLiteralDoubleParser,
      valLiteralIntegerParser,
      valLiteralStringParser,
      sknIdParser
    ]

valLiteralListParser :: SknValLiteralParser u
valLiteralListParser = do
  Prs.char '['
  Prs.spaces
  maybeListContents <- Prs.optionMaybe listContentParser
  Prs.spaces
  Prs.char ']'
  (return . Syntax.SknVList . Maybe.fromMaybe []) maybeListContents
  where
    listContentParser :: CharStreamParser [Syntax.SknValLiteral] u
    listContentParser = do
      Prs.spaces
      headItem <- sknValLiteralParser
      tailItems <- Prs.many listRetroContentParser
      let listContents = headItem : tailItems
      return listContents
    listRetroContentParser :: SknValLiteralParser u
    listRetroContentParser = do
      Prs.spaces
      Prs.char ','
      Prs.spaces
      literal <- sknValLiteralParser
      Prs.spaces
      return literal

sknValLiteralParser :: SknValLiteralParser u
sknValLiteralParser =
  tryChoices [valLiteralPrimitiveParser, valLiteralListParser]
    <?> "a literal value e.g. 1, True, \"><>\", \'A\', 50.4868675"

----Id Parser-----------------------------------------------------------------------------
------------------------------------------------------------------------------------------

sknIdPrimitiveParser :: CharStreamParser String u
sknIdPrimitiveParser = do
  idHead <-
    Prs.letter <|> validIdSymbolParser
      <?> "letter or symbol, excluding parentheses, square brackets, or quotations."
  idTail <-
    Prs.many
      (Prs.alphaNum <|> validIdSymbolParser)
      <?> "alphanumeric character or\
          \ symbol excluding parentheses, square brackets, or quotations."
  return (idHead : idTail)
  where
    validIdSymbolParser :: CharStreamParser Char u
    validIdSymbolParser = Prs.oneOf "!@#$%^&*-=_+,<>/?;:|`~{}"

sknIdParser :: SknValLiteralParser u
sknIdParser = do
  idHead <- sknIdPrimitiveParser
  accessees <- Prs.many postAccessorParser
  (return . Syntax.SknVId . (++) idHead . concat) accessees
  where
    postAccessorParser :: CharStreamParser String u
    postAccessorParser = do
      dot <- Prs.char '.'
      accessee <- sknIdPrimitiveParser
      return (dot : accessee)

----Type Literal Parsers------------------------------------------------------------------
------------------------------------------------------------------------------------------

sknTIntegerParser :: SknTypeLiteralParser u
sknTIntegerParser = Prs.string "Integer" >> return Syntax.SknTInteger

skntTDoubleParser :: SknTypeLiteralParser u
skntTDoubleParser = Prs.string "Double" >> return Syntax.SknTDouble

sknTCharParser :: SknTypeLiteralParser u
sknTCharParser = Prs.string "Char" >> return Syntax.SknTChar

sknTStringParser :: SknTypeLiteralParser u
sknTStringParser = Prs.string "String" >> return Syntax.SknTString

sknTBoolParser :: SknTypeLiteralParser u
sknTBoolParser = Prs.string "Bool" >> return Syntax.SknTBool

sknTVarParser :: SknTypeLiteralParser u
sknTVarParser = fmap (Syntax.SknTVar . Maybe.fromJust . Syntax.unId) sknIdParser

typeLiteralPrimitiveParser :: SknTypeLiteralParser u
typeLiteralPrimitiveParser =
  tryChoices
    [ sknTIntegerParser,
      skntTDoubleParser,
      sknTCharParser,
      sknTStringParser,
      sknTBoolParser,
      sknTVarParser
    ]

-- sknTypeLiteralParser :: SknTypeLiteralParser u
-- sknTypeLiteralParser = do
--   typeLiteralPrimitiveParser

----Keyword and Flag Parsers--------------------------------------------------------------
------------------------------------------------------------------------------------------

sknKeywordParser :: Syntax.SknKeyword -> SknKeywordParser u
sknKeywordParser k = do
  (Prs.string . showKeyword) k <?> ("a keyword: \"" ++ showKeyword k ++ "\".")
  return k
  where
    showKeyword :: Syntax.SknKeyword -> String
    showKeyword = map Char.toLower . show

-- | Could cause some issues down the line.
sknFlagParser :: SknFlagParser u
sknFlagParser = do
  flagStr <-
    (tryChoices . map (Prs.string . show) . enumFrom) Syntax.Impure
  (return . read) flagStr

----SknToken Parsers---------------------------------------------------------------------
------------------------------------------------------------------------------------------

sknTokenBracketParser ::
  Syntax.SknBracketType ->
  Syntax.SknScopeType ->
  Syntax.SknBracketTerminal ->
  SknTokenParser u
sknTokenBracketParser bt st Syntax.Open = do
  Prs.char (if st == Syntax.Send then '>' else '<')
  Prs.char (if bt == Syntax.Value then '(' else ':')
  (return . Syntax.SknTokenBracket) (Syntax.SknBracket bt st Syntax.Open)
sknTokenBracketParser bt st Syntax.Close = do
  Prs.char (if bt == Syntax.Value then ')' else ':')
  Prs.char (if st == Syntax.Send then '>' else '<')
  (return . Syntax.SknTokenBracket) (Syntax.SknBracket bt st Syntax.Close)

-- #TEST
sknTokenDataParser :: SknTokenParser u
sknTokenDataParser = fmap (Syntax.SknTokenData . Syntax.sknData) sknValLiteralParser

-- #TEST
sknTokenKeywordParser :: Syntax.SknKeyword -> SknTokenParser u
sknTokenKeywordParser = fmap Syntax.SknTokenKeyword . sknKeywordParser

-- #TEST
sknTokenFlagParser :: SknTokenParser u
sknTokenFlagParser = fmap Syntax.SknTokenFlag sknFlagParser

-- #TEST
sknTokenIdParser :: SknTokenParser u
sknTokenIdParser = fmap (Syntax.SknTokenData . Syntax.sknData) sknIdParser

-- #TEST
sknPrimitiveTokenTypeLiteralParser :: SknTokenParser u
sknPrimitiveTokenTypeLiteralParser =
  fmap Syntax.SknTokenTypeLiteral typeLiteralPrimitiveParser

----TokenStreamUnit Parsers--------------------------------------------------------------
------------------------------------------------------------------------------------------

-- #TEST

-- | Lift an SknTokenParser to an SknTokenStreamUnitParser
sknTokenStreamUnitParser :: SknTokenParser u -> SknTokenStreamUnitParser u
sknTokenStreamUnitParser stp = do
  pos <- Prs.getPosition
  t <- stp
  let tokenStreamUnit =
        CMonad.liftM2 (Syntax.StreamUnit t) Prs.sourceLine Prs.sourceColumn pos
  return tokenStreamUnit

----SknSyntaxUnit Parsers-----------------------------------------------------------------
------------------------------------------------------------------------------------------

-- #TEST

-- | Lift an SknTokenStreamUnitParser, with a ScopeType, to an SknSyntaxUnitParser
sknSyntaxUnitParser ::
  SknTokenStreamUnitParser u -> Syntax.SknScopeType -> SknSyntaxUnitParser u
sknSyntaxUnitParser = flip (fmap . Syntax.tsutosu)

-- #TEST
suLiftTokenParser :: SknTokenParser u -> Syntax.SknScopeType -> SknSyntaxUnitParser u
suLiftTokenParser tp = sknSyntaxUnitParser (sknTokenStreamUnitParser tp)

----SknTree Parsers-----------------------------------------------------------------------
------------------------------------------------------------------------------------------

-- #TEST
sknTreeKeywordParser :: Syntax.SknKeyword -> Syntax.SknScopeType -> SknTreeParser u
sknTreeKeywordParser k st = do
  k <- suLiftTokenParser (sknTokenKeywordParser k) st
  return [Tree.tree k]

-- #TEST
sknIdTreeParser :: Syntax.SknScopeType -> SknTreeParser u
sknIdTreeParser st = do
  id' <- suLiftTokenParser sknTokenIdParser st
  return [Tree.tree id']

-- #TEST
sknDataTreeParser :: Syntax.SknScopeType -> SknTreeParser u
sknDataTreeParser st = do
  d <- suLiftTokenParser sknTokenDataParser st
  return [Tree.tree d]

-- #TEST
sknTreeInBracketToLabeledTree ::
  Syntax.SknBracketType ->
  Syntax.SknScopeType ->
  Syntax.SknStaticTreeLabel ->
  SknTreeParser u ->
  SknLabeledTreeParser u
sknTreeInBracketToLabeledTree bt st label p = do
  sknTokenBracketParser bt st Syntax.Open
  Prs.spaces
  parseTrees <- p
  Prs.spaces
  sknTokenBracketParser bt st Syntax.Close
  Prs.spaces
  (return . map (Syntax.liftTreeWithLabel label)) parseTrees

-- #TEST
sknLabeledTreeInBracket ::
  Syntax.SknBracketType ->
  Syntax.SknScopeType ->
  SknLabeledTreeParser u ->
  SknLabeledTreeParser u
sknLabeledTreeInBracket bt st p = do
  sknTokenBracketParser bt st Syntax.Open
  Prs.spaces
  labeledTree <- p
  Prs.spaces
  sknTokenBracketParser bt st Syntax.Close
  Prs.spaces
  return labeledTree

----SknLabeledTree Parsers----------------------------------------------------------------
------------------------------------------------------------------------------------------

-- #TEST
liftTreeParserToLabeledTreeParser ::
  Syntax.SknStaticTreeLabel -> SknTreeParser u -> SknLabeledTreeParser u
liftTreeParserToLabeledTreeParser = fmap . map . Syntax.liftTreeWithLabel

-- sknLampreyExprParser :: Syntax.SknScopeType -> SknLabeledTreeParser u
-- sknLampreyExprParser st = do
--   implicitKeyword <- Prs.optionMaybe (sknTreeKeywordParser Syntax.Lamprey st)
--   paramsOrOther <- (Prs.many)
--   Prs.spaces
--   return []
--   where
--     lampreyParameterParser :: Syntax.SknScopeType -> SknLabeledTreeParser u
--     lampreyParameterParser st =
--       return []

-- #TEST
-- #TODO
lampreyParser :: Syntax.SknScopeType -> SknLabeledTreeParser u
lampreyParser st = do
  implicitKeyword <- Prs.optionMaybe (sknTreeKeywordParser Syntax.Lamprey st)
  Prs.spaces
  lampreyParams <-
    ( Prs.many
        . sknLabeledTreeInBracket Syntax.Value Syntax.Send
      )
      (lampreyParameterParser Syntax.Send)
      <?> "valid identifiers, function definitions or value literals."
  Prs.spaces
  value <-
    sknLabeledTreeInBracket Syntax.Value Syntax.Return (expressionParser Syntax.Return)
      <?> "an expression returing a value."
  Prs.spaces
  let lampreySU = getLampreyKeyword implicitKeyword value st
      lampreyExprTree =
        [ Syntax.liftTreeWithLabel Syntax.LampreyExpr (Tree.tree lampreySU)
            -<** lampreyParams
            -<** [value]
        ]
  return lampreyExprTree
  where
    getLampreyKeyword ::
      Maybe.Maybe [Tree.Tree Syntax.SknSyntaxUnit] ->
      [Syntax.LabeledTree Syntax.SknSyntaxUnit] ->
      Syntax.SknScopeType ->
      Syntax.SknSyntaxUnit
    getLampreyKeyword implicitKeyword' alternativeLineSource st' =
      let altLampreyLocation =
            Maybe.fromMaybe
              0
              ( UGen.head' alternativeLineSource
                  >>= Syntax.labeledTreeNode
                  >>= (return . Syntax.sknUnitLine)
              )
       in Maybe.fromMaybe
            ( Syntax.SknSyntaxUnit
                (Syntax.SknTokenKeyword Syntax.Lamprey)
                altLampreyLocation
                st'
            )
            (implicitKeyword' >>= UGen.head' >>= Tree.treeNode)

-- #TEST

-- | Lamprey parameters can have function definitions, value ID's or literal values.
-- #TODO Add a parser for a piscis in the id tree parser option.
lampreyParameterParser :: Syntax.SknScopeType -> SknLabeledTreeParser u
lampreyParameterParser st =
  tryChoices
    [ sknFunctionDefStatementParser st,
      sknIdCallWithTypeAnnotation st,
      (liftTreeParserToLabeledTreeParser Syntax.Literal . sknDataTreeParser) st
    ]

-- #TEST
-- #TODO
sknIdCallWithTypeAnnotation :: Syntax.SknScopeType -> SknLabeledTreeParser u
sknIdCallWithTypeAnnotation st = do
  return []

-- #TEST
-- #TODO
typeAnnotationParser :: SknLabeledTreeParser u
typeAnnotationParser = return []

-- #TEST
-- #TODO
sknFunctionDefStatementParser :: Syntax.SknScopeType -> SknLabeledTreeParser u
sknFunctionDefStatementParser st = return []

-- #TEST
-- #TODO
expressionParser :: Syntax.SknScopeType -> SknLabeledTreeParser u
expressionParser st = tryChoices [lampreyParser st]

-- piscisDefParser :: SknTreeParser u
-- piscisDefParser = return (Syntax.SknTree Syntax.Literal UC.defaultValue)

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

-- ----Keyword Parsers-----------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------

-- ----Token Parsers-------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------

-- genericBracketParser :: Syntax.ScopeType -> Syntax.BracketTerminal -> TokenParser u
-- genericBracketParser st Syntax.Open = do
--   stChar <- (Prs.char . Syntax.fromScopeType) st
--   Prs.char '('
--   return (Syntax.Bracket st Syntax.Open)
-- genericBracketParser st Syntax.Close = do
--   Prs.char ')'
--   stChar <- (Prs.char . Syntax.fromScopeType) st
--   return (Syntax.Bracket st Syntax.Open)

-- dataTokenParser :: TokenParser u
-- dataTokenParser = do
--   d <- dataParser
--   (return . Syntax.Data) d

-- dataIdParser :: TokenParser u
-- dataIdParser = do
--   identification <- idParser
--   (return . Syntax.Data) identification

-- keywordTokenParser :: Syntax.Keyword -> TokenParser u
-- keywordTokenParser = (=<<) (return . Syntax.Keyword) . genericKeywordParser

-- ----TokenSource Parsers-------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------

-- tokenInfoParser :: TokenParser u -> TokenSourceParser u
-- tokenInfoParser tp = do
--   pos <- Prs.getPosition
--   t <- tp
--   let ln = Prs.sourceLine pos
--   (return .< Syntax.Source) t ln

-- ----Combinators and Util functions--------------------------------------------------------
-- ------------------------------------------------------------------------------------------

-- | Apply try to a list of parser choices so that if a choice fails,
-- the next choice is tried as if no input has been consumed.
tryChoices ::
  [Prs.ParsecT [Char] u DFId.Identity a] -> Prs.ParsecT [Char] u DFId.Identity a
tryChoices = Prs.choice . (<$>) Prs.try

allSpaces :: CharStreamParser String u
allSpaces =
  Prs.many $ tryChoices [Prs.char '\n', Prs.char '\t', Prs.char '\r', Prs.char ' ']

infixl 9 -<**

(-<**) :: Syntax.LabeledTree a -> [[Syntax.LabeledTree a]] -> Syntax.LabeledTree a
(-<**) = Data.List.foldl' attachLabeledTreeBranch
  where
    attachLabeledTreeBranch ::
      Syntax.LabeledTree a -> [Syntax.LabeledTree a] -> Syntax.LabeledTree a
    attachLabeledTreeBranch Syntax.EmptyLabeledTree _ = Syntax.EmptyLabeledTree
    attachLabeledTreeBranch (Syntax.LabeledTree label n trs) xs =
      Syntax.LabeledTree label n (trs ++ xs)

-- -- | Takes a parser and ignores the spaces before and after it.
-- stripSpaces ::
--   Prs.ParsecT [Char] u DFId.Identity a -> Prs.ParsecT [Char] u DFId.Identity a
-- stripSpaces p = do
--   Prs.spaces
--   p
--   Prs.spaces
--   p

-- attachAllBranches :: Tree.Tree a -> [[Tree.Tree a]] -> Tree.Tree a
-- attachAllBranches = DList.foldl' (-<=)

-- infixl 9 -<*=

-- -- | Equal to attachAllBranches.
-- --
-- -- Strongly left associative.
-- --
-- -- > a -<*= b -<= c -<*= d
-- --
-- -- Is the same as
-- --
-- -- > ((a -<*= b) -<= c) -<*= d
-- (-<*=) :: Tree.Tree a -> [[Tree.Tree a]] -> Tree.Tree a
-- (-<*=) = attachAllBranches

-- inBracketParser :: Syntax.ScopeType -> TreeParser u -> TreeParser u
-- inBracketParser st tp = do
--   (genericBracketParser st) Syntax.Open <?> ("an opening " ++ show st ++ " bracket")
--   Prs.spaces
--   parseStruct <- tp
--   Prs.spaces
--   (genericBracketParser st) Syntax.Close <?> ("a closing " ++ show st ++ " bracket")
--   Prs.spaces
--   return parseStruct

-- ----Tree Parsers--------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------

-- keywordTreeParser :: Syntax.Keyword -> Syntax.ScopeType -> TreeParser u
-- keywordTreeParser k st = do
--   keywordSource <- (tokenInfoParser . keywordTokenParser) k
--   let keywordTree = (Tree.tree . Syntax.sourceToSyntaxUnit keywordSource) st
--   return [keywordTree]

-- idTreeParser :: Syntax.ScopeType -> TreeParser u
-- idTreeParser st = do
--   identification <- (tokenInfoParser) dataIdParser
--   (return . UGen.listSingleton . Tree.tree . Syntax.sourceToSyntaxUnit identification) st

-- dataTreeParser :: Syntax.ScopeType -> TreeParser u
-- dataTreeParser st = do
--   d <- (tokenInfoParser) dataTokenParser
--   (return . UGen.listSingleton . Tree.tree . Syntax.sourceToSyntaxUnit d) st

-- -- #TODO
-- nullBracketParser :: Syntax.ScopeType -> TreeParser u
-- nullBracketParser st = do
--   (genericBracketParser st) Syntax.Open
--   Prs.spaces
--   (genericBracketParser st) Syntax.Close
--   return [UC.defaultValue]

-- lampreyParser :: Syntax.ScopeType -> TreeParser u
-- lampreyParser st = do
--   implicitKeyword <-
--     (Prs.optionMaybe . tokenInfoParser . keywordTokenParser) Syntax.Lamprey
--   Prs.spaces
--   paramsOrOther <-
--     ( Prs.many
--         . inBracketParser Syntax.Send
--         . lampreyParameterParser
--       )
--       Syntax.Send
--       <?> "valid identifiers, or function definitions"
--   Prs.spaces
--   value <-
--     (inBracketParser Syntax.Return . expressionParser) Syntax.Return
--       <?> "a return value expression."
--   Prs.spaces
--   let lampreyLn = (Syntax.line . DMaybe.fromJust . (=<<) Tree.treeNode . UGen.head') value
--       genericLamprey =
--         Syntax.SyntaxUnit (Syntax.Keyword Syntax.Lamprey) lampreyLn st
--       justLampreyTree =
--         ( Tree.tree
--             . DMaybe.maybe
--               (genericLamprey)
--               (flip Syntax.sourceToSyntaxUnit st)
--         )
--           implicitKeyword
--       lampreyTree =
--         justLampreyTree -<*= paramsOrOther -<= value
--   return [lampreyTree]
--   where
--     lampreyParameterParser :: Syntax.ScopeType -> TreeParser u
--     lampreyParameterParser st =
--       tryChoices
--         [ functionDefinitionParser st,
--           idTreeParser st
--         ]

-- functionDefinitionParser :: Syntax.ScopeType -> TreeParser u
-- functionDefinitionParser st = do
--   fish <- (tokenInfoParser . keywordTokenParser) Syntax.Fish
--   Prs.spaces
--   functionId <- (idTreeParser) st
--   Prs.spaces
--   assocLamprey <- (lampreyParser) Syntax.Return
--   Prs.spaces
--   let fishTR = (Tree.tree . Syntax.sourceToSyntaxUnit fish) st
--       idTR = (DMaybe.fromJust . UGen.head') functionId
--       fishTree = fishTR -<- (idTR -<= assocLamprey)
--   return [fishTree]

-- functionCallParser :: Syntax.ScopeType -> TreeParser u
-- functionCallParser st = do
--   functionCallId <- (idTreeParser) st
--   Prs.spaces
--   arguments <-
--     (Prs.many . inBracketParser Syntax.Send . expressionParser) Syntax.Send
--   Prs.spaces
--   let functionCallTree = ((DMaybe.fromJust . UGen.head') functionCallId) -<*= arguments
--   return [functionCallTree]

-- swimParser :: Syntax.ScopeType -> TreeParser u
-- swimParser st = do
--   swimKeyword <- (keywordTreeParser Syntax.Swim) st
--   Prs.spaces
--   inSendContext <-
--     (Prs.many . inBracketParser Syntax.Send) eitherFishBindOrExpr
--   Prs.spaces
--   returnValue <-
--     (inBracketParser Syntax.Return . expressionParser) Syntax.Return
--   Prs.spaces
--   let swimTreeHead = head swimKeyword
--       swimTree = swimTreeHead -<*= inSendContext -<= returnValue
--   return [swimTree]
--   where
--     eitherFishBindOrExpr :: TreeParser u
--     eitherFishBindOrExpr =
--       -- This is order dependent and I would like it not to be,
--       -- but since this is the only place a fishBind can appear,
--       -- I guess it's okay for it to be first.
--       -- If it's not in this order, the parser will fail.
--       tryChoices [fishBindParser, expressionParser Syntax.Send]

-- fishBindParser :: TreeParser u
-- fishBindParser = do
--   bindId <- (idTreeParser) Syntax.Send
--   Prs.spaces
--   bindingExpression <-
--     (inBracketParser Syntax.Return . expressionParser) Syntax.Return
--   Prs.spaces
--   let bindIdTr = head bindId
--       fishBindTree = bindIdTr -<= bindingExpression
--   return [fishBindTree]

-- shoalParser :: Syntax.ScopeType -> TreeParser u
-- shoalParser st = do
--   shoalKeyword <- (keywordTreeParser Syntax.Shoal) st
--   shoalMembers <-
--     (Prs.many . inBracketParser Syntax.Send . idTreeParser) Syntax.Send
--   let shoalTree = (head shoalKeyword) -<*= shoalMembers
--   return [shoalTree]

-- expressionParser :: Syntax.ScopeType -> TreeParser u
-- expressionParser st =
--   tryChoices
--     [ lampreyParser st,
--       dataTreeParser st,
--       functionCallParser st,
--       swimParser st
--     ]

-- globalStatementParser :: TreeParser u
-- globalStatementParser =
--   tryChoices
--     [functionDefinitionParser Syntax.Return, shoalParser Syntax.Return]

-- ----Parse---------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------

-- docParser :: Prs.ParsecT [Char] u DFId.Identity Syntax.SyntaxTree
-- docParser = do
--   globalStatements <- Prs.many globalStatementParser
--   let mainTree =
--         Tree.tree (UC.defaultValue {Syntax.token = Syntax.Data (Syntax.Id "main")})
--       docTree = mainTree -<*= globalStatements
--   return docTree

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
generalParsePreserveError = Prs.parse

getParseError :: Prs.ParseError -> String -> Exception.Exception
getParseError prsErr srcStr =
  let errLine = (Prs.sourceLine . Prs.errorPos) prsErr
      errColumn = (Prs.sourceColumn . Prs.errorPos) prsErr
      lineContext = (showWindow errColumn . flip (!!) (errLine - 1) . lines) srcStr
      errMsg = unlines ["\n", show prsErr, lineContext]
   in Exception.newException Exception.FailedToParse [errLine] errMsg Exception.Fatal

-- parse' :: Prs.SourceName -> [Char] -> Either Prs.ParseError Syntax.SyntaxTree
-- parse' srcName src = Prs.parse docParser srcName src

-- ----Reporting-----------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------

showWindow :: Int -> String -> String
showWindow n = take windowSize . drop (((-) n . div windowSize) 2)
  where
    windowSize = 30