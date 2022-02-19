{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Parser.Syntax
  ( StreamUnit (..),
    DocStream (..),
    CharUnit (..),
    TokenStreamUnit (..),
    SknBracketTerminal (..),
    SknScopeType (..),
    SknBracketType (..),
    SknBracket (..),
    SknKeyword (..),
    SknFlag (..),
    SknType (..),
    SknValLiteral (..),
    SknData (..),
    SknStaticTreeLabel (..),
    LabeledTree (..),
    SknToken (..),
    SknSyntaxUnit (..),
    sknData,
    reservedWords,
    tsutosu,
    isKeywordRequiringId,
    unId,
    liftTreeWithLabel,
    joinLabeledTree,
    labeledTreeNode,
    labeledTreeChildren,
    (@-<=),
  )
where

import qualified Data.Char
import qualified Data.List
import qualified Data.Maybe as Maybe
import qualified Util.Classes as UC
import Util.General ((.<))
import qualified Util.Tree as Tree

----Data definitions----------------------------------------------------------------------
------------------------------------------------------------------------------------------

-- | Stream of some data, probably Char, not really used for anything
type DocStream = String

-- | Holds information about a single unit from a RawStream
data StreamUnit a = StreamUnit
  { streamUnit :: a,
    streamUnitLine :: Int,
    streamUnitColumn :: Int
  }
  deriving (Show, Eq)

type CharUnit = StreamUnit Char

type TokenStreamUnit = StreamUnit SknToken

-- data SknId = SknId {sknId :: !String} deriving (Show, Eq, UC.Format)

data SknBracketTerminal = Open | Close deriving (Show, Read, Eq, UC.Format)

data SknScopeType = Send | Return deriving (Show, Read, Eq, UC.Format)

data SknBracketType = Value | Type deriving (Show, Read, Eq, UC.Format)

data SknBracket = SknBracket
  { sknBracketType :: !SknBracketType,
    sknBracketScope :: !SknScopeType,
    sknBracketTerminal :: !SknBracketTerminal
  }
  deriving (Show, Eq)

data SknKeyword
  = Fish
  | School
  | Shoal
  | Swim
  | Lamprey
  | Fin
  | Mackerel
  deriving (Show, Read, Eq, UC.Format, Enum)

data SknFlag
  = Impure
  | TailCall
  | IR
  | TypeUnsafe
  deriving (Show, Read, Eq, UC.Format, Enum)

data SknType
  = SknTInteger
  | SknTDouble
  | SknTChar
  | SknTString
  | SknTBool
  | SknTVar !String
  | -- See SknTFunc for why SknTStruct no longer has a list of types
    SknTStruct !String
  | -- SknTFunc doesn't need a list of types because it will hold them as its children
    -- in the tree representation (I think)
    SknTFunc
  | SknTConstraint !String
  | SknTUntyped
  deriving (Show, Eq, UC.Format)

-- | Describing values' literal construction
data SknValLiteral
  = SknVInteger !Integer
  | SknVDouble !Double
  | SknVChar !Char
  | SknVString !String
  | SknVBool !Bool
  | SknVId !String
  | SknVList ![SknValLiteral]
  deriving (Show, Eq, UC.Format)

data SknData = SknData {sknDataLiteral :: !SknValLiteral, sknDataType :: !SknType}
  deriving (Show, Eq, UC.Format)

data SknStaticTreeLabel
  = Literal
  | FishDef
  | SchoolDef
  | ShoalDef
  | SwimExpr
  | FinExpr
  | MackerelExpr
  | LampreyExpr
  | IdCallExpr
  | TypeAnnotation
  | NoLabel
  deriving (Show, Eq, UC.Format)

type SyntaxTree = Tree.Tree SknSyntaxUnit

data SknToken
  = SknTokenBracket !SknBracket
  | SknTokenData !SknData
  | SknTokenKeyword !SknKeyword
  | SknTokenFlag !SknFlag
  | SknTokenTypeLiteral !SknType
  deriving (Show, Eq, UC.Format)

data SknSyntaxUnit = SknSyntaxUnit
  { sknUnitToken :: !SknToken,
    sknUnitLine :: !Int,
    sknUnitContext :: !SknScopeType
  }
  deriving (Show, Eq)

-- | simpler constructor for SknData from just a provided SknValLiteral
sknData :: SknValLiteral -> SknData
sknData svl =
  SknData
    svl
    ( case svl of
        SknVInteger _ -> SknTInteger
        SknVDouble _ -> SknTDouble
        SknVChar _ -> SknTChar
        SknVString _ -> SknTString
        SknVBool _ -> SknTBool
        SknVList _ -> SknTStruct "List"
        SknVId _ -> SknTUntyped
    )

----SknTree data type---------------------------------------------------------------------
------------------------------------------------------------------------------------------

data LabeledTree a
  = EmptyLabeledTree
  | LabeledTree !SknStaticTreeLabel !a ![LabeledTree a]
  deriving (Show, Eq)

instance Functor LabeledTree where
  fmap f EmptyLabeledTree = EmptyLabeledTree
  fmap f (LabeledTree label anode trs) = LabeledTree label (f anode) (map (fmap f) trs)

instance Foldable LabeledTree where
  foldr fabb b EmptyLabeledTree = b
  foldr _ b (LabeledTree _ _ []) = b
  foldr fabb b (LabeledTree label anode (tr : trs)) =
    foldr fabb (fabb anode b) tr

liftTreeWithLabel :: SknStaticTreeLabel -> Tree.Tree a -> LabeledTree a
liftTreeWithLabel label Tree.Empty = EmptyLabeledTree
liftTreeWithLabel label ((Tree.:-<-:) n cs) =
  LabeledTree label n (map (liftTreeWithLabel label) cs)

joinLabeledTree :: LabeledTree a -> Tree.Tree a
joinLabeledTree EmptyLabeledTree = Tree.Empty
joinLabeledTree (LabeledTree _ n trs) =
  Data.List.foldl' (\b a -> (Tree.-<-) b (joinLabeledTree a)) (Tree.tree n) trs

labeledTreeNode :: LabeledTree a -> Maybe.Maybe a
labeledTreeNode EmptyLabeledTree = Maybe.Nothing
labeledTreeNode (LabeledTree _ x _) = Maybe.Just x

labeledTreeChildren :: LabeledTree a -> [LabeledTree a]
labeledTreeChildren EmptyLabeledTree = []
labeledTreeChildren (LabeledTree _ _ cs) = cs

infixl 9 @-<=

(@-<=) :: LabeledTree a -> [LabeledTree a] -> LabeledTree a
EmptyLabeledTree @-<= _ = EmptyLabeledTree
(LabeledTree l b trs) @-<= ltrs = LabeledTree l b (trs ++ ltrs)

-- (LabeledTree xl xsu xcs) @-<= [] = LabeledTree

----Instances-----------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance (UC.Format a) => UC.Format (StreamUnit a) where
  format (StreamUnit x line col) =
    UC.format x
      ++ concat [" on line: ", show line, " and column: ", show col]

instance UC.Format SknBracket where
  format (SknBracket brType scope term) =
    appendInOrder term (getScopeAngleBracket scope) (getTerminalBracket brType term)
    where
      getScopeAngleBracket Send = ">"
      getScopeAngleBracket _ = "<"
      getTerminalBracket Type _ = ":"
      getTerminalBracket _ Open = "("
      getTerminalBracket _ Close = ")"
      appendInOrder Open abr terml = abr ++ terml
      appendInOrder _ abr terml = terml ++ abr

-- instance UC.Format SknTree where
--   format (SknTree label tr) = concat [UC.format label, ":\n", UC.format tr]

instance UC.Format SknSyntaxUnit where
  format (SknSyntaxUnit token line context) =
    Data.List.intercalate " | " [UC.format token, show line, UC.format context]

----Defaultable

-- | Produces an untyped False value.
instance UC.Defaultable SknData where
  defaultValue = SknData (SknVBool False) SknTUntyped

instance UC.Defaultable SknSyntaxUnit where
  defaultValue =
    SknSyntaxUnit (SknTokenData UC.defaultValue) 0 Return

instance UC.Defaultable SknStaticTreeLabel where
  defaultValue = NoLabel

----Data functions------------------------------------------------------------------------
------------------------------------------------------------------------------------------

reservedWords :: [String]
reservedWords = reservedSknKeywords ++ reservedSknFlags
  where
    reservedSknKeywords = (map (toLower . show) . enumFrom) Fish
    reservedSknFlags = (map (toLower . show) . enumFrom) Impure
    toLower = map Data.Char.toLower

-- | TokenStremaUnit to SknSyntaxUnit
tsutosu :: SknScopeType -> TokenStreamUnit -> SknSyntaxUnit
tsutosu st (StreamUnit t l _) = SknSyntaxUnit t l st

isKeywordRequiringId :: SknKeyword -> Bool
isKeywordRequiringId k = or $ (<$>) (k ==) [Fish, School, Shoal]

unId :: SknValLiteral -> Maybe.Maybe String
unId (SknVId id') = Maybe.Just id'
unId _ = Maybe.Nothing
