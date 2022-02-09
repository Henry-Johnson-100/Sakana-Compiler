{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Parser.Syntax
  ( StreamUnit (..),
    DocStream (..),
    CharUnit (..),
    TokenStreamUnit (..),
    SknId (..),
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
    SknTree (..),
    SknSyntaxUnit (..),
    sknData,
    reservedWords,
    tsutosu,
    isKeywordRequiringId,
  )
where

import qualified Data.Char
import qualified Data.List
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

data SknId = SknId !String deriving (Show, Eq, UC.Format)

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
  | Piscis
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

-- | Describing types, which can be possessed by all tokens and also by literals
data SknType
  = SknTInteger
  | SknTDouble
  | SknTChar
  | SknTString
  | SknTBool
  | SknTVar !String
  | SknTStruct !String
  | SknTConstraint !String !SknType
  | SknTUntyped
  deriving (Show, Eq, UC.Format)

-- | Describing values' literal construction
data SknValLiteral
  = SknVInteger !Integer
  | SknVDouble !Double
  | SknVChar !Char
  | SknVString !String
  | SknVBool !Bool
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
  | PiscisDef
  deriving (Show, Eq, UC.Format)

data SknTree = SknTree !SknStaticTreeLabel !SyntaxTree deriving (Show, Eq)

type SyntaxTree = Tree.Tree SknSyntaxUnit

data SknToken
  = SknTokenBracket !SknBracket
  | SknTokenData !SknData
  | SknTokenKeyword !SknKeyword
  | SknTokenFlag !SknFlag
  | SknTokenId !SknId
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
    )

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

instance UC.Format SknTree where
  format (SknTree label tr) = concat [UC.format label, ":\n", UC.format tr]

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
