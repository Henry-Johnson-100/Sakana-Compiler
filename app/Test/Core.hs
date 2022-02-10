module Test.Core
  ( module Test.Tasty,
    module Test.Tasty.HUnit,
    expectRight,
    -- standardTimeout,
    -- timedTest,
    timedAssertEqual,
    -- dataId,
    -- dataNum,
    -- dataString,
    -- makeSU,
    -- treeSU,
    -- treeId,
    -- treeKeyword,
    -- listIds,
    -- attachToMain,
    -- attachToLamprey,
    -- attachToShoal,
  )
where

-- import Parser.Core (generalParse)

--   ( Data (Id, Num, String),
--     Keyword (Fish, Lamprey, Shoal),
--     ScopeType (Send),
--     SyntaxTree,
--     SyntaxUnit (SyntaxUnit, line, token),
--     Token (Data, Keyword),
--   )

import qualified Data.Either as Either
import Parser.Syntax
import Test.Tasty
import Test.Tasty.HUnit

expectRight :: Either.Either a b -> b
expectRight = Either.either (error "Expected value of right type but got left type") id

-- import Util.Classes (Defaultable (defaultValue))
-- import Util.General ((.<))
-- import Util.Tree (Tree, tree, (-<=))

standardTimeout :: Integer -> TestTree -> TestTree
standardTimeout timeS = localOption (Timeout (timeS * 1000000) (show timeS ++ "s"))

timedTest :: Integer -> TestName -> Assertion -> TestTree
timedTest timeS name = standardTimeout timeS . testCase name

timedAssertEqual ::
  (Eq p, Show p) =>
  Integer ->
  TestName ->
  [Char] ->
  p ->
  p ->
  TestTree
timedAssertEqual timeS name description_optional assert func =
  timedTest timeS name assertion
  where
    assertion = assertEqual d a f
    d = if null description_optional then name else description_optional
    a = assert
    f = func

-- dataId :: String -> Token
-- dataId = Data . Id

-- dataNum :: Double -> Token
-- dataNum = Data . Num

-- dataString :: String -> Token
-- dataString = Data . String

-- makeSU :: ScopeType -> Token -> SyntaxUnit
-- makeSU c t = SyntaxUnit t 1 c

-- treeSU :: ScopeType -> Token -> Tree SyntaxUnit
-- treeSU = tree .< makeSU

-- treeId :: ScopeType -> String -> SyntaxTree
-- treeId st = treeSU st . dataId

-- treeKeyword :: ScopeType -> Keyword -> SyntaxTree
-- treeKeyword st = treeSU st . Keyword

-- listIds :: ScopeType -> [[Char]] -> [SyntaxTree]
-- listIds st = (<$>) (treeSU st) . (<$>) dataId

-- sendArgs :: [[Char]] -> [SyntaxTree]
-- sendArgs = listIds Send

-- attachToMain :: [SyntaxTree] -> SyntaxTree
-- attachToMain = (-<=) (tree (defaultValue) {token = Data (Id "main"), line = 1})

-- attachToLamprey :: [Tree SyntaxUnit] -> Tree SyntaxUnit
-- attachToLamprey = (-<=) (tree (defaultValue {token = Keyword Lamprey, line = 1}))

-- attachToFish :: [SyntaxTree] -> SyntaxTree
-- attachToFish = (-<=) (tree (defaultValue {token = Keyword Fish, line = 1}))

-- attachToShoal :: [Tree SyntaxUnit] -> Tree SyntaxUnit
-- attachToShoal = (-<=) (tree (defaultValue {token = Keyword Shoal, line = 1}))