{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
import Data.Either
import Data.Functor.Identity
import Data.List
import Parser.Core
import Parser.Syntax
import Test.Core
import Text.Parsec (ParsecT)
import Text.Parsec.Error as PrsE
import Util.Tree

type AnyParser a = ParsecT String () Identity a

tparse :: (Eq a) => AnyParser a -> String -> a
tparse p = generalParse p ""

tparseAnyFail :: (Eq a) => AnyParser a -> String -> Bool
tparseAnyFail p = either (const True) (const False) . generalParsePreserveError p ""

errorMsgLike :: PrsE.Message -> PrsE.Message -> Bool
errorMsgLike (SysUnExpect _) (SysUnExpect _) = True
errorMsgLike (UnExpect _) (UnExpect _) = True
errorMsgLike (Expect _) (Expect _) = True
errorMsgLike (Message _) (Message _) = True
errorMsgLike _ _ = False

parseErrorMsgStrings :: (String -> PrsE.Message) -> PrsE.ParseError -> [String]
parseErrorMsgStrings mType =
  map PrsE.messageString
    . filter (errorMsgLike (mType ""))
    . PrsE.errorMessages

anyParseErrorMsg :: (String -> PrsE.Message) -> String -> PrsE.ParseError -> Bool
anyParseErrorMsg mType comp = elem comp . parseErrorMsgStrings mType

tparseFailsWithMsg p mType comp =
  either (anyParseErrorMsg mType comp) (const False) . generalParsePreserveError p ""

main = defaultMain tests

tests =
  testGroup
    "Parser Tests"
    [ valueLiteralParserTests,
      reservedWordParserTests,
      primitiveTypeLiteralParserTests,
      idParserTests,
      sknTokenParserTests,
      lampreyParserTests,
      typeAnnotationParserTests
    ]

valueLiteralParserTests =
  testGroup
    "Value Literal Parser Tests"
    [ timedAssertEqual
        1
        "Can parse True"
        []
        (SknVBool True)
        (tparse valLiteralBoolParser "True"),
      timedAssertEqual
        1
        "Can parse False"
        []
        (SknVBool False)
        (tparse valLiteralBoolParser "False"),
      timedAssertEqual
        1
        "Can parse a non-space char literal"
        []
        (SknVChar 'a')
        (tparse valLiteralCharParser "\'a\'"),
      timedAssertEqual
        1
        "Can parse a \' \' char literal"
        []
        (SknVChar ' ')
        (tparse valLiteralCharParser "\' \'"),
      timedAssertEqual
        1
        "Can parse a \\n char literal"
        []
        (SknVChar '\n')
        (tparse valLiteralCharParser "\'\n\'"),
      timedAssertEqual
        1
        "Can parse a string with no whitespace"
        []
        (SknVString "Hello")
        (tparse valLiteralStringParser "\"Hello\""),
      timedAssertEqual
        1
        "Can parse a string with a space"
        []
        (SknVString "Hello World!")
        (tparse valLiteralStringParser "\"Hello World!\""),
      timedAssertEqual
        1
        "can parse a positive Integer"
        []
        (SknVInteger 2)
        (tparse valLiteralIntegerParser "2"),
      timedAssertEqual
        1
        "can parse a negative Integer"
        []
        (SknVInteger (-2))
        (tparse valLiteralIntegerParser "-2"),
      timedAssertEqual
        1
        "Can parse a whole double"
        []
        (SknVDouble 2.0)
        (tparse valLiteralDoubleParser "2.0"),
      timedAssertEqual
        1
        "Can parse an arbitrary double"
        []
        (SknVDouble 4.7685)
        (tparse valLiteralDoubleParser "4.7685"),
      timedAssertEqual
        1
        "Can parse a negative arbitrary double"
        []
        (SknVDouble (-4.7685))
        (tparse valLiteralDoubleParser "-4.7685"),
      timedAssertEqual
        1
        "literal parser differentiates Bools"
        []
        (SknVBool True)
        (tparse sknValLiteralParser "True"),
      timedAssertEqual
        1
        "literal parser differentiates Chars"
        []
        (SknVChar '.')
        (tparse sknValLiteralParser "\'.\'"),
      timedAssertEqual
        1
        "literal parser differentiates Strings"
        []
        (SknVString ".")
        (tparse sknValLiteralParser "\".\""),
      timedAssertEqual
        1
        "literal parser parses strings that contain literal chars"
        []
        (SknVString "This \'a\' is a char")
        (tparse sknValLiteralParser "\"This \'a\' is a char\""),
      timedAssertEqual
        1
        "literal parser differentiates positive integers"
        []
        (SknVInteger 14)
        (tparse sknValLiteralParser "14"),
      timedAssertEqual
        1
        "literal parser differentiates negatives integers"
        []
        (SknVInteger (-460))
        (tparse sknValLiteralParser "-460"),
      timedAssertEqual
        1
        "literal parser differentiates positive doubles"
        []
        (SknVDouble 48.2)
        (tparse sknValLiteralParser "48.2"),
      timedAssertEqual
        1
        "literal parser differentiates negative doubles"
        []
        (SknVDouble (-587.3465))
        (tparse sknValLiteralParser "-587.3465"),
      timedAssertEqual
        1
        "Parser fails malformed token"
        "12.bool.fam"
        True
        (tparseAnyFail sknValLiteralParser "12.bool.fam"),
      timedAssertEqual
        1
        "Parser fails malformed token"
        "\"Hello\".sir.12.or_madam"
        True
        (tparseAnyFail sknValLiteralParser "\"Hello\".sir.12.or_madam")
    ]

reservedWordParserTests =
  testGroup
    "Reserved Keyword and Flag Parser Tests"
    [ timedAssertEqual
        1
        "Parse fish"
        []
        Fish
        (tparse (sknKeywordParser Fish) "fish"),
      timedAssertEqual
        1
        "Parse some other keyword"
        []
        Lamprey
        (tparse (sknKeywordParser Lamprey) "lamprey"),
      timedAssertEqual
        1
        "Parse a flag"
        []
        Impure
        (tparse sknFlagParser "Impure"),
      timedAssertEqual
        1
        "Parse TailCall flag"
        []
        TailCall
        (tparse sknFlagParser "TailCall")
    ]

primitiveTypeLiteralParserTests =
  testGroup
    "Primitive Type Literal Parser Tests"
    [ timedAssertEqual
        1
        "Will parse any of the built-in type literals"
        []
        [SknTInteger, SknTDouble, SknTChar, SknTString, SknTBool]
        ( tparse typeLiteralPrimitiveParser
            <$> ["Integer", "Double", "Char", "String", "Bool"]
        ),
      timedAssertEqual
        1
        "Will parse a simple ID as a type variable."
        []
        (SknTVar "a")
        (tparse typeLiteralPrimitiveParser "a"),
      timedAssertEqual
        1
        "Will parse an arbitrary ID as a type variable."
        []
        (SknTVar "Some.Arbitrary.Type.Int_4$")
        (tparse typeLiteralPrimitiveParser "Some.Arbitrary.Type.Int_4$")
    ]

idParserTests =
  testGroup
    "Id Parser Tests"
    [ timedAssertEqual
        1
        "Parse a letter ID"
        []
        (SknVId "main")
        (tparse sknIdParser "main"),
      timedAssertEqual
        1
        "Parse a capitalized ID"
        []
        (SknVId "Main")
        (tparse sknIdParser "Main"),
      timedAssertEqual
        1
        "Parse a symbol id"
        []
        (SknVId "+")
        (tparse sknIdParser "+"),
      timedAssertEqual
        1
        "Parse an id with numbers after letter head."
        []
        (SknVId "alpha123")
        (tparse sknIdParser "alpha123"),
      timedAssertEqual
        1
        "Parse an ID starting with symbol with numbers"
        []
        (SknVId "+2xgood")
        (tparse sknIdParser "+2xgood"),
      timedAssertEqual
        1
        "Parse an ID with an accessor prefix"
        []
        (SknVId "test.parser")
        (tparse sknIdParser "test.parser"),
      timedAssertEqual
        1
        "Parse an ID with assorted chars in accessor and accessee"
        []
        (SknVId "test1.pars3rtests!#")
        (tparse sknIdParser "test1.pars3rtests!#"),
      timedAssertEqual
        1
        "Parse an arbitrarily nested ID"
        []
        (SknVId "Test.Parser.Core1.@Test014_*")
        (tparse sknIdParser "Test.Parser.Core1.@Test014_*")
    ]

sknTokenParserTests =
  testGroup
    "SknToken Parser Tests"
    [ sknTokenBracketParserTests,
      sknTokenDataParserTests,
      sknTokenKeywordParserTests,
      sknTokenFlagParserTests,
      sknTokenIdParserTests
    ]

sknTokenBracketParserTests =
  testGroup
    "Token Bracket Parser Tests"
    [ timedAssertEqual
        1
        "An open Send value bracket"
        []
        (SknTokenBracket (SknBracket Value Send Open))
        (sknTokenBracketP Value Send Open ">("),
      timedAssertEqual
        1
        "A close Send value bracket"
        []
        (SknTokenBracket (SknBracket Value Send Close))
        (sknTokenBracketP Value Send Close ")>"),
      timedAssertEqual
        1
        "An open Return value bracket"
        []
        (SknTokenBracket (SknBracket Value Return Open))
        (sknTokenBracketP Value Return Open "<("),
      timedAssertEqual
        1
        "A close Return value bracket"
        []
        (SknTokenBracket (SknBracket Value Return Close))
        (sknTokenBracketP Value Return Close ")<"),
      timedAssertEqual
        1
        "An open Send Type bracket"
        []
        (SknTokenBracket (SknBracket Type Send Open))
        (sknTokenBracketP Type Send Open ">:"),
      timedAssertEqual
        1
        "A close Send value bracket"
        []
        (SknTokenBracket (SknBracket Type Send Close))
        (sknTokenBracketP Type Send Close ":>"),
      timedAssertEqual
        1
        "An open Return value bracket"
        []
        (SknTokenBracket (SknBracket Type Return Open))
        (sknTokenBracketP Type Return Open "<:"),
      timedAssertEqual
        1
        "A close Return value bracket"
        []
        (SknTokenBracket (SknBracket Type Return Close))
        (sknTokenBracketP Type Return Close ":<")
    ]
  where
    sknTokenBracketP bt st bterm str = tparse (sknTokenBracketParser bt st bterm) str

sknTokenDataParserTests =
  testGroup
    "Token Data Parser Tests"
    [ timedAssertEqual
        1
        "Data is parsed as a SknTokenData - Bool"
        []
        (SknTokenData (SknData (SknVBool True) SknTBool))
        (tparse sknTokenDataParser "True"),
      timedAssertEqual
        1
        "Data is parsed as a SknTokenData - Bool 2"
        []
        (SknTokenData (SknData (SknVBool False) SknTBool))
        (tparse sknTokenDataParser "False"),
      timedAssertEqual
        1
        "Data is parsed as a SknTokenData - +Integer"
        []
        (SknTokenData (SknData (SknVInteger 25) SknTInteger))
        (tparse sknTokenDataParser "25"),
      timedAssertEqual
        1
        "Data is parsed as a SknTokenData - -Integer"
        []
        (SknTokenData (SknData (SknVInteger (-25)) SknTInteger))
        (tparse sknTokenDataParser "-25"),
      timedAssertEqual
        1
        "Data is parsed as a SknTokenData - Integer (0)"
        []
        (SknTokenData (SknData (SknVInteger 0) SknTInteger))
        (tparse sknTokenDataParser "0"),
      timedAssertEqual
        1
        "Data is parsed as a SknTokenData - +Double"
        []
        (SknTokenData (SknData (SknVDouble 1.5) SknTDouble))
        (tparse sknTokenDataParser "1.5"),
      timedAssertEqual
        1
        "Data is parsed as a SknTokenData - -Double"
        []
        (SknTokenData (SknData (SknVDouble (-1.5)) SknTDouble))
        (tparse sknTokenDataParser "-1.5"),
      timedAssertEqual
        1
        "Data is parsed as a SknTokenData - Double (0)"
        []
        (SknTokenData (SknData (SknVDouble 0.0) SknTDouble))
        (tparse sknTokenDataParser "0.0"),
      timedAssertEqual
        1
        "Data is parsed as a SknTokenData - Char"
        []
        (SknTokenData (SknData (SknVChar '.') SknTChar))
        (tparse sknTokenDataParser "\'.\'"),
      timedAssertEqual
        1
        "Data is parsed as a SknTokenData - Char Space"
        []
        (SknTokenData (SknData (SknVChar ' ') SknTChar))
        (tparse sknTokenDataParser "\' \'"),
      timedAssertEqual
        1
        "Data is parsed as a SknTokenData - String"
        []
        (SknTokenData (SknData (SknVString "Hello World!") SknTString))
        (tparse sknTokenDataParser "\"Hello World!\"")
    ]

sknTokenIdParserTests =
  testGroup
    "Token Data Id Parser Tests"
    [ timedAssertEqual
        1
        "Parse a letter ID"
        []
        ((SknTokenData . sknData . SknVId) "main")
        (tparse sknTokenDataParser "main"),
      timedAssertEqual
        1
        "Parse a capitalized ID"
        []
        ((SknTokenData . sknData . SknVId) "Main")
        (tparse sknTokenDataParser "Main"),
      timedAssertEqual
        1
        "Parse a symbol id"
        []
        ((SknTokenData . sknData . SknVId) "+")
        (tparse sknTokenDataParser "+"),
      timedAssertEqual
        1
        "Parse an id with numbers after letter head."
        []
        ((SknTokenData . sknData . SknVId) "alpha123")
        (tparse sknTokenDataParser "alpha123"),
      timedAssertEqual
        1
        "Parse an ID starting with symbol with numbers"
        []
        ((SknTokenData . sknData . SknVId) "+2xgood")
        (tparse sknTokenDataParser "+2xgood"),
      timedAssertEqual
        1
        "Parse an ID with an accessor prefix"
        []
        ((SknTokenData . sknData . SknVId) "test.parser")
        (tparse sknTokenDataParser "test.parser"),
      timedAssertEqual
        1
        "Parse an ID with assorted chars in accessor and accessee"
        []
        ((SknTokenData . sknData . SknVId) "test1.pars3rtests!#")
        (tparse sknTokenDataParser "test1.pars3rtests!#"),
      timedAssertEqual
        1
        "Parse an arbitrarily nested ID"
        []
        ((SknTokenData . sknData . SknVId) "Test.Parser.Core1.@Test014_*")
        (tparse sknTokenDataParser "Test.Parser.Core1.@Test014_*")
    ]

sknTokenKeywordParserTests =
  testGroup
    "Token Keyword Parser Tests"
    [ timedAssertEqual
        1
        "Parse a keyword fish"
        []
        (SknTokenKeyword Fish)
        (keywordTokenTParser Fish "fish"),
      timedAssertEqual
        1
        "Parse a keyword school"
        []
        (SknTokenKeyword School)
        (keywordTokenTParser School "school"),
      timedAssertEqual
        1
        "Parse a keyword shoal"
        []
        (SknTokenKeyword Shoal)
        (keywordTokenTParser Shoal "shoal"),
      timedAssertEqual
        1
        "Parse a keyword swim"
        []
        (SknTokenKeyword Swim)
        (keywordTokenTParser Swim "swim"),
      timedAssertEqual
        1
        "Parse a keyword lamprey"
        []
        (SknTokenKeyword Lamprey)
        (keywordTokenTParser Lamprey "lamprey"),
      timedAssertEqual
        1
        "Parse a keyword fin"
        []
        (SknTokenKeyword Fin)
        (keywordTokenTParser Fin "fin"),
      timedAssertEqual
        1
        "Parse a keyword mackerel"
        []
        (SknTokenKeyword Mackerel)
        (keywordTokenTParser Mackerel "mackerel")
    ]
  where
    keywordTokenTParser k s = tparse (sknTokenKeywordParser k) s

sknTokenFlagParserTests =
  testGroup
    "Token Flag Parser Tests"
    [ timedAssertEqual
        1
        "Parses Impure flag"
        []
        (SknTokenFlag Impure)
        (tparse sknTokenFlagParser "Impure"),
      timedAssertEqual
        1
        "Parses TailCall flag"
        []
        (SknTokenFlag TailCall)
        (tparse sknTokenFlagParser "TailCall"),
      timedAssertEqual
        1
        "Parses IR flag"
        []
        (SknTokenFlag IR)
        (tparse sknTokenFlagParser "IR"),
      timedAssertEqual
        1
        "Parses TypeUnsafe flag"
        []
        (SknTokenFlag TypeUnsafe)
        (tparse sknTokenFlagParser "TypeUnsafe")
    ]

ktosu :: SknScopeType -> SknKeyword -> SknSyntaxUnit
ktosu st k = SknSyntaxUnit (SknTokenKeyword k) 1 st

idtosu :: SknScopeType -> String -> SknSyntaxUnit
idtosu st i = SknSyntaxUnit (SknTokenData (sknData (SknVId i))) 1 st

lampreyParserTests =
  testGroup
    "Lamprey Component Part Parser Tests"
    [ lampreyParameterParserTests,
      lampreyDefinitionParserTests
    ]

lampreyParameterParserTests =
  testGroup
    "Lamprey Parameter Parser Tests"
    [ timedAssertEqual
        1
        "Parse a simple id"
        []
        [LabeledTree IdCallExpr (idtosu Send "x") []]
        (tparse (lampreyParameterParser Send) "x"),
      -- Lamprey parameters can either have explicit or implicit type annotations
      -- whether or not the type is known will be checked in static analysis.
      timedAssertEqual
        1
        "Parse a simple id with a type annotation"
        []
        [ LabeledTree
            IdCallExpr
            (idtosu Send "x")
            [ LabeledTree
                TypeAnnotation
                (SknSyntaxUnit (SknTokenTypeLiteral SknTInteger) 1 Return)
                []
            ]
        ]
        (tparse (lampreyParameterParser Send) "x <:Integer:<"),
      timedAssertEqual
        1
        "Parse a simple function definition - this should never fail"
        []
        ( tparse
            (sknFunctionDefStatementParser Send)
            "fish add` >:Integer:> >:Integer:> <:Integer:<\
            \ >(x)> >(y)> <(+ >(x)> >(y)>)<"
        )
        ( tparse
            (lampreyParameterParser Send)
            "fish add` >:Integer:> >:Integer:> <:Integer:<\
            \ >(x)> >(y)> <(+ >(x)> >(y)>)<"
        )
    ]

lampreyDefinitionParserTests =
  testGroup
    "Lamprey Parser Tests"
    [ timedAssertEqual
        1
        "Parse a lamprey with one parameter and id return."
        []
        [ LabeledTree
            LampreyExpr
            (ktosu Return Lamprey)
            [ LabeledTree IdCallExpr (idtosu Send "x") [],
              LabeledTree IdCallExpr (idtosu Return "x") []
            ]
        ]
        (tparse (lampreyParser Return) "lamprey >(x)> <(x)<"),
      timedAssertEqual
        1
        "Parse a lamprey with multiple parameters and id return."
        []
        [ LabeledTree
            LampreyExpr
            (ktosu Return Lamprey)
            [ LabeledTree IdCallExpr (idtosu Send "x") [],
              LabeledTree IdCallExpr (idtosu Send "y") [],
              LabeledTree IdCallExpr (idtosu Return "x") []
            ]
        ]
        (tparse (lampreyParser Return) "lamprey >(x)> >(y)> <(x)<")
    ]

typeAnnotationParserTests =
  testGroup
    "Type Annotation Parser Tests"
    [ primitiveTypeAnnotationReturnParserTests,
      primitiveTypeAnnotationSendParserTests,
      typeAnnotationConstraintParserTests,
      typeAnnotationStructParserTests
    ]

primitiveTypeAnnotationReturnParserTests :: TestTree
primitiveTypeAnnotationReturnParserTests =
  testGroup
    "Type Annotation Return Parser Tests"
    ( zipWith
        ( \assertion text ->
            timedAssertEqual
              1
              ( "TypeAnnotationReturnParser Test works\
                \ for literal: "
                  ++ text
              )
              []
              [ LabeledTree
                  TypeAnnotation
                  (SknSyntaxUnit (SknTokenTypeLiteral assertion) 1 Return)
                  []
              ]
              (tparse (typeAnnotationPrimitiveParser Return) text)
        )
        [ SknTInteger,
          SknTDouble,
          SknTChar,
          SknTString,
          SknTBool,
          SknTVar "test",
          SknTVar "test",
          SknTVar "test._symbol3s$"
        ]
        [ "<:Integer:<",
          "<:Double:<",
          "<: Char :<",
          "<: String:<",
          "<:Bool:<",
          "<:test:<",
          "<: test :<",
          "<:   test._symbol3s$ :<"
        ]
    )

primitiveTypeAnnotationSendParserTests :: TestTree
primitiveTypeAnnotationSendParserTests =
  testGroup
    "Type Annotation Send Parser Tests"
    ( zipWith
        ( \assertion text ->
            timedAssertEqual
              1
              ( "TypeAnnotationSendParser Test works\
                \ for literal: "
                  ++ text
              )
              []
              [ LabeledTree
                  TypeAnnotation
                  (SknSyntaxUnit (SknTokenTypeLiteral assertion) 1 Send)
                  []
              ]
              (tparse (typeAnnotationPrimitiveParser Send) text)
        )
        [ SknTInteger,
          SknTDouble,
          SknTChar,
          SknTString,
          SknTBool,
          SknTVar "test",
          SknTVar "test",
          SknTVar "test._symbol3s$"
        ]
        [ ">:Integer:>",
          ">:Double:>",
          ">: Char :>",
          ">: String:>",
          ">:Bool:>",
          ">:test:>",
          ">: test :>",
          ">:   test._symbol3s$ :>"
        ]
    )

typeAnnoTree tcons tid st =
  LabeledTree TypeAnnotation (SknSyntaxUnit (SknTokenTypeLiteral (tcons tid)) 1 st) []

typeAnnotationConstraintParserTests =
  testGroup
    "Type Annotation Constraint Parser Tests"
    [ timedAssertEqual
        1
        "Parse a simple constraint"
        []
        [typeAnnoTree SknTVar "a" Send @-<= [typeAnnoTree SknTConstraint "Num" Return]]
        (tparse (typeAnnotationConstraintParser Send) ">:a <:Num:< :>"),
      timedAssertEqual
        1
        "Parse a simple constraint"
        []
        [typeAnnoTree SknTVar "a" Return @-<= [typeAnnoTree SknTConstraint "Num" Return]]
        (tparse (typeAnnotationConstraintParser Return) "<:a <:Num:< :<"),
      timedAssertEqual
        1
        "Parse a type with multiple constraints"
        []
        [ typeAnnoTree SknTVar "a" Send
            @-<= ((\x -> typeAnnoTree SknTConstraint x Return) <$> ["Semigroup", "Monad"])
        ]
        ( tparse
            (typeAnnotationConstraintParser Send)
            ">:a <: Semigroup:< <:Monad   :< :>"
        ),
      timedAssertEqual
        1
        "Parse a type with an arbitrary number of constraints"
        []
        [ typeAnnoTree SknTVar "a" Send
            @-<= ( (\x -> typeAnnoTree SknTConstraint x Return)
                     <$> ["Semigroup", "Monad", "Foldable"]
                 )
        ]
        ( tparse
            (typeAnnotationConstraintParser Send)
            ">:a <:Semigroup:< <:Monad:< <:Foldable:< :>"
        ),
      timedAssertEqual
        1
        "Doesn't parse a constraint that has a primitive type literal"
        []
        True
        (tparseAnyFail (typeAnnotationConstraintParser Send) ">: a <: Integer :< :>")
    ]

typeAnnotationStructParserTests =
  testGroup
    "Type Annotation Struct Parser Tests"
    [ timedAssertEqual
        1
        "Parse a simple struct literal"
        []
        [typeAnnoTree SknTStruct "List" Send @-<= [typeAnnoTree SknTVar "a" Send]]
        (tparse (typeAnnotationStructLiteralParser Send) ">:List >:a:> :>"),
      timedAssertEqual
        1
        "Doesn't parse a malformed struct literal that looks like a constraint"
        []
        True
        (tparseAnyFail (typeAnnotationStructLiteralParser Send) ">: List <:a:< :>"),
      timedAssertEqual
        1
        "Parse a simple return struct literal"
        []
        [typeAnnoTree SknTStruct "List" Return @-<= [typeAnnoTree SknTVar "a" Send]]
        (tparse (typeAnnotationStructLiteralParser Return) "<: List >:a:> :<"),
      timedAssertEqual
        1
        "Parse a struct with arbritrary number of member types"
        []
        -- Work smarter not harder
        [ typeAnnoTree SknTStruct "Tri" Send
            @-<= ((\x -> typeAnnoTree SknTVar x Send) <$> ["a", "b", "c"])
        ]
        (tparse (typeAnnotationStructLiteralParser Send) ">: Tri >:a:> >:b:> >:c:> :>"),
      timedAssertEqual
        1
        "Parse a struct with struct type members"
        []
        [ typeAnnoTree SknTStruct "List" Send
            @-<= [ typeAnnoTree SknTStruct "Tree" Send
                     @-<= [typeAnnoTree SknTVar "a" Send]
                 ]
        ]
        ( tparse
            (typeAnnotationStructLiteralParser Send)
            ">: LabeledTree >:Tree >:a:> :> :>"
        ),
      timedAssertEqual
        1
        "Parse a struct with function type members"
        []
        [ typeAnnoTree SknTStruct "Something" Send
            @-<= [ typeAnnoTree (const SknTFunc) "" Send
                     @-<= [ typeAnnoTree SknTVar "a" Send,
                            typeAnnoTree SknTVar "b" Return
                          ]
                 ]
        ]
        ( tparse
            (typeAnnotationStructLiteralParser Send)
            ">: Something >: >:a:> <:b:<  :>  :>"
        )
    ]

-- miscTypeAnnotationParserTests =
--   testGroup
--     "Misc. Type Annotation Parser Tests"
--     [ timedAssertEqual
--         1
--         "Parse a return type literal"
--         []
--         [ LabeledTree
--             TypeAnnotation
--             (SknSyntaxUnit (SknTokenTypeLiteral SknTInteger) 1 Return)
--             []
--         ]
--         (tparse typeAnnotationParser "<:Integer:<"),
--       timedAssertEqual
--         1
--         "Parse a send type literal"
--         []
--         [ LabeledTree
--             TypeAnnotation
--             (SknSyntaxUnit (SknTokenTypeLiteral SknTInteger) 1 Send)
--             []
--         ]
--         (tparse typeAnnotationParser ">:Integer:>"),
--       timedAssertEqual
--         1
--         "Parse a return type variable"
--         []
--         [LabeledTree TypeAnnotation (idtosu Return "some_var.type") []]
--         (tparse typeAnnotationParser "<: some_var.type :<"),
--       timedAssertEqual
--         1
--         "Parse a function signature"
--         []
--         [ LabeledTree TypeAnnotation (idtosu Send "a") [],
--           LabeledTree TypeAnnotation (idtosu Return "b") []
--         ]
--         (tparse typeAnnotationParser ">:a:> <:b:<"),
--       timedAssertEqual
--         1
--         "Parse a more complex function signature"
--         []
--         [ LabeledTree TypeAnnotation (idtosu Send "a") [],
--           LabeledTree TypeAnnotation (idtosu Send "b") [],
--           LabeledTree TypeAnnotation (idtosu Return "c") []
--         ]
--         (tparse typeAnnotationParser ">:a:> >:b:> <:c:<"),
--       timedAssertEqual
--         1
--         "Parse a nested function signature"
--         []
--         [ LabeledTree
--             TypeAnnotation
--             (SknSyntaxUnit (SknTokenTypeLiteral SknTFunc) 0 Send)
--             [ LabeledTree TypeAnnotation (idtosu Send "a") [],
--               LabeledTree TypeAnnotation (idtosu Send "b") [],
--               LabeledTree TypeAnnotation (idtosu Return "b") []
--             ],
--           LabeledTree TypeAnnotation (idtosu Send "b") [],
--           LabeledTree TypeAnnotation (idtosu Send "a") [],
--           LabeledTree TypeAnnotation (idtosu Return "b") []
--         ]
--         (tparse typeAnnotationParser ">: >:a:> >:b:> <:b:< :>  >:b:> >:a:> <:b:<"),
--       timedAssertEqual
--         1
--         "Parse a type annotation with constraints"
--         []
--         [LabeledTree NoLabel (idtosu Return "NOT IMPLEMENTED") []]
--         (tparse typeAnnotationParser ">: a <:Eq:< :>"),
--       timedAssertEqual
--         1
--         "Parse a nested function signature with constraints"
--         []
--         [ LabeledTree
--             TypeAnnotation
--             (SknSyntaxUnit (SknTokenTypeLiteral SknTFunc) 0 Send)
--             [ LabeledTree
--                 TypeAnnotation
--                 (idtosu Send "a")
--                 [ LabeledTree
--                     TypeAnnotation
--                     (SknSyntaxUnit (SknTokenTypeLiteral (SknTVar "Num")) 0 Return)
--                     []
--                 ],
--               LabeledTree
--                 TypeAnnotation
--                 (idtosu Send "b")
--                 [ LabeledTree
--                     TypeAnnotation
--                     (SknSyntaxUnit (SknTokenTypeLiteral (SknTVar "Num")) 0 Return)
--                     []
--                 ],
--               LabeledTree
--                 TypeAnnotation
--                 (idtosu Return "b")
--                 [ LabeledTree
--                     TypeAnnotation
--                     (SknSyntaxUnit (SknTokenTypeLiteral (SknTVar "Num")) 0 Return)
--                     []
--                 ]
--             ],
--           LabeledTree
--             TypeAnnotation
--             (idtosu Send "b")
--             [ LabeledTree
--                 TypeAnnotation
--                 (SknSyntaxUnit (SknTokenTypeLiteral (SknTVar "Num")) 0 Return)
--                 []
--             ],
--           LabeledTree
--             TypeAnnotation
--             (idtosu Send "a")
--             [ LabeledTree
--                 TypeAnnotation
--                 (SknSyntaxUnit (SknTokenTypeLiteral (SknTVar "Num")) 0 Return)
--                 []
--             ],
--           LabeledTree
--             TypeAnnotation
--             (idtosu Return "b")
--             [ LabeledTree
--                 TypeAnnotation
--                 (SknSyntaxUnit (SknTokenTypeLiteral (SknTVar "Num")) 0 Return)
--                 []
--             ]
--         ]
--         ( tparse
--             typeAnnotationParser
--             ">: >:a <:Num:< :> >:b <:Num:< :> <:b <:Num:< :< :>\
--             \  >:b <:Num:< :> >:a <:Num:< :> <:b <:Num:< :<"
--         )
--     ]