import Parser.Core
import Parser.Syntax
import Test.Core
import Util.Tree

tparse p = generalParse p ""

main = defaultMain tests

tests =
  testGroup
    "Parser Tests"
    [ valueLiteralParserTests,
      reservedWordParserTests,
      primitiveTypeLiteralParserTests,
      idParserTests,
      sknTokenParserTests
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
        "Can parse an empty list"
        []
        (SknVList [])
        (tparse valLiteralListParser "[]"),
      timedAssertEqual
        1
        "Can parse a well-formed list of one type"
        []
        ((SknVList . map SknVInteger) [1, 2, 3, 4])
        (tparse valLiteralListParser "[1,2,3,4]"),
      timedAssertEqual
        1
        "Can parse a list of ints and doubles"
        []
        ( SknVList
            [ SknVInteger 1,
              SknVDouble 2.4,
              SknVInteger 5,
              SknVDouble 9.78,
              SknVDouble 57.89,
              SknVInteger 100
            ]
        )
        (tparse valLiteralListParser "[1,2.4,5,9.78,57.89,100]"),
      timedAssertEqual
        1
        "Can parse nested lists"
        []
        ( SknVList
            [ SknVList [SknVInteger 1],
              SknVList [SknVList [SknVList [SknVBool False]]]
            ]
        )
        (tparse valLiteralListParser "[[1], [[[False]]]]"),
      timedAssertEqual
        1
        "literal parser can differentiate lists"
        []
        ((SknVList . map SknVBool) [True, False, True, True, False])
        (tparse sknValLiteralParser "[True, False, True, True, False]"),
      timedAssertEqual
        1
        "List literals can contain ID's NOT IMPLEMENTED"
        []
        ( (SknVList . map SknVId)
            ["id", "add", "subtract", "foldl`", "foldr", "map", "fmap"]
        )
        ( tparse
            sknValLiteralParser
            "[ id ,add ,subtract ,foldl` ,foldr ,map ,fmap]"
        ),
      timedAssertEqual
        1
        "Can parse an ill-formed list of one type (1)"
        []
        ((SknVList . map SknVInteger) [1, 2, 3, 4])
        (tparse valLiteralListParser "[1,2,   3, 4 ]"),
      timedAssertEqual
        1
        "Can parse an ill-formed list of one type (2)"
        []
        ((SknVList . map SknVInteger) [1, 2, 3, 4, 6])
        (tparse valLiteralListParser "[     1,   2,   3, 4   ,  6 ]")
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
        (tparse sknTokenDataParser "\"Hello World!\""),
      timedAssertEqual
        1
        "Can parse an empty list"
        []
        ((SknTokenData . sknData . SknVList) [])
        (tparse sknTokenDataParser "[]"),
      timedAssertEqual
        1
        "Can parse a well-formed list of one type"
        []
        ((SknTokenData . sknData . SknVList . map SknVInteger) [1, 2, 3, 4])
        (tparse sknTokenDataParser "[1,2,3,4]"),
      timedAssertEqual
        1
        "Can parse a list of ints and doubles"
        []
        ( (SknTokenData . sknData . SknVList)
            [ SknVInteger 1,
              SknVDouble 2.4,
              SknVInteger 5,
              SknVDouble 9.78,
              SknVDouble 57.89,
              SknVInteger 100
            ]
        )
        (tparse sknTokenDataParser "[1,2.4,5,9.78,57.89,100]"),
      timedAssertEqual
        1
        "Can parse nested lists"
        []
        ( (SknTokenData . sknData . SknVList)
            [ SknVList [SknVInteger 1],
              SknVList [SknVList [SknVList [SknVBool False]]]
            ]
        )
        (tparse sknTokenDataParser "[[1], [[[False]]]]"),
      timedAssertEqual
        1
        "literal parser can differentiate lists"
        []
        ((SknTokenData . sknData . SknVList . map SknVBool) [True, False, True, True, False])
        (tparse sknTokenDataParser "[True, False, True, True, False]"),
      timedAssertEqual
        1
        "List literals can contain ID's NOT IMPLEMENTED"
        []
        ( (SknTokenData . sknData . SknVList . map SknVId)
            ["id", "add", "subtract", "foldl`", "foldr", "map", "fmap"]
        )
        ( tparse
            sknTokenDataParser
            "[ id ,add ,subtract ,foldl` ,foldr ,map ,fmap]"
        ),
      timedAssertEqual
        1
        "Can parse an ill-formed list of one type (1)"
        []
        ((SknTokenData . sknData . SknVList . map SknVInteger) [1, 2, 3, 4])
        (tparse sknTokenDataParser "[1,2,   3, 4 ]"),
      timedAssertEqual
        1
        "Can parse an ill-formed list of one type (2)"
        []
        ((SknTokenData . sknData . SknVList . map SknVInteger) [1, 2, 3, 4, 6])
        (tparse sknTokenDataParser "[     1,   2,   3, 4   ,  6 ]")
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

-- import Parser.Core
-- import Parser.Syntax as Syntax
-- -- import Test.Core
-- import Util.Tree

-- prepareParser p str = generalParse p "Test" str

-- main = defaultMain tests

-- tests =
--   testGroup
--     ""
--     [ dataParserTests,
--       keywordParserTests,
--       functionCallParserTests,
--       fishBindParserTests,
--       lampreyAndFunctionParserTests
--     ]

-- dataParserTests =
--   testGroup
--     "Data Parser Tests"
--     [ timedAssertEqual
--         2
--         "Parse a boolean"
--         []
--         (Syntax.Boolean True)
--         (prepareParser dataParser "True"),
--       timedAssertEqual
--         2
--         "Parse a False Boolean"
--         []
--         (Syntax.Boolean False)
--         (prepareParser dataParser "False"),
--       timedAssertEqual
--         2
--         "Parse a string literal with no whitespace."
--         "Normal string with no extra whitespace (\\n \\t \\r) are parsed."
--         (Syntax.String "Hello World!")
--         (prepareParser dataParser "\"Hello World!\""),
--       timedAssertEqual
--         2
--         "Parse a positive integer."
--         []
--         (Syntax.Num 100.0)
--         (prepareParser dataParser "100"),
--       timedAssertEqual
--         2
--         "Parse a negative integer."
--         []
--         (Syntax.Num (-100))
--         (prepareParser dataParser "-100"),
--       timedAssertEqual
--         2
--         "Parse a positive integer with a zero single digit decimal."
--         []
--         (Syntax.Num 100.0)
--         (prepareParser dataParser "100.0"),
--       timedAssertEqual
--         2
--         "Parse a positive double."
--         []
--         (Syntax.Num 100.12345)
--         (prepareParser dataParser "100.12345"),
--       timedAssertEqual
--         2
--         "Parse a negative double."
--         []
--         (Syntax.Num (-100.12345))
--         (prepareParser dataParser "-100.12345"),
--       timedAssertEqual
--         2
--         "Parse a normal \"main\" id."
--         []
--         (Syntax.Id "main")
--         (prepareParser idParser "main"),
--       timedAssertEqual
--         2
--         "Parse an operator id."
--         []
--         (Syntax.Id "+")
--         (prepareParser idParser "+"),
--       timedAssertEqual
--         2
--         "Parse a capitalized id."
--         []
--         (Syntax.Id "Main")
--         (prepareParser idParser "Main"),
--       timedAssertEqual
--         2
--         "Parse a normal id with symbols in it."
--         []
--         (Syntax.Id "mai%n")
--         (prepareParser idParser "mai%n"),
--       timedAssertEqual
--         2
--         "Parse a normal id with a capitalized accessor prefix."
--         []
--         (Syntax.Id "Main.main")
--         (prepareParser idParser "Main.main"),
--       timedAssertEqual
--         2
--         "Parser an operator id with a capitalized accessor prefix."
--         []
--         (Syntax.Id "Main.+")
--         (prepareParser idParser "Main.+"),
--       timedAssertEqual
--         2
--         "Parse a normal id with a normal accessor prefix."
--         []
--         (Syntax.Id "normal.main")
--         (prepareParser idParser "normal.main"),
--       timedAssertEqual
--         2
--         "Parse a normal id with an operator accessor prefix."
--         []
--         (Syntax.Id "+.main")
--         (prepareParser idParser "+.main"),
--       timedAssertEqual
--         2
--         "Parse \"fish\" as an id."
--         []
--         (Syntax.Id "fish")
--         (prepareParser idParser "fish")
--     ]

-- keywordParserTests =
--   testGroup
--     "Keyword Parser Tests"
--     [ timedAssertEqual
--         2
--         "Parse a fish keyword."
--         []
--         (Syntax.Fish)
--         (prepareParser (genericKeywordParser Syntax.Fish) "fish"),
--       timedAssertEqual
--         2
--         "Parse a shoal keyword."
--         []
--         (Syntax.Shoal)
--         (prepareParser (genericKeywordParser Syntax.Shoal) "shoal"),
--       timedAssertEqual
--         2
--         "Parse a lamprey keyword."
--         []
--         (Syntax.Lamprey)
--         (prepareParser (genericKeywordParser Syntax.Lamprey) "lamprey")
--     ]

-- functionCallParserTests =
--   testGroup
--     "Function Call Parser Tests"
--     [ timedAssertEqual
--         2
--         "Parse a simple function call."
--         []
--         [treeId Return "or" -<= (listIds Send ["x", "y"])]
--         (prepareParser (functionCallParser Return) "or >(x)> >(y)>")
--     ]

-- fishBindParserTests =
--   testGroup
--     "FishBind Parser Tests"
--     [ timedAssertEqual
--         2
--         "Parse a simple fishBind."
--         []
--         [treeId Send "bind_this" -<= [treeId Return "x"]]
--         (prepareParser fishBindParser "bind_this <(x)<"),
--       timedAssertEqual
--         2
--         "Parse a fishBind bound to a lamprey."
--         []
--         [ treeId Send "bind_this"
--             -<= [ treeKeyword Return Lamprey
--                     -<= [ treeId Send "x",
--                           (treeSU Return . Data . Boolean) True
--                         ]
--                 ]
--         ]
--         (prepareParser fishBindParser "bind_this <(lamprey >(x)> <(True)<)<")
--     ]

-- lampreyAndFunctionParserTests =
--   testGroup
--     "Lamprey and FunctionDefinition Parser Tests"
--     [ timedAssertEqual
--         2
--         "Parse a simple Lamprey."
--         []
--         [ treeKeyword Return Lamprey
--             -<= [ treeId Send "x",
--                   treeId Send "y",
--                   treeId Return "+"
--                     -<= [ treeId Send "x",
--                           treeId Send "y"
--                         ]
--                 ]
--         ]
--         ( prepareParser
--             (lampreyParser Syntax.Return)
--             "lamprey >(x)> >(y)> <(+ >(x)> >(y)>)<"
--         ),
--       timedAssertEqual
--         2
--         "Parse a simple function definition."
--         []
--         [ treeKeyword Return Fish
--             -<= [ treeId Return "add"
--                     -<= [ treeKeyword Return Lamprey
--                             -<= [ treeId Send "x",
--                                   treeId Send "y",
--                                   treeId Return "+"
--                                     -<= [ treeId Send "x",
--                                           treeId Send "y"
--                                         ]
--                                 ]
--                         ]
--                 ]
--         ]
--         ( prepareParser
--             (functionDefinitionParser Syntax.Return)
--             "fish add lamprey\
--             \ >(x)> >(y)> <(+ >(x)> >(y)>)<"
--         ),
--       timedAssertEqual
--         2
--         "A lamprey does not need an explicit keyword."
--         []
--         [ treeKeyword Return Lamprey
--             -<= [ treeId Send "x",
--                   treeId Send "y",
--                   treeId Return "+"
--                     -<= [ treeId Send "x",
--                           treeId Send "y"
--                         ]
--                 ]
--         ]
--         ( prepareParser
--             (lampreyParser Return)
--             ">(x)> \
--             \>(y)> \
--             \<(+ >(x)> >(y)>)<"
--         ),
--       -- LAMPREY WITH DEFINED CO-FUNCTION AS PARAMETER.
--       timedAssertEqual
--         2
--         "Parse a lamprey with a scoped function definition."
--         []
--         [ treeKeyword Syntax.Return Lamprey
--             -<= [ treeId Send "x",
--                   treeKeyword Send Fish
--                     -<= [ treeId Send "sub"
--                             -- It makes sense that this is Return because calling the
--                             -- function's id should return this lambda function.
--                             -<= [ treeKeyword Return Lamprey
--                                     -<= [ treeId Send "x",
--                                           treeId Send "y",
--                                           treeId Return "-"
--                                             -<= (listIds Send ["x", "y"])
--                                         ]
--                                 ]
--                         ],
--                   -- The 'x' and 'y' are parsed as Return but they should definitely
--                   -- be Send
--                   treeId Return "+" -<= (listIds Send ["x", "y"])
--                 ]
--         ]
--         ( prepareParser
--             (lampreyParser Syntax.Return)
--             "lamprey\
--             \ >(x)>\
--             \ >(fish sub >(x)> >(y)> <(- >(x)> >(y)>)<)>\
--             \ <(+ >(x)> >(y)>)<"
--         ),
--       timedAssertEqual
--         2
--         "Parse a function definition that returns an explicit lamprey."
--         []
--         [ treeKeyword Return Fish
--             -<= [ treeId Return "return_part"
--                     -<= [ treeKeyword Return Lamprey
--                             -<= [ treeId Send "x",
--                                   treeKeyword Return Lamprey
--                                     -<= [ treeId Send "y",
--                                           treeId Return "+"
--                                             -<= [ treeId Send "x",
--                                                   treeId Send "sqrt"
--                                                     -<= [treeId Send "y"]
--                                                 ]
--                                         ]
--                                 ]
--                         ]
--                 ]
--         ]
--         ( prepareParser
--             (functionDefinitionParser Return)
--             "fish return_part \
--             \lamprey \
--             \>(x)> \
--             \<( lamprey \
--             \    >(y)> \
--             \    <(+ >(x)> >(sqrt >(y)>)>)< \
--             \)<"
--         ),
--       timedAssertEqual
--         2
--         "Parse a function definition that returns an implicit lamprey"
--         []
--         [ treeKeyword Return Fish
--             -<= [ treeId Return "return_part"
--                     -<= [ treeKeyword Return Lamprey
--                             -<= [ treeId Send "x",
--                                   treeKeyword Return Lamprey
--                                     -<= [ treeId Send "y",
--                                           treeId Return "+"
--                                             -<= [ treeId Send "x",
--                                                   treeId Send "sqrt"
--                                                     -<= [treeId Send "y"]
--                                                 ]
--                                         ]
--                                 ]
--                         ]
--                 ]
--         ]
--         ( prepareParser
--             (functionDefinitionParser Return)
--             "fish return_part \
--             \lamprey \
--             \>(x)> \
--             \<( \
--             \    >(y)> \
--             \    <(+ >(x)> >(sqrt >(y)>)>)< \
--             \)<"
--         ),
--       timedAssertEqual
--         2
--         "Parse a function that contains a swim block."
--         []
--         [ treeKeyword Return Fish
--             -<= [ treeId Return "do_swim"
--                     -<= [ treeKeyword Return Lamprey
--                             -<= [ treeId Send "x",
--                                   treeKeyword Return Swim
--                                     -<= [ treeId Send "bind_this"
--                                             -<= [treeId Return "x"],
--                                           treeId Send "trout"
--                                             -<= [ treeId Send "to_string"
--                                                     -<= [treeId Send "bind_this"]
--                                                 ],
--                                           treeId Return "+"
--                                             -<= [ (treeSU Send . dataNum) 1,
--                                                   treeId Send "bind_this"
--                                                 ]
--                                         ]
--                                 ]
--                         ]
--                 ]
--         ]
--         ( prepareParser
--             (functionDefinitionParser Return)
--             "fish do_swim >(x)> \
--             \<( \
--             \swim \
--             \    >(bind_this <(x)<)> \
--             \    >(trout >( to_string >(bind_this)>)>)> \
--             \    <(+ >(1)> >(bind_this)>)< \
--             \)<"
--         ),
--       timedAssertEqual
--         2
--         "Parse a function with a swim block that contains a fishbind with a lamprey."
--         []
--         [ treeKeyword Return Fish
--             -<= [ treeId Return "do_swim"
--                     -<= [ treeKeyword Return Lamprey
--                             -<= [ treeId Send "x",
--                                   treeKeyword Return Swim
--                                     -<= [ treeId Send "bind_this"
--                                             -<= [ treeKeyword Return Lamprey
--                                                     -<= [ treeId Send "z",
--                                                           treeId Return "z"
--                                                         ]
--                                                 ],
--                                           treeId Send "trout"
--                                             -<= [ treeId Send "to_string"
--                                                     -<= [ treeId Send "bind_this"
--                                                             -<= [treeId Send "x"]
--                                                         ]
--                                                 ],
--                                           treeId Return "+"
--                                             -<= [ (treeSU Send . dataNum) 1,
--                                                   treeId Send "bind_this"
--                                                     -<= [treeId Send "x"]
--                                                 ]
--                                         ]
--                                 ]
--                         ]
--                 ]
--         ]
--         ( prepareParser
--             (functionDefinitionParser Return)
--             "fish do_swim >(x)> \
--             \<( \
--             \swim \
--             \    >(bind_this <(lamprey >(z)> <(z)<)<)> \
--             \    >(trout >( to_string >(bind_this >(x)>)>)>)> \
--             \    <(+ >(1)> >(bind_this >(x)>)>)< \
--             \)<"
--         ),
--       timedAssertEqual
--         2
--         "Parse a function definition with multiple nested swim or function blocks."
--         []
--         [ treeKeyword Return Fish
--             -<= [ treeId Return "top"
--                     -<= [ treeKeyword Return Lamprey
--                             -<= [ treeId Send "x",
--                                   treeId Send "y",
--                                   treeKeyword Send Fish
--                                     --I'm not really sure what scopetype this is supposed
--                                     --to have.
--                                     -<= [ treeId Send "middle"
--                                             -<= [ treeKeyword Return Lamprey
--                                                     -<= [ treeId Send "z",
--                                                           treeKeyword Return Swim
--                                                             -<= [ treeId
--                                                                     Send
--                                                                     "bind_middle"
--                                                                     -<= [ ( treeSU Return
--                                                                               . Data
--                                                                               . Boolean
--                                                                           )
--                                                                             True
--                                                                         ],
--                                                                   treeKeyword
--                                                                     Return
--                                                                     Lamprey
--                                                                     -<= [ treeId Send "h",
--                                                                           ( treeSU Return
--                                                                               . Data
--                                                                               . Boolean
--                                                                           )
--                                                                             False
--                                                                         ]
--                                                                 ]
--                                                         ]
--                                                 ]
--                                         ],
--                                   treeKeyword Return Swim
--                                     -<= [ treeId Send "trout"
--                                             -<= [ ( treeSU Send
--                                                       . Data
--                                                       . String
--                                                   )
--                                                     "Hello"
--                                                 ],
--                                           treeId Send "bind_top"
--                                             -<= [ treeId Return "middle"
--                                                     -<= [ ( treeSU Send
--                                                               . Data
--                                                               . Boolean
--                                                           )
--                                                             False
--                                                         ]
--                                                 ],
--                                           treeKeyword Return Lamprey
--                                             -<= [ treeId Send "q",
--                                                   treeId Return "or"
--                                                     -<= [ treeId Send "q",
--                                                           treeId Send "bind_top"
--                                                         ]
--                                                 ]
--                                         ]
--                                 ]
--                         ]
--                 ]
--         ]
--         ( prepareParser
--             (functionDefinitionParser Return)
--             "fish top \
--             \ >(x)> >(y)> \
--             \ >(fish middle >(z)> <( \
--             \ swim \
--             \   >(bind_middle <(True)< )> \
--             \   <(lamprey >(h)> <(False)< )< )< )> \
--             \ <( \
--             \   swim \
--             \     >(trout >(\"Hello\")>)> \
--             \     >(bind_top <(middle >(False)>)<)> \
--             \     <( >(q)> <( or >(q)> >(bind_top)> )< )< \
--             \ )<"
--         )
--     ]
