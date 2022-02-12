import Parser.Syntax
import Test.Core
import Util.Tree

main = defaultMain tests

tests =
  testGroup
    "Parser.Syntax Tests"
    [labeledTreeFunctionTests]

labeledTreeFunctionTests =
  testGroup
    "Labeled Tree Function Tests"
    [ timedAssertEqual
        1
        "Can lift a whole arbitrary sized tree to a uniform label"
        []
        ( LabeledTree
            NoLabel
            1
            [ LabeledTree NoLabel 2 [],
              LabeledTree
                NoLabel
                3
                [ LabeledTree NoLabel 4 [LabeledTree NoLabel 5 []]
                ],
              LabeledTree NoLabel 6 []
            ]
        )
        ( liftTreeWithLabel
            NoLabel
            (tree 1 -<= [tree 2, tree 3 -<= [tree 4 -<- tree 5], tree 6])
        ),
      timedAssertEqual
        1
        "Can join an arbitrary sized tree with a uniform label"
        []
        (tree 1 -<= [tree 2, tree 3 -<= [tree 4 -<- tree 5], tree 6])
        ( joinLabeledTree
            ( LabeledTree
                NoLabel
                1
                [ LabeledTree NoLabel 2 [],
                  LabeledTree
                    NoLabel
                    3
                    [ LabeledTree NoLabel 4 [LabeledTree NoLabel 5 []]
                    ],
                  LabeledTree NoLabel 6 []
                ]
            )
        ),
      timedAssertEqual
        1
        "Can join a labeled tree with arbitrary labels"
        []
        ( tree 1
            -<= [ tree 2,
                  tree 3
                    -<= [ tree 4 -<- tree 5
                        ],
                  tree 6
                    -<= [ tree 7 -<- tree 8,
                          tree 9
                            -<= [ tree 10,
                                  tree 11,
                                  tree 12
                                ]
                        ]
                ]
        )
        ( joinLabeledTree
            ( LabeledTree
                NoLabel
                1
                [ LabeledTree NoLabel 2 [],
                  LabeledTree
                    NoLabel
                    3
                    [ LabeledTree NoLabel 4 [LabeledTree NoLabel 5 []]
                    ],
                  LabeledTree
                    NoLabel
                    6
                    [ LabeledTree Literal 7 [LabeledTree Literal 8 []],
                      LabeledTree
                        IdCallExpr
                        9
                        [ LabeledTree IdCallExpr 10 [],
                          LabeledTree NoLabel 11 [],
                          LabeledTree NoLabel 12 []
                        ]
                    ]
                ]
            )
        )
    ]