module Testgraph exposing (..)


nodes =
    [ { nodeId = 0
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 1
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 2
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 3
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 4
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 5
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 6
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 7
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 8
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 9
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 10
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 11
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 12
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 13
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 14
      , label = "t"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 15
      , label = "t15"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 16
      , label = "t16"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 1
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 2
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 3
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 4
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 5
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 6
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 7
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 8
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 9
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 10
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 11
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 12
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 13
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 14
      , label = "t"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 15
      , label = "t15"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 32
      , label = "t16"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 1
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 2
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 3
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 4
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 5
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 6
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 7
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 8
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 9
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 10
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 11
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 12
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 13
      , label = "t0"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 14
      , label = "t"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 15
      , label = "t15"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    , { nodeId = 48
      , label = "t16"
      , state = "Reg"
      , truth = 0.55
      , indeterminacy = 0.55
      , falsehood = 0.55
      }
    ]


edges =
    [ { from = 0, to = 1 }
    , { from = 1, to = 15 }
    , { from = 2, to = 13 }
    , { from = 3, to = 12 }
    , { from = 4, to = 10 }
    , { from = 5, to = 12 }
    , { from = 6, to = 2 }
    , { from = 7, to = 9 }
    , { from = 8, to = 11 }
    , { from = 9, to = 0 }
    , { from = 10, to = 1 }
    , { from = 11, to = 19 }
    , { from = 12, to = 18 }
    , { from = 13, to = 17 }
    , { from = 14, to = 16 }
    , { from = 20, to = 15 }
    , { from = 30, to = 14 }
    , { from = 40, to = 13 }
    , { from = 15, to = 12 }
    , { from = 16, to = 11 }
    , { from = 17, to = 10 }
    , { from = 18, to = 1 }
    , { from = 19, to = 11 }
    , { from = 31, to = 21 }
    , { from = 32, to = 31 }
    , { from = 33, to = 41 }
    ]
