module Testgraph exposing (..)


nodes =
    [ { nodeId = 0
      , label = "A"
      , truth = 0.8
      , indeterminacy = 0.1
      , falsehood = 0.1
      , state = "Tar"
      , linkState = "Rec"
      , inDegree = 1
      , outDegree = 0
      }
    , { nodeId = 1
      , label = "B"
      , truth = 0.8
      , indeterminacy = 0.1
      , falsehood = 0.3
      , state = "Reg"
      , linkState = "Ord"
      , inDegree = 3
      , outDegree = 2
      }
    , { nodeId = 2
      , label = "C"
      , truth = 0.7
      , indeterminacy = 0.1
      , falsehood = 0.1
      , state = "Sim"
      , linkState = "Tra"
      , inDegree = 0
      , outDegree = 2
      }
    , { nodeId = 3
      , label = "D"
      , truth = 0.8
      , indeterminacy = 0.2
      , falsehood = 0.1
      , state = "Reg"
      , linkState = "Ord"
      , inDegree = 1
      , outDegree = 1
      }
    , { nodeId = 4
      , label = "E"
      , truth = 0.7
      , indeterminacy = 0.4
      , falsehood = 0.2
      , state = "Reg"
      , linkState = "Tra"
      , inDegree = 0
      , outDegree = 2
      }
    , { nodeId = 5
      , label = "F"
      , truth = 0.8
      , indeterminacy = 0.3
      , falsehood = 0.4
      , state = "Reg"
      , linkState = "Rec"
      , inDegree = 2
      , outDegree = 0
      }
    , { nodeId = 6
      , label = "A1"
      , truth = 0.8
      , indeterminacy = 0.1
      , falsehood = 0.1
      , state = "Tar"
      , linkState = "Rec"
      , inDegree = 1
      , outDegree = 0
      }
    , { nodeId = 7
      , label = "B1"
      , truth = 0.8
      , indeterminacy = 0.1
      , falsehood = 0.3
      , state = "Reg"
      , linkState = "Ord"
      , inDegree = 3
      , outDegree = 2
      }
    , { nodeId = 8
      , label = "C1"
      , truth = 0.7
      , indeterminacy = 0.1
      , falsehood = 0.1
      , state = "Sim"
      , linkState = "Tra"
      , inDegree = 0
      , outDegree = 2
      }
    , { nodeId = 9
      , label = "D1"
      , truth = 0.8
      , indeterminacy = 0.2
      , falsehood = 0.1
      , state = "Reg"
      , linkState = "Ord"
      , inDegree = 1
      , outDegree = 1
      }
    , { nodeId = 10
      , label = "E1"
      , truth = 0.7
      , indeterminacy = 0.4
      , falsehood = 0.2
      , state = "Reg"
      , linkState = "Tra"
      , inDegree = 0
      , outDegree = 2
      }
    , { nodeId = 11
      , label = "F1"
      , truth = 0.8
      , indeterminacy = 0.3
      , falsehood = 0.4
      , state = "Reg"
      , linkState = "Rec"
      , inDegree = 2
      , outDegree = 0
      }
    , { nodeId = 12
      , label = "A2"
      , truth = 0.8
      , indeterminacy = 0.1
      , falsehood = 0.1
      , state = "Tar"
      , linkState = "Rec"
      , inDegree = 1
      , outDegree = 0
      }
    , { nodeId = 13
      , label = "B2"
      , truth = 0.8
      , indeterminacy = 0.1
      , falsehood = 0.3
      , state = "Reg"
      , linkState = "Ord"
      , inDegree = 3
      , outDegree = 2
      }
    , { nodeId = 14
      , label = "C2"
      , truth = 0.7
      , indeterminacy = 0.1
      , falsehood = 0.1
      , state = "Sim"
      , linkState = "Tra"
      , inDegree = 0
      , outDegree = 2
      }
    , { nodeId = 15
      , label = "D2"
      , truth = 0.8
      , indeterminacy = 0.2
      , falsehood = 0.1
      , state = "Reg"
      , linkState = "Ord"
      , inDegree = 1
      , outDegree = 1
      }
    , { nodeId = 16
      , label = "E2"
      , truth = 0.7
      , indeterminacy = 0.4
      , falsehood = 0.2
      , state = "Reg"
      , linkState = "Tra"
      , inDegree = 0
      , outDegree = 2
      }
    , { nodeId = 17
      , label = "F2"
      , truth = 0.8
      , indeterminacy = 0.3
      , falsehood = 0.4
      , state = "Reg"
      , linkState = "Rec"
      , inDegree = 2
      , outDegree = 0
      }
    , { nodeId = 18
      , label = "A3"
      , truth = 0.8
      , indeterminacy = 0.1
      , falsehood = 0.1
      , state = "Tar"
      , linkState = "Rec"
      , inDegree = 1
      , outDegree = 0
      }
    , { nodeId = 19
      , label = "B3"
      , truth = 0.8
      , indeterminacy = 0.1
      , falsehood = 0.3
      , state = "Reg"
      , linkState = "Ord"
      , inDegree = 3
      , outDegree = 2
      }
    , { nodeId = 20
      , label = "C3"
      , truth = 0.7
      , indeterminacy = 0.1
      , falsehood = 0.1
      , state = "Sim"
      , linkState = "Tra"
      , inDegree = 0
      , outDegree = 2
      }
    , { nodeId = 21
      , label = "D3"
      , truth = 0.8
      , indeterminacy = 0.2
      , falsehood = 0.1
      , state = "Reg"
      , linkState = "Ord"
      , inDegree = 1
      , outDegree = 1
      }
    , { nodeId = 22
      , label = "E3"
      , truth = 0.7
      , indeterminacy = 0.4
      , falsehood = 0.2
      , state = "Reg"
      , linkState = "Tra"
      , inDegree = 0
      , outDegree = 2
      }
    , { nodeId = 23
      , label = "F3"
      , truth = 0.8
      , indeterminacy = 0.3
      , falsehood = 0.4
      , state = "Reg"
      , linkState = "Rec"
      , inDegree = 2
      , outDegree = 0
      }
    , { nodeId = 24
      , label = "A4"
      , truth = 0.8
      , indeterminacy = 0.1
      , falsehood = 0.1
      , state = "Tar"
      , linkState = "Rec"
      , inDegree = 1
      , outDegree = 0
      }
    , { nodeId = 25
      , label = "B4"
      , truth = 0.8
      , indeterminacy = 0.1
      , falsehood = 0.3
      , state = "Reg"
      , linkState = "Ord"
      , inDegree = 3
      , outDegree = 2
      }
    , { nodeId = 26
      , label = "C4"
      , truth = 0.7
      , indeterminacy = 0.1
      , falsehood = 0.1
      , state = "Sim"
      , linkState = "Tra"
      , inDegree = 0
      , outDegree = 2
      }
    , { nodeId = 27
      , label = "D4"
      , truth = 0.8
      , indeterminacy = 0.2
      , falsehood = 0.1
      , state = "Reg"
      , linkState = "Ord"
      , inDegree = 1
      , outDegree = 1
      }
    , { nodeId = 28
      , label = "E4"
      , truth = 0.7
      , indeterminacy = 0.4
      , falsehood = 0.2
      , state = "Reg"
      , linkState = "Tra"
      , inDegree = 0
      , outDegree = 2
      }
    , { nodeId = 29
      , label = "F4"
      , truth = 0.8
      , indeterminacy = 0.3
      , falsehood = 0.4
      , state = "Reg"
      , linkState = "Rec"
      , inDegree = 2
      , outDegree = 0
      }
    ]


edges =
    [ { edgeId = 0
      , from = 1
      , to = 0
      , truth = 0.7
      , indeterminacy = 0.1
      , falsehood = 0.1
      }
    , { edgeId = 1
      , from = 2
      , to = 1
      , truth = 0.6
      , indeterminacy = 0.1
      , falsehood = 0.2
      }
    , { edgeId = 2
      , from = 2
      , to = 3
      , truth = 0.5
      , indeterminacy = 0.1
      , falsehood = 0.5
      }
    , { edgeId = 3
      , from = 4
      , to = 1
      , truth = 0.3
      , indeterminacy = 0.1
      , falsehood = 0.7
      }
    , { edgeId = 4
      , from = 4
      , to = 5
      , truth = 0.7
      , indeterminacy = 0.1
      , falsehood = 0.4
      }
    , { edgeId = 5
      , from = 1
      , to = 5
      , truth = 0.7
      , indeterminacy = 0.1
      , falsehood = 0.4
      }
    , { edgeId = 6
      , from = 3
      , to = 1
      , truth = 0.7
      , indeterminacy = 0.1
      , falsehood = 0.4
      }
    ]



--, { from = 0, to = 1 }
--, { from = 1, to = 15 }
--, { from = 2, to = 13 }
--, { from = 3, to = 12 }
--, { from = 4, to = 10 }
--, { from = 5, to = 12 }
--, { from = 6, to = 2 }
--, { from = 7, to = 9 }
--, { from = 8, to = 11 }
--, { from = 9, to = 0 }
--, { from = 10, to = 1 }
--, { from = 11, to = 19 }
--, { from = 12, to = 18 }
--, { from = 13, to = 17 }
--, { from = 14, to = 16 }
--, { from = 20, to = 15 }
--, { from = 28, to = 14 }
--, { from = 29, to = 13 }
--, { from = 15, to = 12 }
--, { from = 16, to = 11 }
--, { from = 17, to = 10 }
--, { from = 18, to = 1 }
--, { from = 19, to = 11 }
--, { from = 26, to = 18 }
--, { from = 27, to = 22 }
--, { from = 29, to = 21 }
--]
