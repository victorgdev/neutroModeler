port module Main exposing (..)

{-
   TODO
         Set local storage port example code
         --------------------------------------------------------------------------------------
         , update = updateWithStorage
         , subscriptions = \_ -> Sub.none
         }

         port setStorage : List NeutroNode -> Cmd msg

         updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
         updateWithStorage msg model =
         let
         ( newModel, cmds ) =
             update msg model
         in
         ( newModel
         , Cmd.batch [ setStorage newModel.nodes, cmds ]
         )
         --------------------------------------------------------------------------------------
-}

import Browser
import Color exposing (Color)
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html exposing (..)
import Html.Attributes exposing (class, disabled, hidden, id, placeholder, required, step, style, type_, value)
import Html.Events exposing (..)
import IntDict
import Json.Decode as Decode exposing (Decoder, Value, float, int, list, string)
import Json.Decode.Pipeline as Decode
import List exposing (range)
import List.Extra
import Round
import String exposing (concat)
import Tuple exposing (first, pair, second)
import TypedSvg exposing (circle, g, line, polygon, svg)
import TypedSvg.Attributes exposing (fill, points, stroke, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..))



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


port sendModel : NeutroModel -> Cmd msg


port messageReceiver : (Value -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver Recv



-- MODEL


type alias Model =
    -- Elements
    { nodes : List NeutroNode
    , edges : List NeutroEdge
    , simulatedNodes : List NeutroNode
    , targetNodes : List TargetNode
    , neutroModel : NeutroModel
    , nodeLabelPairs : List NodeLabelPair
    , simLabels : List String
    , nodeLabels : List String
    , targetLabels : List String
    , edgePairs : List ( Int, Int )

    -- Forms
    , nodeForm : NodeForm
    , edgeForm : EdgeForm
    , simulationForm : SimulationForm
    , targetNodeForm : TargetNodeForm
    , nodeFormDisplay : Bool
    , edgeFormDisplay : Bool
    , simFormDisplay : Bool
    , targetFormDisplay : Bool
    , disableEdgeForm : String
    , disableSimForm : String
    , disableTargetForm : String
    , disableRunButton : Bool
    , disableDeleteButton : Bool
    , disableFormBtn : Bool

    -- Tables
    , nodeTableDisplay : Bool
    , edgeTableDisplay : Bool
    , currentStateTabDisplay : Bool
    , simulatedStateTabDisplay : Bool
    , simulatedStateResultDisplay : Bool

    -- KPIs
    , listTransmitters : List Int
    , listReceivers : List Int
    , listOrdinaries : List Int
    , numConcepts : Int -- number of nodes
    , numConnections : Int -- number of edges
    , numTransmitters : Int -- number of nodes that transmit impact (edges)
    , numReceivers : Int -- number of nodes that receive impact (edges)
    , numOrdinary : Int -- number of nodes that transmit and receive impact (edges)
    , cnScore : Float -- connections (edges) / concepts (nodes) ratio
    , complexityScore : Float -- transmitters nodes / receivers nodes ratio
    , densityScore : Float -- connections (edges) / (concepts (nodes) * (concepts (nodes) - 1)) ratio

    -- Ports testing
    , simulationResult : NeutroResult
    }



-- TYPES


type alias NodeLabelPair =
    ( Int, String )


type alias NeutroModel =
    { modelNodes : List NeutroNode
    , modelEdges : List NeutroEdge
    }


type alias NeutroNode =
    { nodeId : Int
    , label : String
    , state : String
    , linkState : String

    -- Neutro Number
    , truth : Float
    , indeterminacy : Float
    , falsehood : Float
    , inDegree : Int
    , outDegree : Int
    }


type alias NeutroEdge =
    { edgeId : Int
    , from : Int
    , to : Int

    -- Neutro Number
    , truth : Float
    , indeterminacy : Float
    , falsehood : Float
    }


type alias TargetNode =
    { targetNodeId : NodeId
    , targetNodeLabel : String
    , state : String
    }


type alias ResultNode =
    { nodeId : Int
    , label : String
    , state : String
    , truth : Float
    , indeterminacy : Float
    , falsehood : Float
    }


type alias NeutroResult =
    List ResultNode



-- FORMS


type Nid
    = Nid (Maybe Int) String


type NeutroField
    = NeutroField (Maybe Float) String


type alias NodeForm =
    { nodeId : Int
    , label : String
    , truth : NeutroField
    , indeterminacy : NeutroField
    , falsehood : NeutroField
    , hideForm : Bool
    , hideTable : Bool
    , state : String
    , linkState : String
    , inDegree : Int
    , outDegree : Int
    }


type alias EdgeForm =
    { edgeId : Int
    , from : Nid
    , to : Nid
    , truth : NeutroField
    , indeterminacy : NeutroField
    , falsehood : NeutroField
    , hideForm : Bool
    }


type alias SimulationForm =
    { nodeId : Int
    , simLabel : String
    , truth : NeutroField
    , indeterminacy : NeutroField
    , falsehood : NeutroField
    , hideForm : Bool
    , hideTable : Bool
    , state : String
    , linkState : String
    , inDegree : Int
    , outDegree : Int
    }


type alias TargetNodeForm =
    { targetNodeId : Int
    , targetNodeLabel : String
    , hideForm : Bool
    , state : String
    }



-- GRAPH TYPES


type alias Entity =
    Force.Entity NodeId { value : CustomNode }


type alias CustomNode =
    { rank : Int, name : String }



-- MODEL DEFAULTS


defaultNeutroModel : NeutroModel
defaultNeutroModel =
    { modelNodes = []
    , modelEdges = []
    }


defaultNodeForm : NodeForm
defaultNodeForm =
    { nodeId = 0
    , label = ""
    , truth = NeutroField Nothing ""
    , indeterminacy = NeutroField Nothing ""
    , falsehood = NeutroField Nothing ""
    , hideForm = True
    , hideTable = True
    , state = "Reg"
    , linkState = ""
    , inDegree = 0
    , outDegree = 0
    }


defaultEdgeForm : EdgeForm
defaultEdgeForm =
    { edgeId = 0
    , from = Nid Nothing ""
    , to = Nid Nothing ""
    , truth = NeutroField Nothing ""
    , indeterminacy = NeutroField Nothing ""
    , falsehood = NeutroField Nothing ""
    , hideForm = True
    }


defaultSimulationForm : SimulationForm
defaultSimulationForm =
    { nodeId = 0
    , simLabel = ""
    , truth = NeutroField Nothing ""
    , indeterminacy = NeutroField Nothing ""
    , falsehood = NeutroField Nothing ""
    , hideForm = True
    , hideTable = True
    , state = "Reg"
    , linkState = ""
    , inDegree = 0
    , outDegree = 0
    }


defaultTargetNodeForm : TargetNodeForm
defaultTargetNodeForm =
    { targetNodeId = 0
    , targetNodeLabel = ""
    , hideForm = True
    , state = "Tar"
    }


errorNode : NeutroNode
errorNode =
    { nodeId = -1
    , label = "Error"
    , truth = 0
    , indeterminacy = 0
    , falsehood = 0
    , state = "Error"
    , linkState = "Error"
    , inDegree = -1
    , outDegree = -1
    }


initialFormView =
    "m-0 p-1 bg-dark btn btn-outline-secondary text-center"



-- INIT MODEL


initModel : Model
initModel =
    -- Elements
    { nodes = []
    , edges = []
    , simulatedNodes = []
    , targetNodes = []
    , neutroModel = defaultNeutroModel
    , nodeLabelPairs = []
    , simLabels = []
    , nodeLabels = []
    , targetLabels = []
    , edgePairs = []

    -- Forms
    , nodeForm = defaultNodeForm
    , edgeForm = defaultEdgeForm
    , simulationForm = defaultSimulationForm
    , targetNodeForm = defaultTargetNodeForm
    , nodeFormDisplay = True
    , edgeFormDisplay = True
    , simFormDisplay = True
    , targetFormDisplay = True
    , disableEdgeForm = initialFormView
    , disableSimForm = initialFormView
    , disableTargetForm = initialFormView
    , disableRunButton = True
    , disableDeleteButton = True
    , disableFormBtn = False

    -- Tables
    , nodeTableDisplay = False
    , edgeTableDisplay = False
    , currentStateTabDisplay = False
    , simulatedStateTabDisplay = True
    , simulatedStateResultDisplay = True

    -- KPIs
    , listTransmitters = []
    , listReceivers = []
    , listOrdinaries = []
    , numConcepts = 0
    , numConnections = 0
    , numTransmitters = 0
    , numReceivers = 0
    , numOrdinary = 0
    , cnScore = 0.0
    , complexityScore = 0.0
    , densityScore = 0.0

    -- Ports
    , simulationResult = []
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Cmd.none
    )



-- GRAPH DEFAULTS


w : Float
w =
    990


h : Float
h =
    800


initGraph : Model -> Graph Entity ()
initGraph model =
    let
        graph =
            Graph.mapContexts
                (\{ node, incoming, outgoing } ->
                    { incoming = incoming
                    , outgoing = outgoing
                    , node =
                        { label =
                            Force.entity node.id
                                (CustomNode
                                    (IntDict.size incoming + IntDict.size outgoing)
                                    node.label
                                )
                        , id = node.id
                        }
                    }
                )
                (neutroGraph model)

        links =
            graph
                |> Graph.edges
                |> List.map
                    (\{ from, to } ->
                        { source = from
                        , target = to
                        , distance = 200
                        , strength = Nothing
                        }
                    )

        forces =
            [ Force.customLinks 1 links
            , Force.manyBodyStrength -30 <| List.map .id <| Graph.nodes graph
            , Force.center (w / 2) (h / 2)
            ]
    in
    Graph.nodes graph
        |> List.map .label
        |> Force.computeSimulation (Force.simulation forces)
        |> updateGraphWithList graph


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


updateContextWithValue : { a | node : { b | label : c } } -> c -> { a | node : { b | label : c } }
updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = value } }


linkElement : Graph Entity () -> Edge () -> Svg msg
linkElement graph edge =
    let
        retrieveEntity =
            Maybe.withDefault (Force.entity 0 (CustomNode 0 "")) << Maybe.map (.node >> .label)

        source =
            retrieveEntity <| Graph.get edge.from graph

        target =
            retrieveEntity <| Graph.get edge.to graph
    in
    line
        [ strokeWidth 2
        , stroke <| Paint <| Color.rgba 255 255 255 1
        , x1 source.x
        , y1 source.y
        , x2 target.x
        , y2 target.y
        ]
        []


hexagon ( x, y ) size attrs =
    let
        angle =
            2 * pi / 360

        p =
            range 0 360
                |> List.map toFloat
                |> List.map (\a -> ( x + cos (a * angle) * 2 * size, y + sin (a * angle) * 2 * size ))
                |> points
    in
    polygon
        (p :: attrs)


nodeSize size node =
    hexagon ( node.x, node.y )
        size
        [ fill <| Paint <| Color.rgba 255 255 255 1
        , stroke <| Paint <| Color.rgba 0 0 0 1
        ]
        [ TypedSvg.title [] [ text node.value.name ] ]


nodeElement node =
    nodeSize 6 node.label


neutroGraph : Model -> Graph String ()
neutroGraph model =
    let
        nodeList =
            List.map
                (\node ->
                    concat
                        [ node.label
                        , "\n("
                        , " tru : "
                        , String.fromFloat node.truth
                        , "   ind : "
                        , String.fromFloat node.indeterminacy
                        , "   fal : "
                        , String.fromFloat node.falsehood
                        , " )"
                        ]
                )
                model.nodes

        edgeList =
            List.map (\edge -> ( edge.from, edge.to )) model.edges
    in
    Graph.fromNodeLabelsAndEdgePairs
        nodeList
        edgeList



-- UPDATE


type Msg
    = NoOp
    | RunSimulation
      -- Adding
    | AddNode
    | AddEdge
    | AddSimNode
    | AddTargetNode
      -- Deleting
    | DeleteNode Int
    | DeleteEdge Int
    | DeleteModel
      -- Node
    | UpdateNodeLabel String
    | UpdateNodeTruth String
    | UpdateNodeIndeterminacy String
    | UpdateNodeFalsehood String
      -- Edge
    | UpdateEdgeOriginNode String
    | UpdateEdgeDestinyNode String
    | UpdateEdgeTruth String
    | UpdateEdgeIndeterminacy String
    | UpdateEdgeFalsehood String
      -- Simulation Node
    | UpdateSimNodeLabel String
    | UpdateSimNodeTruth String
    | UpdateSimNodeIndeterminacy String
    | UpdateSimNodeFalsehood String
      -- Target Node
    | UpdateTargetNodeLabel String
      -- Forms
    | DisplayNodeForm
    | DisplayEdgeForm
    | DisplaySimForm
    | DisplayTargetForm
      -- Tables
    | DisplayNodeTable
    | DisplayEdgeTable
    | DisplayCurState
    | DisplaySimState
      -- Ports
    | Recv Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        numNodes =
            toFloat model.numConcepts

        numEdges =
            toFloat model.numConnections

        numSimNodes =
            List.length model.simulatedNodes + 1

        numTargetNodes =
            List.length model.targetNodes + 1

        numTransmitterNodes =
            toFloat model.numTransmitters

        numReceiverNodes =
            toFloat model.numReceivers

        newCnScore =
            isNaNChecked (cnScore numEdges numNodes)

        newComplexityScore =
            isNaNChecked (complexityScore numTransmitterNodes numReceiverNodes)

        newDensityScore =
            isNaNChecked (densityScore numEdges numNodes)

        isButtonEnabled =
            enabledButton model.nodes model.edges numSimNodes numTargetNodes

        isEdgeFormEnabled =
            enabledFormView model.nodes

        isNodeSimFormEnabled =
            enabledFormView model.nodes

        isTargetFormEnabled =
            enabledFormView model.nodes
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RunSimulation ->
            let
                finalNeutroModel =
                    { modelNodes = model.nodes
                    , modelEdges = model.edges
                    }
            in
            ( { model | simulatedStateResultDisplay = False }, sendModel finalNeutroModel )

        AddNode ->
            let
                newNodeId =
                    List.length model.nodes

                newNode =
                    let
                        newNodeLabel =
                            model.nodeForm.label

                        newTruth =
                            neutroNumberCheck model.nodeForm.truth

                        newIndeterminacy =
                            neutroNumberCheck model.nodeForm.indeterminacy

                        newFalsehood =
                            neutroNumberCheck model.nodeForm.falsehood

                        newInDegree =
                            model.nodeForm.inDegree

                        newOutDegree =
                            model.nodeForm.outDegree
                    in
                    { nodeId = newNodeId
                    , label = newNodeLabel
                    , truth = newTruth
                    , indeterminacy = newIndeterminacy
                    , falsehood = newFalsehood
                    , state = "Reg"
                    , linkState = ""
                    , inDegree = newInDegree
                    , outDegree = newOutDegree
                    }

                newForm =
                    defaultNodeForm

                newNeutroModelNodes =
                    let
                        currentNodesNeutroModel =
                            model.neutroModel.modelNodes

                        currentEdgesNeutroModel =
                            model.neutroModel.modelEdges
                    in
                    { modelNodes = currentNodesNeutroModel ++ [ newNode ]
                    , modelEdges = currentEdgesNeutroModel
                    }

                newNodeLabelPair =
                    pair newNodeId model.nodeForm.label
            in
            ( { model
                | nodeForm = newForm
                , nodes = model.nodes ++ [ newNode ]
                , numConcepts = List.length model.nodes + 1
                , cnScore = newCnScore
                , complexityScore = newComplexityScore
                , densityScore = newDensityScore
                , neutroModel = newNeutroModelNodes
                , nodeLabelPairs = model.nodeLabelPairs ++ [ newNodeLabelPair ]
                , disableRunButton = isButtonEnabled
                , disableDeleteButton = isButtonEnabled
                , nodeLabels = model.nodeLabels ++ [ model.nodeForm.label ]
              }
            , Cmd.none
            )

        AddEdge ->
            let
                newEdge =
                    let
                        newEdgeId =
                            List.length model.edges

                        newOriginNode =
                            edgeFromToCheck model.edgeForm.from

                        newDestinyNode =
                            edgeFromToCheck model.edgeForm.to

                        newEdgeTruth =
                            neutroNumberCheck model.edgeForm.truth

                        newEdgeIndeterminacy =
                            neutroNumberCheck model.edgeForm.indeterminacy

                        newEdgeFalsehood =
                            neutroNumberCheck model.edgeForm.falsehood
                    in
                    { edgeId = newEdgeId
                    , from = newOriginNode
                    , to = newDestinyNode
                    , truth = newEdgeTruth
                    , indeterminacy = newEdgeIndeterminacy
                    , falsehood = newEdgeFalsehood
                    }

                newEdgeForm =
                    defaultEdgeForm

                newNeutroModelEdges =
                    { modelNodes = model.neutroModel.modelNodes
                    , modelEdges = model.neutroModel.modelEdges ++ [ newEdge ]
                    }

                newTransmitterNode =
                    edgeFromToCheck model.edgeForm.from

                newReceiverNode =
                    edgeFromToCheck model.edgeForm.to

                currentTransmitterNodeList =
                    model.listTransmitters

                currentReceiverNodeList =
                    model.listReceivers

                currentNodes =
                    model.nodes

                currentEdges =
                    model.edges

                receiverNode =
                    getNode newReceiverNode currentNodes

                transmitterNode =
                    getNode newTransmitterNode currentNodes

                newListTransmitterNodes =
                    let
                        appendedTransmitterList =
                            newTransmitterNode :: currentTransmitterNodeList

                        nodeAlreadyListed =
                            List.member newTransmitterNode currentTransmitterNodeList
                    in
                    if nodeAlreadyListed == True then
                        currentTransmitterNodeList

                    else
                        appendedTransmitterList

                newListReceivers =
                    let
                        appendedReceiverList =
                            newReceiverNode :: currentReceiverNodeList

                        nodeAlreadyListed =
                            List.member newReceiverNode currentReceiverNodeList
                    in
                    if nodeAlreadyListed == True then
                        currentReceiverNodeList

                    else
                        appendedReceiverList

                newNumOrdinary =
                    let
                        currentNumOrdinary =
                            model.numOrdinary
                    in
                    if receiverNode.linkState == "Ord" && transmitterNode.linkState == "Ord" then
                        currentNumOrdinary + 2

                    else if receiverNode.linkState == "Ord" || transmitterNode.linkState == "Ord" then
                        currentNumOrdinary + 1

                    else
                        currentNumOrdinary

                newNumConnections =
                    List.length currentEdges + 1

                newNumTransmitters =
                    List.length currentTransmitterNodeList + 1

                newNumReceivers =
                    List.length currentReceiverNodeList + 1

                updatedNodeDegreeAndLinkState =
                    let
                        transmitterNodeId =
                            edgeFromToCheck model.edgeForm.from

                        receiverNodeId =
                            edgeFromToCheck model.edgeForm.to

                        nodeOutDegreeToUpdate =
                            List.filter (\n -> n.nodeId == transmitterNodeId) currentNodes

                        nodeInDegreeToUpdate =
                            List.filter (\n -> n.nodeId == receiverNodeId) currentNodes

                        allNodesButTransmitter =
                            List.filter (\n -> n.nodeId /= transmitterNodeId) currentNodes

                        outNodes =
                            case List.head nodeOutDegreeToUpdate of
                                Just n ->
                                    { n
                                        | outDegree = n.outDegree + 1
                                        , linkState =
                                            if n.inDegree > 0 then
                                                "Ord"

                                            else
                                                "Tra"
                                    }
                                        :: allNodesButTransmitter

                                Nothing ->
                                    model.nodes

                        allNodesButReceiverWithUpdatedOutNodes =
                            List.filter (\n -> n.nodeId /= receiverNodeId) outNodes

                        updatedNodes =
                            case List.head nodeInDegreeToUpdate of
                                Just n ->
                                    { n
                                        | inDegree = n.inDegree + 1
                                        , linkState =
                                            if n.outDegree > 0 then
                                                "Ord"

                                            else
                                                "Rec"
                                    }
                                        :: allNodesButReceiverWithUpdatedOutNodes

                                Nothing ->
                                    model.nodes
                    in
                    updatedNodes

                newFromNodeId =
                    edgeFromToCheck model.edgeForm.from

                newToNodeId =
                    edgeFromToCheck model.edgeForm.to

                newEdgePair =
                    pair newFromNodeId newToNodeId

                isEdgePairAlreadyListed =
                    List.member newEdgePair model.edgePairs

                newEdgePairList =
                    if isEdgePairAlreadyListed == True then
                        model.edgePairs

                    else
                        model.edgePairs ++ [ newEdgePair ]

                newEdges =
                    if isEdgePairAlreadyListed == True then
                        model.edges

                    else
                        model.edges ++ [ newEdge ]
            in
            ( { model
                | nodes = updatedNodeDegreeAndLinkState
                , edges = newEdges
                , edgeForm = newEdgeForm
                , neutroModel = newNeutroModelEdges
                , edgePairs = newEdgePairList

                -- KPIs
                , listTransmitters = newListTransmitterNodes
                , listReceivers = newListReceivers
                , numConnections = newNumConnections
                , numTransmitters = newNumTransmitters
                , numReceivers = newNumReceivers
                , numOrdinary = newNumOrdinary
                , cnScore = newCnScore
                , complexityScore = newComplexityScore
                , densityScore = newDensityScore

                --
                , disableRunButton = isButtonEnabled
                , disableDeleteButton = isButtonEnabled
              }
            , Cmd.none
            )

        AddSimNode ->
            let
                simulatedNodeId =
                    model.simulationForm.simLabel
                        |> String.toInt
                        |> Maybe.withDefault 0

                nodeToUpdate =
                    List.filter (\n -> n.nodeId == simulatedNodeId) model.nodes

                allNodesButSim =
                    List.filter (\n -> n.nodeId /= simulatedNodeId) model.nodes

                updatedNodeFromSim =
                    case List.head nodeToUpdate of
                        Just n ->
                            { n
                                | state = "Sim"
                                , truth = neutroNumberCheck model.simulationForm.truth
                                , indeterminacy = neutroNumberCheck model.simulationForm.indeterminacy
                                , falsehood = neutroNumberCheck model.simulationForm.falsehood
                            }
                                :: allNodesButSim

                        Nothing ->
                            model.nodes

                newSimulationNode =
                    let
                        newSimNodeId =
                            model.simulationForm.simLabel
                                |> String.toInt
                                |> Maybe.withDefault 0

                        newSimNodeLabel =
                            model.simulationForm.simLabel

                        newSimNodeTruth =
                            neutroNumberCheck model.simulationForm.truth

                        newSimNodeIndeterminacy =
                            neutroNumberCheck model.simulationForm.indeterminacy

                        newSimNodeFalsehood =
                            neutroNumberCheck model.simulationForm.falsehood
                    in
                    { nodeId = newSimNodeId
                    , label = newSimNodeLabel
                    , truth = newSimNodeTruth
                    , indeterminacy = newSimNodeIndeterminacy
                    , falsehood = newSimNodeFalsehood
                    , state = "Sim"
                    , linkState = ""
                    , inDegree = 0
                    , outDegree = 0
                    }

                newSimulationForm =
                    defaultSimulationForm

                newNeutroModelSimNodes =
                    { modelNodes = updatedNodeFromSim
                    , modelEdges = model.neutroModel.modelEdges
                    }
            in
            ( { model
                | simulationForm = newSimulationForm
                , simulatedNodes = model.simulatedNodes ++ [ newSimulationNode ]
                , nodes = updatedNodeFromSim
                , neutroModel = newNeutroModelSimNodes
                , disableRunButton = isButtonEnabled
                , disableDeleteButton = isButtonEnabled
                , simLabels = model.simLabels ++ [ model.simulationForm.simLabel ]
              }
            , Cmd.none
            )

        AddTargetNode ->
            let
                targetedNodeId =
                    model.targetNodeForm.targetNodeLabel
                        |> String.toInt
                        |> Maybe.withDefault 0

                nodeToUpdate =
                    List.filter (\n -> n.nodeId == targetedNodeId) model.nodes

                allNodesButTarget =
                    List.filter (\n -> n.nodeId /= targetedNodeId) model.nodes

                updatedNodeFromTarget =
                    case List.head nodeToUpdate of
                        Just n ->
                            { n | state = "Tar" } :: allNodesButTarget

                        Nothing ->
                            model.nodes

                newTargetNode =
                    let
                        newTargetNodeId =
                            List.length model.targetNodes
                    in
                    { targetNodeId = newTargetNodeId
                    , targetNodeLabel = model.targetNodeForm.targetNodeLabel
                    , state = "Tar"
                    }

                newTargetNodeForm =
                    defaultTargetNodeForm

                newNeutroModelTargetNodes =
                    { modelNodes = updatedNodeFromTarget
                    , modelEdges = model.neutroModel.modelEdges
                    }
            in
            ( { model
                | targetNodeForm = newTargetNodeForm
                , targetNodes = model.targetNodes ++ [ newTargetNode ]
                , nodes = updatedNodeFromTarget
                , neutroModel = newNeutroModelTargetNodes
                , disableRunButton = isButtonEnabled
                , disableDeleteButton = isButtonEnabled
                , targetLabels = model.targetLabels ++ [ model.targetNodeForm.targetNodeLabel ]
              }
            , Cmd.none
            )

        DeleteNode nodeId ->
            let
                newNeutroModelNodesDeleted =
                    { modelNodes = List.filter (\n -> n.nodeId /= nodeId) model.neutroModel.modelNodes
                    , modelEdges = model.neutroModel.modelEdges
                    }

                indexToRemove =
                    nodeId
            in
            ( { model
                | nodes = List.filter (\n -> n.nodeId /= nodeId) model.nodes
                , neutroModel = newNeutroModelNodesDeleted
                , nodeLabelPairs = List.Extra.removeAt indexToRemove model.nodeLabelPairs
                , disableRunButton = isButtonEnabled
                , disableDeleteButton = isButtonEnabled
              }
            , Cmd.none
            )

        DeleteEdge edgeId ->
            let
                newNeutroModelEdgesDeleted =
                    { modelNodes = model.neutroModel.modelNodes
                    , modelEdges = List.filter (\n -> n.edgeId /= edgeId) model.neutroModel.modelEdges
                    }
            in
            ( { model
                | edges = List.filter (\n -> n.edgeId /= edgeId) model.edges
                , neutroModel = newNeutroModelEdgesDeleted
                , disableRunButton = isButtonEnabled
                , disableDeleteButton = isButtonEnabled
              }
            , Cmd.none
            )

        DeleteModel ->
            ( initModel, Cmd.none )

        UpdateNodeLabel newLabel ->
            let
                oldNodeForm =
                    model.nodeForm

                newNodeForm =
                    { oldNodeForm | label = newLabel }

                sameLabelValidation =
                    if List.member newLabel model.nodeLabels == True then
                        True

                    else
                        False
            in
            ( { model
                | nodeForm = newNodeForm
                , disableEdgeForm = isEdgeFormEnabled
                , disableSimForm = isNodeSimFormEnabled
                , disableTargetForm = isTargetFormEnabled
                , disableRunButton = isButtonEnabled
                , disableDeleteButton = isButtonEnabled
                , disableFormBtn = sameLabelValidation
              }
            , Cmd.none
            )

        UpdateNodeTruth newTruth ->
            let
                oldNodeForm =
                    model.nodeForm

                newNodeForm =
                    if String.right 1 newTruth == "." then
                        { oldNodeForm | truth = NeutroField Nothing newTruth }

                    else
                        let
                            maybeTruth =
                                newTruth |> String.toFloat
                        in
                        case maybeTruth of
                            Nothing ->
                                { oldNodeForm | truth = NeutroField Nothing newTruth }

                            Just t ->
                                { oldNodeForm | truth = NeutroField (Just t) newTruth }
            in
            ( { model | nodeForm = newNodeForm }, Cmd.none )

        UpdateNodeIndeterminacy newIndeterminacy ->
            let
                oldNodeForm =
                    model.nodeForm

                newNodeForm =
                    if String.right 1 newIndeterminacy == "." then
                        { oldNodeForm | indeterminacy = NeutroField Nothing newIndeterminacy }

                    else
                        let
                            maybeIndeterminacy =
                                newIndeterminacy |> String.toFloat
                        in
                        case maybeIndeterminacy of
                            Nothing ->
                                { oldNodeForm | indeterminacy = NeutroField Nothing newIndeterminacy }

                            Just p ->
                                { oldNodeForm | indeterminacy = NeutroField (Just p) newIndeterminacy }
            in
            ( { model | nodeForm = newNodeForm }, Cmd.none )

        UpdateNodeFalsehood newFalsehood ->
            let
                oldNodeForm =
                    model.nodeForm

                newNodeForm =
                    if String.right 1 newFalsehood == "." then
                        { oldNodeForm | falsehood = NeutroField Nothing newFalsehood }

                    else
                        let
                            maybeFalsehood =
                                newFalsehood |> String.toFloat
                        in
                        case maybeFalsehood of
                            Nothing ->
                                { oldNodeForm | falsehood = NeutroField Nothing newFalsehood }

                            Just p ->
                                { oldNodeForm | falsehood = NeutroField (Just p) newFalsehood }
            in
            ( { model | nodeForm = newNodeForm }, Cmd.none )

        UpdateEdgeOriginNode newOriginNode ->
            let
                oldEdgeForm =
                    model.edgeForm

                newEdgeForm =
                    let
                        isDigitNodeCheck =
                            String.all Char.isDigit newOriginNode
                    in
                    if isDigitNodeCheck == True then
                        { oldEdgeForm | from = Nid Nothing newOriginNode }

                    else
                        let
                            maybeNewOriginNode =
                                newOriginNode |> String.toInt
                        in
                        case maybeNewOriginNode of
                            Nothing ->
                                { oldEdgeForm | from = Nid Nothing newOriginNode }

                            Just t ->
                                { oldEdgeForm | from = Nid (Just t) newOriginNode }

                disabledEdgeForm =
                    let
                        originNode =
                            nidToString model.edgeForm.from

                        originNodeId =
                            case String.toInt originNode of
                                Just n ->
                                    n

                                Nothing ->
                                    -1

                        destinyNode =
                            nidToString model.edgeForm.to

                        destinyNodeId =
                            case String.toInt destinyNode of
                                Just n ->
                                    n

                                Nothing ->
                                    -1

                        edgeNodeIdPair =
                            pair originNodeId destinyNodeId

                        isEdgeAlreadyListed =
                            List.member edgeNodeIdPair model.edgePairs
                    in
                    if newOriginNode == originNode || isEdgeAlreadyListed == True then
                        True

                    else
                        False
            in
            ( { model
                | edgeForm = newEdgeForm
                , disableRunButton = isButtonEnabled
                , disableDeleteButton = isButtonEnabled
                , disableFormBtn = disabledEdgeForm
              }
            , Cmd.none
            )

        UpdateEdgeDestinyNode newDestinyNode ->
            let
                oldEdgeForm =
                    model.edgeForm

                newEdgeForm =
                    let
                        isDigitNodeCheck =
                            String.all Char.isDigit newDestinyNode
                    in
                    if isDigitNodeCheck == True then
                        { oldEdgeForm | to = Nid Nothing newDestinyNode }

                    else
                        let
                            maybeNewDestinyNode =
                                newDestinyNode |> String.toInt
                        in
                        case maybeNewDestinyNode of
                            Nothing ->
                                { oldEdgeForm | to = Nid Nothing newDestinyNode }

                            Just t ->
                                { oldEdgeForm | to = Nid (Just t) newDestinyNode }

                disabledEdgeForm =
                    let
                        originNode =
                            nidToString model.edgeForm.from

                        originNodeId =
                            case String.toInt originNode of
                                Just n ->
                                    n

                                Nothing ->
                                    -1

                        destinyNode =
                            nidToString model.edgeForm.to

                        destinyNodeId =
                            case String.toInt destinyNode of
                                Just n ->
                                    n

                                Nothing ->
                                    -1

                        edgeNodeIdPair =
                            pair originNodeId destinyNodeId

                        isEdgeAlreadyListed =
                            List.member edgeNodeIdPair model.edgePairs
                    in
                    if newDestinyNode == originNode || isEdgeAlreadyListed == True then
                        True

                    else
                        False
            in
            ( { model
                | edgeForm = newEdgeForm
                , disableFormBtn = disabledEdgeForm
              }
            , Cmd.none
            )

        UpdateEdgeTruth newTruth ->
            let
                oldEdgeForm =
                    model.edgeForm

                newEdgeForm =
                    if String.right 1 newTruth == "." then
                        { oldEdgeForm | truth = NeutroField Nothing newTruth }

                    else
                        let
                            maybeTruth =
                                newTruth |> String.toFloat
                        in
                        case maybeTruth of
                            Nothing ->
                                { oldEdgeForm | truth = NeutroField Nothing newTruth }

                            Just t ->
                                { oldEdgeForm | truth = NeutroField (Just t) newTruth }

                disabledEdgeForm =
                    let
                        originNode =
                            nidToString model.edgeForm.from

                        originNodeId =
                            case String.toInt originNode of
                                Just n ->
                                    n

                                Nothing ->
                                    -1

                        destinyNode =
                            nidToString model.edgeForm.to

                        destinyNodeId =
                            case String.toInt destinyNode of
                                Just n ->
                                    n

                                Nothing ->
                                    -1

                        edgeNodeIdPair =
                            pair originNodeId destinyNodeId

                        isEdgeAlreadyListed =
                            List.member edgeNodeIdPair model.edgePairs
                    in
                    if destinyNode == originNode || isEdgeAlreadyListed == True then
                        True

                    else
                        False
            in
            ( { model
                | edgeForm = newEdgeForm
                , disableFormBtn = disabledEdgeForm
              }
            , Cmd.none
            )

        UpdateEdgeIndeterminacy newIndeterminacy ->
            let
                oldEdgeForm =
                    model.edgeForm

                newEdgeForm =
                    if String.right 1 newIndeterminacy == "." then
                        { oldEdgeForm | indeterminacy = NeutroField Nothing newIndeterminacy }

                    else
                        let
                            maybeIndeterminacy =
                                newIndeterminacy |> String.toFloat
                        in
                        case maybeIndeterminacy of
                            Nothing ->
                                { oldEdgeForm | indeterminacy = NeutroField Nothing newIndeterminacy }

                            Just p ->
                                { oldEdgeForm | indeterminacy = NeutroField (Just p) newIndeterminacy }
            in
            ( { model | edgeForm = newEdgeForm }, Cmd.none )

        UpdateEdgeFalsehood newFalsehood ->
            let
                oldEdgeForm =
                    model.edgeForm

                newEdgeForm =
                    if String.right 1 newFalsehood == "." then
                        { oldEdgeForm | falsehood = NeutroField Nothing newFalsehood }

                    else
                        let
                            maybeFalsehood =
                                newFalsehood |> String.toFloat
                        in
                        case maybeFalsehood of
                            Nothing ->
                                { oldEdgeForm | falsehood = NeutroField Nothing newFalsehood }

                            Just p ->
                                { oldEdgeForm | falsehood = NeutroField (Just p) newFalsehood }
            in
            ( { model | edgeForm = newEdgeForm }, Cmd.none )

        UpdateSimNodeLabel newSimulatedNodeLabel ->
            let
                oldSimulationForm =
                    model.simulationForm

                newSimulationForm =
                    { oldSimulationForm | simLabel = newSimulatedNodeLabel }

                alreadySim =
                    let
                        simNodeId =
                            case String.toInt newSimulatedNodeLabel of
                                Just n ->
                                    n

                                Nothing ->
                                    -1

                        maybeCurrSimNode =
                            List.filter (\n -> simNodeId == n.nodeId) model.nodes
                                |> List.head

                        currentSimNode =
                            case maybeCurrSimNode of
                                Just n ->
                                    n

                                Nothing ->
                                    errorNode

                        isAlreadySimulated =
                            List.member newSimulatedNodeLabel model.simLabels
                    in
                    if
                        isAlreadySimulated
                            == True
                            || model.edges
                            == []
                            || currentSimNode.linkState
                            == "Rec"
                            || currentSimNode.linkState
                            == "Ord"
                    then
                        True

                    else
                        False
            in
            ( { model
                | simulationForm = newSimulationForm
                , disableRunButton = isButtonEnabled
                , disableDeleteButton = isButtonEnabled
                , disableFormBtn = alreadySim
              }
            , Cmd.none
            )

        UpdateSimNodeTruth newSimNodeTruth ->
            let
                oldSimNodeForm =
                    model.simulationForm

                newSimNodeForm =
                    if String.right 1 newSimNodeTruth == "." then
                        { oldSimNodeForm | truth = NeutroField Nothing newSimNodeTruth }

                    else
                        let
                            maybeTruth =
                                newSimNodeTruth |> String.toFloat
                        in
                        case maybeTruth of
                            Nothing ->
                                { oldSimNodeForm | truth = NeutroField Nothing newSimNodeTruth }

                            Just t ->
                                { oldSimNodeForm | truth = NeutroField (Just t) newSimNodeTruth }
            in
            ( { model | simulationForm = newSimNodeForm }, Cmd.none )

        UpdateSimNodeIndeterminacy newSimNodeIndeterminacy ->
            let
                oldSimNodeForm =
                    model.simulationForm

                newSimNodeForm =
                    if String.right 1 newSimNodeIndeterminacy == "." then
                        { oldSimNodeForm | indeterminacy = NeutroField Nothing newSimNodeIndeterminacy }

                    else
                        let
                            maybeIndeterminacy =
                                newSimNodeIndeterminacy |> String.toFloat
                        in
                        case maybeIndeterminacy of
                            Nothing ->
                                { oldSimNodeForm | indeterminacy = NeutroField Nothing newSimNodeIndeterminacy }

                            Just t ->
                                { oldSimNodeForm | indeterminacy = NeutroField (Just t) newSimNodeIndeterminacy }
            in
            ( { model | simulationForm = newSimNodeForm }, Cmd.none )

        UpdateSimNodeFalsehood newSimNodeFalsehood ->
            let
                oldSimNodeForm =
                    model.simulationForm

                newSimNodeForm =
                    if String.right 1 newSimNodeFalsehood == "." then
                        { oldSimNodeForm | falsehood = NeutroField Nothing newSimNodeFalsehood }

                    else
                        let
                            maybeFalsehood =
                                newSimNodeFalsehood |> String.toFloat
                        in
                        case maybeFalsehood of
                            Nothing ->
                                { oldSimNodeForm | falsehood = NeutroField Nothing newSimNodeFalsehood }

                            Just t ->
                                { oldSimNodeForm | falsehood = NeutroField (Just t) newSimNodeFalsehood }
            in
            ( { model | simulationForm = newSimNodeForm }, Cmd.none )

        UpdateTargetNodeLabel newTargetNodeLabel ->
            let
                oldTargetNodeForm =
                    model.targetNodeForm

                newTargetNodeForm =
                    { oldTargetNodeForm | targetNodeLabel = newTargetNodeLabel }

                sameLabelValidation =
                    if List.member newTargetNodeLabel model.targetLabels == True then
                        True

                    else
                        False
            in
            ( { model
                | targetNodeForm = newTargetNodeForm
                , disableRunButton = isButtonEnabled
                , disableDeleteButton = isButtonEnabled
                , disableFormBtn = sameLabelValidation
              }
            , Cmd.none
            )

        DisplayCurState ->
            ( { model
                | currentStateTabDisplay = False
                , simulatedStateTabDisplay = True
              }
            , Cmd.none
            )

        DisplaySimState ->
            ( { model
                | simulatedStateTabDisplay = False
                , currentStateTabDisplay = True
              }
            , Cmd.none
            )

        DisplayNodeForm ->
            ( { model
                | nodeFormDisplay = isToggled model.nodeFormDisplay
                , edgeFormDisplay = True
                , simFormDisplay = True
                , targetFormDisplay = True
              }
            , Cmd.none
            )

        DisplayEdgeForm ->
            ( { model
                | edgeFormDisplay =
                    if List.length model.nodes < 2 then
                        True

                    else
                        isToggled model.edgeFormDisplay
                , nodeFormDisplay = True
                , simFormDisplay = True
                , targetFormDisplay = True
                , disableEdgeForm = isEdgeFormEnabled
              }
            , Cmd.none
            )

        DisplaySimForm ->
            ( { model
                | simFormDisplay =
                    if List.length model.nodes == 0 then
                        True

                    else
                        isToggled model.simFormDisplay
                , simulationForm = model.simulationForm
                , nodeFormDisplay = True
                , edgeFormDisplay = True
                , targetFormDisplay = True
                , disableSimForm = isNodeSimFormEnabled
              }
            , Cmd.none
            )

        DisplayTargetForm ->
            ( { model
                | targetFormDisplay =
                    if model.nodes == [] then
                        True

                    else
                        isToggled model.targetFormDisplay
                , targetNodeForm = model.targetNodeForm
                , nodeFormDisplay = True
                , edgeFormDisplay = True
                , simFormDisplay = True
                , disableTargetForm = isTargetFormEnabled
              }
            , Cmd.none
            )

        DisplayNodeTable ->
            ( { model
                | nodeTableDisplay = isToggled model.nodeTableDisplay
              }
            , Cmd.none
            )

        DisplayEdgeTable ->
            ( { model
                | edgeTableDisplay = isToggled model.edgeTableDisplay
              }
            , Cmd.none
            )

        Recv val ->
            let
                decodeResultNode : Decoder ResultNode
                decodeResultNode =
                    Decode.succeed ResultNode
                        |> Decode.required "nodeId" int
                        |> Decode.required "label" string
                        |> Decode.required "state" string
                        |> Decode.required "truth" float
                        |> Decode.required "indeterminacy" float
                        |> Decode.required "falsehood" float

                result =
                    case Decode.decodeValue (list decodeResultNode) val of
                        Ok v ->
                            v

                        Err _ ->
                            model.simulationResult
            in
            ( { model | simulationResult = Debug.log "Log From Elm: Result from JS-WASM" result }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "container-fluid app bg-dark m-0 p-0" ]
        [ viewLeftMenuBar model
        , viewGraphCanvas model
        , viewRightMenuBar model
        ]


viewLeftMenuBar : Model -> Html Msg
viewLeftMenuBar model =
    div
        [ class "col-3 text-center m-0 p-0 d-flex"
        , style "height" "100vh"
        ]
        [ viewVerticalMenu
        , viewInputFormsSection model
        ]


viewVerticalMenu : Html Msg
viewVerticalMenu =
    div
        [ class "shadow d-inline-block w-full m-0 p-auto"
        , style "max-width" "80px"
        , style "height" "100vh"
        ]
        [ viewMenuButton "Save"
        , viewMenuButton "Open"
        , viewMenuButton "Import"
        , viewMenuButton "Export"
        , viewMenuButton "Logout"
        ]


viewInputFormsSection : Model -> Html Msg
viewInputFormsSection model =
    div
        [ class "container-fluid m-0 p-0 d-inline-block"
        , style "height" "100vh"
        ]
        [ viewNodeInputForm model model.nodeForm
        , viewEdgeInputForm model model.edgeForm
        , viewSimulationInputForm model model.simulationForm
        , viewTargetNodeForm model model.targetNodeForm
        , viewModelControl model
        ]


viewGraphCanvas : Model -> Html Msg
viewGraphCanvas model =
    div
        [ class "col-6 bg-dark text-white" ]
        [ svg
            [ viewBox 0 0 w h ]
            [ g
                [ TypedSvg.Attributes.class [ "links" ] ]
              <|
                List.map (linkElement (initGraph model)) <|
                    Graph.edges (initGraph model)
            , g
                [ TypedSvg.Attributes.class [ "nodes" ] ]
              <|
                List.map nodeElement <|
                    Graph.nodes (initGraph model)
            ]
        ]


viewRightMenuBar : Model -> Html Msg
viewRightMenuBar model =
    div
        [ class "shadow bar-scroll col-3" ]
        [ div [ class "m-0 w-100" ]
            [ viewTabMenu model
            , div
                [ hidden model.currentStateTabDisplay ]
                [ viewCurrentState model ]
            , div
                [ hidden model.simulatedStateTabDisplay ]
                [ viewResultNodesState model.simulationResult ]
            ]
        ]



-- FORMS


viewNodeInputForm : Model -> NodeForm -> Html Msg
viewNodeInputForm model node =
    div
        [ class "accordion mt-5"
        ]
        [ div
            [ class "shadow card border-0 m-auto w-50"
            , style "border-radius" "2rem"
            , style "min-width" "180px"
            ]
            [ viewFormHeader "Node" "card-header m-0 p-1 bg-primary text-center" DisplayNodeForm
            , form
                [ class "collapse show"
                , style "display" "inline-block"
                , hidden model.nodeFormDisplay
                , onSubmit AddNode
                ]
                [ div
                    [ class "card-body p-3"
                    , style "align-items" "center"
                    ]
                    [ viewInputNodeLabel "140px" "mb-3" "Label" node.label UpdateNodeLabel
                    , viewInputNumber "Tru" node.truth UpdateNodeTruth
                    , viewInputNumber "Ind" node.indeterminacy UpdateNodeIndeterminacy
                    , viewInputNumber "Fal" node.falsehood UpdateNodeFalsehood
                    , viewFormButton "Add Node" model
                    ]
                ]
            ]
        ]


viewEdgeInputForm : Model -> EdgeForm -> Html Msg
viewEdgeInputForm model edge =
    div
        [ class "accordion mt-3" ]
        [ div
            [ class "shadow card border-0 m-auto w-50"
            , style "border-radius" "2rem"
            , style "min-width" "180px"
            ]
            [ viewFormHeader "Edge" model.disableEdgeForm DisplayEdgeForm
            , form
                [ class "collapse show"
                , style "display" "inline-block"
                , hidden model.edgeFormDisplay
                , onSubmit AddEdge
                ]
                [ div
                    [ class "card-body p-3"
                    , style "align-items" "center"
                    ]
                    [ viewNodesList "Label" "mb-3 mx-1" "60px" UpdateEdgeOriginNode (nidToString edge.from) model
                    , viewNodesList "Label" "mb-3 mx-1" "60px" UpdateEdgeDestinyNode (nidToString edge.to) model
                    , viewInputNumber "Tru" edge.truth UpdateEdgeTruth
                    , viewInputNumber "Ind" edge.indeterminacy UpdateEdgeIndeterminacy
                    , viewInputNumber "Fal" edge.falsehood UpdateEdgeFalsehood
                    , viewFormButton "Add Edge" model
                    ]
                ]
            ]
        ]


viewSimulationInputForm : Model -> SimulationForm -> Html Msg
viewSimulationInputForm model simNode =
    div
        [ class "accordion mt-3"
        , id "accordionExample"
        ]
        [ div
            [ class "shadow card border-0 m-auto w-50"
            , style "border-radius" "2rem"
            , style "min-width" "180px"
            ]
            [ viewFormHeader "Simulate" model.disableSimForm DisplaySimForm
            , form
                [ class "collapse show"
                , style "display" "inline-block"
                , hidden model.simFormDisplay
                , onSubmit AddSimNode
                ]
                [ div [ class "card-body p-3" ]
                    [ viewNodesList "Label" "mb-3" "140px" UpdateSimNodeLabel simNode.simLabel model
                    , viewInputNumber "Tru" simNode.truth UpdateSimNodeTruth
                    , viewInputNumber "Ind" simNode.indeterminacy UpdateSimNodeIndeterminacy
                    , viewInputNumber "Fal" simNode.falsehood UpdateSimNodeFalsehood
                    , viewFormButton "Simulate Node" model
                    ]
                ]
            ]
        ]


viewTargetNodeForm : Model -> TargetNodeForm -> Html Msg
viewTargetNodeForm model targetNode =
    div
        [ class "accordion mt-3" ]
        [ div
            [ class "shadow card border-0 m-auto w-50"
            , style "border-radius" "2rem"
            , style "min-width" "180px"
            ]
            [ viewFormHeader "Target" model.disableTargetForm DisplayTargetForm
            , form
                [ class "collapse show"
                , style "display" "inline-block"
                , hidden model.targetFormDisplay
                , onSubmit AddTargetNode
                ]
                [ div [ class "card-body p-3" ]
                    [ viewNodesList "Label" "mb-3" "140px" UpdateTargetNodeLabel targetNode.targetNodeLabel model
                    , viewFormButton "Add Target" model
                    ]
                ]
            ]
        ]


viewModelControl : Model -> Html Msg
viewModelControl model =
    div
        [ class "accordion mt-3 m-auto w-100"
        , style "max-width" "180px"
        ]
        [ div
            [ class "d-inline-block mx-auto my-5" ]
            [ viewRunButton "Run Model" "btn-outline-success" model RunSimulation
            , viewDeleteButton "Delete Model" "btn-outline-danger" model DeleteModel
            ]
        ]



-- TABLES


viewCurrentState : Model -> Html Msg
viewCurrentState model =
    div
        [ class "bg-dark w-100 mt-4" ]
        [ viewKpiTable "KPIs" model
        , viewNodeTable "Nodes" viewNodes model.nodes model
        , viewEdgeTable "Edges" (viewEdges model) model
        ]


viewResultNodesState : NeutroResult -> Html Msg
viewResultNodesState model =
    table
        [ class "table" ]
        (tr
            [ class "border-bottom border-secondary" ]
            [ td [ class "tb-header-label text-white text-left" ] [ text "Label" ]
            , td [ class "tb-header-label text-white text-center" ] [ text "State" ]
            , td [ class "tb-header-label text-white text-right" ] [ text "Tru" ]
            , td [ class "tb-header-label text-white text-right" ] [ text "Ind" ]
            , td [ class "tb-header-label text-white text-right" ] [ text "Fal" ]
            ]
            :: List.map viewResultNode model
        )


viewResultNode : ResultNode -> Html Msg
viewResultNode node =
    let
        truth =
            Round.round 2 node.truth

        indeterminacy =
            Round.round 2 node.indeterminacy

        falsehood =
            Round.round 2 node.falsehood
    in
    tr []
        [ td [ class "tb-header-label align-center text-white align-middle text-left border-0" ]
            [ text node.label ]
        , td [ class "tb-header-label text-white text-center border-0" ]
            [ text node.state ]
        , td [ class "tb-header-label align-center text-white align-middle text-right border-0" ]
            [ text truth ]
        , td [ class "tb-header-label align-center text-white align-middle text-right border-0" ]
            [ text indeterminacy ]
        , td [ class "tb-header-label align-center text-white align-middle text-right border-0" ]
            [ text falsehood ]
        ]


viewNodes : List NeutroNode -> Html Msg
viewNodes nodes =
    table
        [ class "table" ]
        (tr
            [ class "border-bottom border-secondary" ]
            [ td [ class "tb-header-label text-white text-left" ] [ text "Label" ]
            , td [ class "tb-header-label text-white text-center" ] [ text "State" ]
            , td [ class "tb-header-label text-white text-right" ] [ text "Tru" ]
            , td [ class "tb-header-label text-white text-right" ] [ text "Ind" ]
            , td [ class "tb-header-label text-white text-right" ] [ text "Fal" ]
            , td [ class "tb-header-label text-white text-right" ] [ text "" ]
            ]
            :: List.map viewNode nodes
        )


viewNode : NeutroNode -> Html Msg
viewNode node =
    tr []
        [ td [ class "tb-header-label align-center text-white align-middle text-left border-0" ] [ text node.label ]
        , td [ class "tb-header-label text-white text-center border-0" ] [ text node.state ]
        , td [ class "tb-header-label align-center text-white align-middle text-right border-0" ] [ text (String.fromFloat node.truth) ]
        , td [ class "tb-header-label align-center text-white align-middle text-right border-0" ] [ text (String.fromFloat node.indeterminacy) ]
        , td [ class "tb-header-label align-center text-white align-middle text-right border-0" ] [ text (String.fromFloat node.falsehood) ]
        , td [ class "tb-header-label align-center text-white align-middle text-right border-0" ]
            [ a
                [ class "tb-header-label text-danger font-weight-bold"
                , type_ "button"
                , onClick (DeleteNode node.nodeId)
                ]
                [ text "X" ]
            ]
        ]


viewEdges : Model -> Html Msg
viewEdges model =
    table
        [ class "table" ]
        (tr
            [ class "border-bottom border-secondary" ]
            [ td [ class "tb-header-label text-white text-left" ] [ text "From" ]
            , td [ class "tb-header-label text-white text-center" ] [ text "To" ]
            , td [ class "tb-header-label text-white text-right" ] [ text "Tru" ]
            , td [ class "tb-header-label text-white text-right" ] [ text "Ind" ]
            , td [ class "tb-header-label text-white text-right" ] [ text "Fal" ]
            , td [ class "tb-header-label text-white text-right" ] [ text "" ]
            ]
            :: List.map (viewEdge model.nodes) model.edges
        )


viewEdge : List NeutroNode -> NeutroEdge -> Html Msg
viewEdge nodes edge =
    let
        fromNode =
            List.filter (\n -> edge.from == n.nodeId) nodes
                |> List.head

        fromNodeLabel =
            case fromNode of
                Just n ->
                    n.label

                Nothing ->
                    String.fromInt edge.from

        toNode =
            List.filter (\n -> edge.to == n.nodeId) nodes
                |> List.head

        toNodeLabel =
            case toNode of
                Just n ->
                    n.label

                Nothing ->
                    String.fromInt edge.to
    in
    tr []
        [ td [ class "tb-header-label text-white align-middle text-left border-0" ] [ text fromNodeLabel ]
        , td [ class "tb-header-label text-white align-middle text-center border-0" ] [ text toNodeLabel ]
        , td [ class "tb-header-label text-white align-middle text-right border-0" ] [ text (String.fromFloat edge.truth) ]
        , td [ class "tb-header-label text-white align-middle text-right border-0" ] [ text (String.fromFloat edge.indeterminacy) ]
        , td [ class "tb-header-label text-white align-middle text-right border-0" ] [ text (String.fromFloat edge.falsehood) ]
        , td [ class "tb-header-label text-white align-middle text-right border-0" ]
            [ a
                [ class "tb-header-label text-danger font-weight-bold"
                , type_ "button"
                , onClick (DeleteEdge edge.edgeId)
                ]
                [ text "X" ]
            ]
        ]



-- SHARED COMPONENTS


viewInputNumber : String -> NeutroField -> (String -> msg) -> Html msg
viewInputNumber p val msg =
    div []
        [ input
            [ type_ "range"
            , class "mx-1"
            , style "width" "100px"
            , style "float" "left"
            , placeholder p
            , Html.Attributes.min "0.0"
            , Html.Attributes.max "1.0"
            , step "0.01"
            , required True
            , value (neutroFieldToString val)
            , onInput msg
            ]
            []
        , text (neutroFieldToString val)
        ]


viewInputNodeLabel : String -> String -> String -> String -> (String -> msg) -> Html msg
viewInputNodeLabel size c p val msg =
    input
        [ type_ "text"
        , class c
        , style "width" size
        , placeholder p
        , required True
        , value val
        , onInput msg
        ]
        []


viewNodesList : String -> String -> String -> (String -> Msg) -> String -> Model -> Html Msg
viewNodesList p c size msg index model =
    let
        nodes =
            model.nodeLabelPairs
    in
    select
        [ onInput msg
        , class c
        , style "width" size
        , placeholder p
        , required True
        , value index
        ]
        (option
            [ class "tb-header-label text-left" ]
            []
            :: List.map viewNodeOpt nodes
        )


viewNodeOpt : ( Int, String ) -> Html Msg
viewNodeOpt node =
    let
        index =
            first node

        label =
            second node
    in
    option
        [ class "tb-header-label"
        , value (String.fromInt index)
        ]
        [ text label ]


viewFormButton : String -> Model -> Html Msg
viewFormButton p model =
    button
        [ class "btn btn-sm btn-outline-primary w-100 mt-3"
        , type_ "submit"
        , style "border-radius" "2rem"
        , disabled model.disableFormBtn
        ]
        [ text p ]


viewMenuButton : String -> Html msg
viewMenuButton p =
    button
        [ class "btn btn-sm btn-outline-secondary btn-circle my-3"
        , type_ "submit"
        , style "border-radius" "2rem"

        --, disabled True
        ]
        [ text p ]


viewRunButton : String -> String -> Model -> Msg -> Html Msg
viewRunButton p c model msg =
    button
        [ class "shadow btn btn-sm w-50 mx-auto mb-2 px-1"
        , class c
        , type_ "submit"
        , style "border-radius" "2rem"
        , style "min-width" "180px"
        , onClick msg
        , disabled model.disableRunButton
        ]
        [ text p ]


viewDeleteButton : String -> String -> Model -> Msg -> Html Msg
viewDeleteButton p c model msg =
    button
        [ class "shadow btn btn-sm w-50 mx-auto mb-2 px-1"
        , class c
        , type_ "submit"
        , style "border-radius" "2rem"
        , style "min-width" "180px"
        , onClick msg
        , disabled model.disableDeleteButton
        ]
        [ text p ]


viewFormHeader : String -> String -> Msg -> Html Msg
viewFormHeader title cls msg =
    div
        [ class cls
        , style "cursor" "pointer"
        , onClick msg
        ]
        [ button
            [ class "btn mt-1 p-0"
            , type_ "button"
            ]
            [ h6
                [ class "text-white"
                ]
                [ text title ]
            ]
        ]


viewTabMenu : Model -> Html Msg
viewTabMenu model =
    div
        [ class "tab d-flex bg-dark w-100 mt-2" ]
        [ button
            [ class "w-100 text-white float-left p-1"
            , style "cursor" "pointer"
            , onClick DisplayCurState
            ]
            [ text "Current State" ]
        , button
            [ class "w-100 text-white p-1 bg-danger"
            , style "cursor" "pointer"
            , onClick DisplaySimState
            , hidden model.simulatedStateResultDisplay
            ]
            [ text "Simulation" ]
        ]


viewNodeTable : String -> (c -> Html Msg) -> c -> { a | nodeTableDisplay : Bool } -> Html Msg
viewNodeTable title func nodes model =
    div [ class "m-0 w-100" ]
        [ div
            [ class "d-flex bg-dark w-100 m-0 border-bottom" ]
            [ p
                [ class "p-1 m-0 text-primary" ]
                [ text title ]
            , div
                [ class "w-100 text-right text-primary pr-1"
                , style "cursor" "pointer"
                , onClick DisplayNodeTable
                ]
                [ text "▼" ]
            ]
        , div
            [ hidden model.nodeTableDisplay ]
            [ func nodes ]
        ]


viewEdgeTable : String -> Html Msg -> { a | edgeTableDisplay : Bool } -> Html Msg
viewEdgeTable title edges model =
    div [ class "m-0 w-100" ]
        [ div
            [ class "d-flex bg-dark w-100 m-0 border-bottom" ]
            [ p
                [ class "p-1 m-0 text-primary" ]
                [ text title ]
            , div
                [ class "w-100 text-right text-primary pr-1"
                , style "cursor" "pointer"
                , onClick DisplayEdgeTable
                ]
                [ text "▼" ]
            ]
        , div
            [ hidden model.edgeTableDisplay ]
            [ edges ]
        ]


viewKpiTable title model =
    div [ class "m-auto w-100" ]
        [ div
            [ class "d-flex bg-dark w-100 m-0 border-bottom" ]
            [ p
                [ class "p-1 m-0 text-primary" ]
                [ text title ]
            ]
        , div
            [ class "bg-dark w-100" ]
            [ table
                [ class "my-2" ]
                [ viewRow "# Nodes:" model.numConcepts
                , viewRow "# Edges:" model.numConnections
                , viewRow "# Transmitters:" model.numTransmitters
                , viewRow "# Receivers:" model.numReceivers
                , viewRow "# Ordinary:" model.numOrdinary
                , viewRowFloat "C/N Score:" model.cnScore
                , viewRowFloat "Complexity" model.complexityScore
                , viewRowFloat "Density" model.densityScore
                ]
            ]
        ]


viewRow : String -> Int -> Html msg
viewRow p kpi =
    tr
        []
        [ td [ class "tb-header-label text-white align-middle text-left" ] [ text p ]
        , td [ class "tb-header-label text-white align-middle text-right pl-3" ] [ text (String.fromInt kpi) ]
        ]


viewRowFloat : String -> Float -> Html msg
viewRowFloat p kpi =
    tr
        []
        [ td [ class "tb-header-label text-white align-middle text-left" ] [ text p ]
        , td
            [ class "tb-header-label text-white align-middle text-right pl-3"
            , step "0.01"
            ]
            [ text (String.fromFloat kpi) ]
        ]



-- HELPERS


edgeFromToCheck model =
    model
        |> nidToString
        |> String.toInt
        |> Maybe.withDefault 0


neutroNumberCheck model =
    model
        |> neutroFieldToString
        |> String.toFloat
        |> Maybe.withDefault 0.0


neutroFieldToString : NeutroField -> String
neutroFieldToString neutroField =
    case neutroField of
        NeutroField Nothing neutro ->
            neutro

        NeutroField (Just _) neutro ->
            neutro


nidToString : Nid -> String
nidToString nid =
    case nid of
        Nid Nothing n ->
            n

        Nid (Just _) n ->
            n


isToggled : Bool -> Bool
isToggled formToggle =
    if formToggle == True then
        False

    else
        True


enabledFormView : List NeutroNode -> String
enabledFormView nodes =
    if nodes == [] || List.length nodes < 2 then
        "m-0 p-1 bg-dark btn btn-outline-secondary text-center"

    else
        "card-header m-0 p-1 bg-primary text-center"


enabledButton : List NeutroNode -> List NeutroEdge -> Int -> Int -> Bool
enabledButton nodes edges numSimNodes numTargetNodes =
    if nodes == [] && edges == [] && numSimNodes < 1 && numTargetNodes < 1 then
        True

    else
        False


isNaNChecked : Float -> Float
isNaNChecked kpiResult =
    if isNaN kpiResult == True then
        0.0

    else
        kpiResult


isOrdinaryNodeValidated : NeutroNode -> List Int -> Bool
isOrdinaryNodeValidated node nodeIdList =
    let
        outDegree =
            node.outDegree

        inDegree =
            node.inDegree

        nodeId =
            node.nodeId

        isNodeAlreadyMember =
            List.member nodeId nodeIdList

        isDegreeOrdinary =
            if outDegree > 0 && inDegree > 0 then
                True

            else
                False
    in
    if isNodeAlreadyMember == False && isDegreeOrdinary == True then
        True

    else
        False


getNode : Int -> List NeutroNode -> NeutroNode
getNode nodeId nodes =
    let
        filteredNode =
            List.filter (\n -> nodeId == n.nodeId) nodes
    in
    case List.head filteredNode of
        Just n ->
            n

        Nothing ->
            errorNode


cnScore : Float -> Float -> Float
cnScore numEdges numNodes =
    (numEdges / numNodes)
        |> Round.round 2
        |> String.toFloat
        |> Maybe.withDefault 0.0


complexityScore : Float -> Float -> Float
complexityScore numTransmitterNodes numReceiverNodes =
    (numTransmitterNodes / numReceiverNodes)
        |> Round.round 2
        |> String.toFloat
        |> Maybe.withDefault 0.0


densityScore : Float -> Float -> Float
densityScore numEdges numNodes =
    (numEdges / numNodes * (numNodes - 1))
        |> Round.round 2
        |> String.toFloat
        |> Maybe.withDefault 0.0
