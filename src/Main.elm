port module Main exposing (..)

{-
      TODOs
   Primary
             - Implement Ordinary Kpi function
             - Implement Run functionality -- Only after backend implementation
             - Validate the following rules
                - From /= To to AddEdge
                - Existing edges
          Secondary
             - Configure Labels to show the neutro number
             - Implement line with arrows
             - Implement Force Directed Graph interactive graph drag and drop
             - Tooltip - tips and explanations modals
             - Icons for the buttons
             - Adding DropDown menu for Edges
             - Change the color of the node in graph and table to ID simNode and targetNode

            Set storage port example

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

-}

import Browser
import Browser.Dom as Dom
import Browser.Events
import Color exposing (Color)
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html exposing (..)
import Html.Attributes exposing (class, disabled, hidden, id, placeholder, required, src, step, style, type_, value)
import Html.Events exposing (..)
import Html.Events.Extra.Mouse as Mouse
import IntDict
import Json.Decode as Decode
import List exposing (range)
import Material.Icons.Round as Icon exposing (add, center_focus_weak, cloud_download, cloud_upload, delete_outline, folder, login, save, share)
import Scale exposing (SequentialScale)
import Scale.Color
import Task
import Time
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


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver Recv



-- MODEL


type alias Model =
    -- Elements
    { nodes : List NeutroNode
    , edges : List NeutroEdge
    , simulatedNodes : List SimulatedNode
    , targetNodes : List TargetNode

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

    -- Tables
    , nodeTableDisplay : Bool
    , edgeTableDisplay : Bool
    , simTableDisplay : Bool
    , targetTableDisplay : Bool
    , currentStateTabDisplay : Bool
    , simStateTabDisplay : Bool

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
    , draft : String
    , messages : List String
    }



-- TYPES


type alias NeutroModel =
    { nodes : List NeutroNode
    , edges : List NeutroEdge
    , simNodes : List SimulatedNode
    , targetNodes : List TargetNode
    }



--nodeState =
--    [ "Reg" -- Regular Node
--    , "Sim" -- Simulation Node
--    , "Tar" -- Target Node
--    ]


type alias NeutroNode =
    { nodeId : Int
    , label : String
    , truth : Float
    , indeterminacy : Float
    , falsehood : Float
    , nodeState : String
    }


type alias NeutroEdge =
    { edgeId : Int
    , from : Int
    , to : Int
    , truth : Float
    , indeterminacy : Float
    , falsehood : Float
    }


type alias SimulatedNode =
    { simNodeId : NodeId
    , simNodeLabel : String
    , simNodeTruth : Float
    , simNodeIndeterminacy : Float
    , simNodeFalsehood : Float
    , nodeState : String
    }


type alias TargetNode =
    { targetNodeId : NodeId
    , targetNodeLabel : String
    , nodeState : String
    }



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
    , nodeState : String
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
    { simNodeId : Int
    , simNodeLabel : String
    , simNodeTruth : NeutroField
    , simNodeIndeterminacy : NeutroField
    , simNodeFalsehood : NeutroField
    , hideForm : Bool
    , nodeState : String
    }


type alias TargetNodeForm =
    { targetNodeId : Int
    , targetNodeLabel : String
    , hideForm : Bool
    , nodeState : String
    }



-- GRAPH TYPES


type alias Entity =
    Force.Entity NodeId { value : CustomNode }


type alias CustomNode =
    { rank : Int, name : String }



-- MODEL DEFAULTS


defaultNodeForm : NodeForm
defaultNodeForm =
    { nodeId = 0
    , label = ""
    , truth = NeutroField Nothing ""
    , indeterminacy = NeutroField Nothing ""
    , falsehood = NeutroField Nothing ""
    , hideForm = True
    , hideTable = True
    , nodeState = "Regular"
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
    { simNodeId = 0
    , simNodeLabel = ""
    , simNodeTruth = NeutroField Nothing ""
    , simNodeIndeterminacy = NeutroField Nothing ""
    , simNodeFalsehood = NeutroField Nothing ""
    , hideForm = True
    , nodeState = "Simulation"
    }


defaultTargetNodeForm : TargetNodeForm
defaultTargetNodeForm =
    { targetNodeId = 0
    , targetNodeLabel = ""
    , hideForm = True
    , nodeState = "Target"
    }


initModel : Model
initModel =
    -- Elements
    { nodes = []
    , edges = []
    , simulatedNodes = []
    , targetNodes = []

    -- Forms
    , nodeForm = defaultNodeForm
    , edgeForm = defaultEdgeForm
    , simulationForm = defaultSimulationForm
    , targetNodeForm = defaultTargetNodeForm
    , nodeFormDisplay = True
    , nodeTableDisplay = True
    , edgeFormDisplay = True
    , simFormDisplay = True
    , targetFormDisplay = True
    , disableEdgeForm = "card-header m-0 p-1 bg-secondary text-center"
    , disableSimForm = "card-header m-0 p-1 bg-secondary text-center"
    , disableTargetForm = "card-header m-0 p-1 bg-secondary text-center"

    -- Tables
    , edgeTableDisplay = True
    , simTableDisplay = True
    , targetTableDisplay = True
    , currentStateTabDisplay = False
    , simStateTabDisplay = True

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
    , draft = ""
    , messages = []
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
                (\({ node, incoming, outgoing } as ctx) ->
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
                        , distance = 150
                        , strength = Nothing
                        }
                    )

        forces =
            [ Force.customLinks 1 links
            , Force.manyBodyStrength 150 <| List.map .id <| Graph.nodes graph
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
        ]
        [ TypedSvg.title [] [ text node.value.name ] ]


nodeElement node =
    if node.label.value.rank < 5 then
        nodeSize 8 node.label

    else if node.label.value.rank < 9 then
        nodeSize 14 node.label

    else if modBy 2 node.label.value.rank == 0 then
        g []
            [ nodeSize 18 node.label
            , circle
                [ r 8
                , cx node.label.x
                , cy node.label.y
                , fill PaintNone
                , stroke <| Paint <| Color.rgba 255 255 255 1
                ]
                []
            ]

    else
        nodeSize 20 node.label


neutroGraph : Model -> Graph String ()
neutroGraph model =
    let
        nodeList =
            List.map (\node -> node.label) model.nodes

        edgeList =
            List.map (\edge -> ( edge.from, edge.to )) model.edges
    in
    Graph.fromNodeLabelsAndEdgePairs
        nodeList
        edgeList



-- UPDATE


type Msg
    = NoOp
      -- Adding
    | AddNode
    | AddEdge
    | AddSimNode
    | AddTargetNode
      -- Deleting
    | DeleteNode Int
    | DeleteEdge Int
    | DeleteSimNode Int
    | DeleteTargetNode Int
    | DeleteModel
      -- Node
    | UpdateNodeLabel String
    | UpdateNodeTruth String
    | UpdateNodeIndeterminacy String
    | UpdateNodeFalsehood String
      -- Edge
    | UpdateEdgeFrom String
    | UpdateEdgeTo String
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
    | DisplaySimTable
    | DisplayTargetTable
    | DisplayCurState
    | DisplaySimState
      -- Ports
    | Recv String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newCnScore =
            if isNaN (toFloat model.numConnections / toFloat model.numConcepts) == True then
                0.0

            else
                toFloat model.numConnections / toFloat model.numConcepts

        newComplexityScore =
            if isNaN (toFloat model.numTransmitters / toFloat model.numReceivers) == True then
                0.0

            else
                toFloat model.numTransmitters / toFloat model.numReceivers

        newDensityScore =
            if isNaN (toFloat model.numConnections / toFloat model.numConcepts * (toFloat model.numConcepts - 1)) == True then
                0.0

            else
                toFloat model.numConnections / toFloat model.numConcepts * (toFloat model.numConcepts - 1)
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddNode ->
            let
                newNode =
                    let
                        newNodeId =
                            if model.nodeForm.nodeId == 0 then
                                0

                            else
                                model.nodeForm.nodeId + 1

                        newNodeLabel =
                            -- TODO: Create validation to prevent same label nodes
                            model.nodeForm.label

                        newTruth =
                            model.nodeForm.truth
                                |> neutroFieldToString
                                |> String.toFloat
                                |> Maybe.withDefault 0.0

                        newIndeterminacy =
                            model.nodeForm.indeterminacy
                                |> neutroFieldToString
                                |> String.toFloat
                                |> Maybe.withDefault 0.0

                        newFalsehood =
                            model.nodeForm.falsehood
                                |> neutroFieldToString
                                |> String.toFloat
                                |> Maybe.withDefault 0.0
                    in
                    { nodeId = newNodeId
                    , label = newNodeLabel
                    , truth = newTruth
                    , indeterminacy = newIndeterminacy
                    , falsehood = newFalsehood
                    , nodeState = "Reg"
                    }

                newForm =
                    defaultNodeForm
            in
            ( { model
                | nodeForm = newForm
                , nodes = model.nodes ++ [ newNode ]
                , numConcepts = List.length model.nodes + 1
                , cnScore = newCnScore
                , complexityScore = newComplexityScore
                , densityScore = newDensityScore
              }
            , sendMessage newNode.label
            )

        AddEdge ->
            let
                newEdge =
                    let
                        newEdgeId =
                            model.edgeForm.edgeId + 1

                        newFrom =
                            model.edgeForm.from
                                |> nidToString
                                |> String.toInt
                                |> Maybe.withDefault 0

                        newTo =
                            model.edgeForm.to
                                |> nidToString
                                |> String.toInt
                                |> Maybe.withDefault 0

                        newTruth =
                            model.edgeForm.truth
                                |> neutroFieldToString
                                |> String.toFloat
                                |> Maybe.withDefault 0.0

                        newIndeterminacy =
                            model.edgeForm.indeterminacy
                                |> neutroFieldToString
                                |> String.toFloat
                                |> Maybe.withDefault 0.0

                        newFalsehood =
                            model.edgeForm.falsehood
                                |> neutroFieldToString
                                |> String.toFloat
                                |> Maybe.withDefault 0.0
                    in
                    { edgeId = newEdgeId
                    , from = newFrom
                    , to = newTo
                    , truth = newTruth
                    , indeterminacy = newIndeterminacy
                    , falsehood = newFalsehood
                    }

                newEdgeForm =
                    defaultEdgeForm

                newTransmitter =
                    model.edgeForm.from
                        |> nidToString
                        |> String.toInt
                        |> Maybe.withDefault 0

                newReceiver =
                    model.edgeForm.to
                        |> nidToString
                        |> String.toInt
                        |> Maybe.withDefault 0

                newListTransmitters =
                    newTransmitter :: model.listTransmitters

                newListReceivers =
                    newReceiver :: model.listReceivers

                newOrdinaryFromFrom =
                    List.member newTransmitter model.listReceivers

                newOrdinaryFromTo =
                    List.member newReceiver model.listTransmitters

                newListOrdinaries =
                    if newOrdinaryFromFrom == True && newOrdinaryFromTo == True then
                        model.listOrdinaries

                    else if newOrdinaryFromFrom == True && newOrdinaryFromTo == False then
                        newReceiver :: model.listOrdinaries

                    else if newOrdinaryFromFrom == False && newOrdinaryFromTo == True then
                        newTransmitter :: model.listOrdinaries

                    else
                        List.append [ newTransmitter, newReceiver ] model.listOrdinaries

                newNumOrdinary =
                    List.length model.listOrdinaries

                newNumConnections =
                    List.length model.edges + 1

                newNumTransmitters =
                    List.length model.listTransmitters + 1

                newNumReceivers =
                    List.length model.listReceivers + 1
            in
            ( { model
                | edgeForm = newEdgeForm
                , edges = model.edges ++ [ newEdge ]
                , listTransmitters = newListTransmitters
                , listReceivers = newListReceivers
                , listOrdinaries = newListOrdinaries
                , numConnections = newNumConnections
                , numTransmitters = newNumTransmitters
                , numReceivers = newNumReceivers
                , numOrdinary = newNumOrdinary
                , cnScore = newCnScore
                , complexityScore = newComplexityScore
                , densityScore = newDensityScore
              }
            , Cmd.none
            )

        AddSimNode ->
            let
                newSimulationNode =
                    let
                        newSimNodeId =
                            if model.simulationForm.simNodeId == 0 then
                                0

                            else
                                model.simulationForm.simNodeId + 1

                        newSimNodeLabel =
                            model.simulationForm.simNodeLabel

                        newSimNodeTruth =
                            model.simulationForm.simNodeTruth
                                |> neutroFieldToString
                                |> String.toFloat
                                |> Maybe.withDefault 0.0

                        newSimNodeIndeterminacy =
                            model.simulationForm.simNodeIndeterminacy
                                |> neutroFieldToString
                                |> String.toFloat
                                |> Maybe.withDefault 0.0

                        newSimNodeFalsehood =
                            model.simulationForm.simNodeFalsehood
                                |> neutroFieldToString
                                |> String.toFloat
                                |> Maybe.withDefault 0.0
                    in
                    { simNodeId = newSimNodeId
                    , simNodeLabel = newSimNodeLabel
                    , simNodeTruth = newSimNodeTruth
                    , simNodeIndeterminacy = newSimNodeIndeterminacy
                    , simNodeFalsehood = newSimNodeFalsehood
                    , nodeState = "Sim" -- check if needed
                    }

                newSimulationForm =
                    defaultSimulationForm
            in
            ( { model
                | simulationForm = newSimulationForm
                , simulatedNodes =
                    model.simulatedNodes ++ [ newSimulationNode ]
              }
            , Cmd.none
            )

        AddTargetNode ->
            let
                newTargetNode =
                    { targetNodeId = model.targetNodeForm.targetNodeId + 1
                    , targetNodeLabel = model.targetNodeForm.targetNodeLabel
                    , nodeState = "Tar"
                    }

                newTargetNodeForm =
                    defaultTargetNodeForm
            in
            ( { model
                | targetNodeForm = newTargetNodeForm
                , targetNodes =
                    model.targetNodes ++ [ newTargetNode ]
              }
            , Cmd.none
            )

        DeleteNode nodeId ->
            ( { model | nodes = List.filter (\n -> n.nodeId /= nodeId) model.nodes }
            , Cmd.none
            )

        DeleteEdge edgeId ->
            ( { model | edges = List.filter (\n -> n.edgeId /= edgeId) model.edges }
            , Cmd.none
            )

        DeleteSimNode nodeId ->
            ( { model | simulatedNodes = List.filter (\n -> n.simNodeId /= nodeId) model.simulatedNodes }
            , Cmd.none
            )

        DeleteTargetNode nodeId ->
            ( { model | targetNodes = List.filter (\n -> n.targetNodeId /= nodeId) model.targetNodes }
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

                enableEdgeForm =
                    if List.length model.nodes < 2 then
                        "card-header m-0 p-1 bg-secondary text-center"

                    else
                        "card-header m-0 p-1 bg-primary text-center"

                enableSimForm =
                    if model.nodes == [] then
                        "card-header m-0 p-1 bg-secondary text-center"

                    else
                        "card-header m-0 p-1 bg-primary text-center"

                enableTargetForm =
                    if model.nodes == [] then
                        "card-header m-0 p-1 bg-secondary text-center"

                    else
                        "card-header m-0 p-1 bg-primary text-center"
            in
            ( { model
                | nodeForm = newNodeForm
                , draft = model.nodeForm.label
                , disableEdgeForm = enableEdgeForm
                , disableSimForm = enableSimForm
                , disableTargetForm = enableTargetForm
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

        UpdateEdgeFrom newFrom ->
            let
                oldEdgeForm =
                    model.edgeForm

                newEdgeForm =
                    if String.all Char.isDigit newFrom == True then
                        { oldEdgeForm | from = Nid Nothing newFrom }

                    else
                        let
                            maybeFrom =
                                newFrom |> String.toInt
                        in
                        case maybeFrom of
                            Nothing ->
                                { oldEdgeForm | from = Nid Nothing newFrom }

                            Just t ->
                                { oldEdgeForm | from = Nid (Just t) newFrom }
            in
            ( { model | edgeForm = newEdgeForm }, Cmd.none )

        UpdateEdgeTo newTo ->
            let
                oldEdgeForm =
                    model.edgeForm

                newEdgeForm =
                    if String.all Char.isDigit newTo == True then
                        { oldEdgeForm | to = Nid Nothing newTo }

                    else
                        let
                            maybeTo =
                                newTo |> String.toInt
                        in
                        case maybeTo of
                            Nothing ->
                                { oldEdgeForm | to = Nid Nothing newTo }

                            Just t ->
                                { oldEdgeForm | to = Nid (Just t) newTo }
            in
            ( { model | edgeForm = newEdgeForm }, Cmd.none )

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
            in
            ( { model | edgeForm = newEdgeForm }, Cmd.none )

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
                    { oldSimulationForm | simNodeLabel = newSimulatedNodeLabel }
            in
            ( { model | simulationForm = newSimulationForm }, Cmd.none )

        UpdateSimNodeTruth newSimNodeTruth ->
            let
                oldSimNodeForm =
                    model.simulationForm

                newSimNodeForm =
                    if String.right 1 newSimNodeTruth == "." then
                        { oldSimNodeForm | simNodeTruth = NeutroField Nothing newSimNodeTruth }

                    else
                        let
                            maybeTruth =
                                newSimNodeTruth |> String.toFloat
                        in
                        case maybeTruth of
                            Nothing ->
                                { oldSimNodeForm | simNodeTruth = NeutroField Nothing newSimNodeTruth }

                            Just t ->
                                { oldSimNodeForm | simNodeTruth = NeutroField (Just t) newSimNodeTruth }
            in
            ( { model | simulationForm = newSimNodeForm }, Cmd.none )

        UpdateSimNodeIndeterminacy newSimNodeIndeterminacy ->
            let
                oldSimNodeForm =
                    model.simulationForm

                newSimNodeForm =
                    if String.right 1 newSimNodeIndeterminacy == "." then
                        { oldSimNodeForm | simNodeIndeterminacy = NeutroField Nothing newSimNodeIndeterminacy }

                    else
                        let
                            maybeIndeterminacy =
                                newSimNodeIndeterminacy |> String.toFloat
                        in
                        case maybeIndeterminacy of
                            Nothing ->
                                { oldSimNodeForm | simNodeIndeterminacy = NeutroField Nothing newSimNodeIndeterminacy }

                            Just t ->
                                { oldSimNodeForm | simNodeIndeterminacy = NeutroField (Just t) newSimNodeIndeterminacy }
            in
            ( { model | simulationForm = newSimNodeForm }, Cmd.none )

        UpdateSimNodeFalsehood newSimNodeFalsehood ->
            let
                oldSimNodeForm =
                    model.simulationForm

                newSimNodeForm =
                    if String.right 1 newSimNodeFalsehood == "." then
                        { oldSimNodeForm | simNodeFalsehood = NeutroField Nothing newSimNodeFalsehood }

                    else
                        let
                            maybeFalsehood =
                                newSimNodeFalsehood |> String.toFloat
                        in
                        case maybeFalsehood of
                            Nothing ->
                                { oldSimNodeForm | simNodeFalsehood = NeutroField Nothing newSimNodeFalsehood }

                            Just t ->
                                { oldSimNodeForm | simNodeFalsehood = NeutroField (Just t) newSimNodeFalsehood }
            in
            ( { model | simulationForm = newSimNodeForm }, Cmd.none )

        UpdateTargetNodeLabel newTargetNodeLabel ->
            let
                oldTargetNodeForm =
                    model.targetNodeForm

                newTargetNodeForm =
                    { oldTargetNodeForm | targetNodeLabel = newTargetNodeLabel }
            in
            ( { model | targetNodeForm = newTargetNodeForm }, Cmd.none )

        DisplayCurState ->
            ( { model
                | currentStateTabDisplay = False
                , simStateTabDisplay = True
              }
            , Cmd.none
            )

        DisplaySimState ->
            ( { model
                | simStateTabDisplay = False
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
                , disableEdgeForm =
                    if List.length model.nodes < 2 then
                        "card-header m-0 p-1 bg-secondary text-center"

                    else
                        "card-header m-0 p-1 bg-primary text-center"
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
                , disableSimForm =
                    if model.nodes == [] then
                        "card-header m-0 p-1 bg-secondary text-center"

                    else
                        "card-header m-0 p-1 bg-primary text-center"
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
                , disableTargetForm =
                    if model.nodes == [] then
                        "card-header m-0 p-1 bg-secondary text-center"

                    else
                        "card-header m-0 p-1 bg-primary text-center"
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

        DisplaySimTable ->
            ( { model
                | simTableDisplay = isToggled model.simTableDisplay
              }
            , Cmd.none
            )

        DisplayTargetTable ->
            ( { model
                | targetTableDisplay = isToggled model.targetTableDisplay
              }
            , Cmd.none
            )

        Recv message ->
            ( { model | messages = model.messages ++ [ message ] }
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
        [ class "col-3 text-center m-0 p-0"
        , style "height" "100vh"
        ]
        [ div
            [ class "d-flex" ]
            [ div
                [ class "shadow d-inline-block"
                , style "max-width" "80px"
                , style "height" "100vh"
                ]
                [ div
                    [ class "w-full m-0 p-auto" ]
                    [ viewMenuButton "Save"
                    , viewMenuButton "Open"
                    , viewMenuButton "Import"
                    , viewMenuButton "Export"
                    , viewMenuButton "Logout"
                    ]
                ]
            , div
                [ class "container-fluid m-0 p-0 d-inline-block"
                , style "height" "100vh"
                ]
                [ viewNodeInputForm model model.nodeForm
                , viewEdgeInputForm model model.edgeForm
                , viewSimulationInputForm model model.simulationForm
                , viewTargetNodeForm model model.targetNodeForm
                , viewModelControl
                ]
            ]
        ]


viewGraphCanvas : Model -> Html Msg
viewGraphCanvas model =
    div
        [ class "col-6 bg-dark text-white" ]
        [ svg
            [ viewBox 0 0 w h ]
            [ g [ TypedSvg.Attributes.class [ "links" ] ] <|
                List.map (linkElement (initGraph model)) <|
                    Graph.edges (initGraph model)
            , g [ TypedSvg.Attributes.class [ "nodes" ] ] <|
                List.map nodeElement <|
                    Graph.nodes (initGraph model)
            ]
        ]


viewRightMenuBar : Model -> Html Msg
viewRightMenuBar model =
    div
        [ class "shadow bar-scroll col-3" ]
        [ div [ class "m-0 w-100" ]
            [ viewTabMenu
            , div
                [ hidden model.currentStateTabDisplay ]
                [ viewCurrentState model ]
            , div
                [ hidden model.simStateTabDisplay ]
                [ viewSimulationResults model ]
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
                    , viewFormButton "Add Node"
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
                    [ viewInputNodeLabel "60px" "mb-3 mx-1" "From" (nidToString edge.from) UpdateEdgeFrom
                    , viewInputNodeLabel "60px" "mb-3 mx-1" "To" (nidToString edge.to) UpdateEdgeTo
                    , viewInputNumber "Tru" edge.truth UpdateEdgeTruth
                    , viewInputNumber "Ind" edge.indeterminacy UpdateEdgeIndeterminacy
                    , viewInputNumber "Fal" edge.falsehood UpdateEdgeFalsehood
                    , viewFormButton "Add Edge"
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
                    [ viewNodesList "Label" "mb-3" "140px" UpdateSimNodeLabel simNode.simNodeLabel model.nodes
                    , viewInputNumber "Tru" simNode.simNodeTruth UpdateSimNodeTruth
                    , viewInputNumber "Ind" simNode.simNodeIndeterminacy UpdateSimNodeIndeterminacy
                    , viewInputNumber "Fal" simNode.simNodeFalsehood UpdateSimNodeFalsehood
                    , viewFormButton "Simulate Node"
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
                    [ viewNodesList "Label" "mb-3" "140px" UpdateTargetNodeLabel targetNode.targetNodeLabel model.nodes
                    , viewFormButton "Add Target"
                    ]
                ]
            ]
        ]


viewModelControl : Html Msg
viewModelControl =
    div
        [ class "accordion mt-3" ]
        [ div
            [ class "d-inline-block mx-auto my-5" ]
            [ viewControlButton "Run Model" "btn-outline-success" DeleteModel
            , viewControlButton "Delete Model" "btn-outline-danger" DeleteModel
            ]
        ]



-- TABLES


viewCurrentState : Model -> Html Msg
viewCurrentState model =
    div
        [ class "bg-dark w-100 mt-4" ]
        [ viewKpiTable "KPIs" model
        , viewNodeTable "Nodes" viewNodes model.nodes model
        , viewEdgeTable "Edges" viewEdges model.edges model
        , viewSimTable "Simulation" viewSimulatedNodes model.simulatedNodes model
        , viewTargetTable "Target" viewTargetNodes model.targetNodes model
        ]


viewSimulationResults model =
    div []
        [ h1 [] [ text "Simulation Results" ]
        , ul
            [ class "text-white"
            , style "style" "none"
            ]
            (List.map (\msg -> li [] [ text msg ]) model.messages)
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
        , td [ class "tb-header-label text-white text-center border-0" ] [ text node.nodeState ]
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


viewEdges : List NeutroEdge -> Html Msg
viewEdges edges =
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
            :: List.map viewEdge edges
        )


viewEdge : NeutroEdge -> Html Msg
viewEdge edge =
    tr []
        [ td [ class "tb-header-label text-white align-middle text-left border-0" ] [ text (String.fromInt edge.from) ]
        , td [ class "tb-header-label text-white align-middle text-center border-0" ] [ text (String.fromInt edge.to) ]
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


viewSimulatedNodes : List SimulatedNode -> Html Msg
viewSimulatedNodes simulatedNodes =
    table
        [ class "table" ]
        (tr
            [ class "border-bottom border-secondary" ]
            [ td [ class "tb-header-label text-white text-left" ] [ text "Label" ]
            , td [ class "tb-header-label text-white text-center" ] [ text "-" ]
            , td [ class "tb-header-label text-white text-right" ] [ text "Tru" ]
            , td [ class "tb-header-label text-white text-right" ] [ text "Ind" ]
            , td [ class "tb-header-label text-white text-right" ] [ text "Fal" ]
            , td [ class "tb-header-label text-white text-right" ] [ text "" ]
            ]
            :: List.map viewSimulatedNode simulatedNodes
        )


viewSimulatedNode : SimulatedNode -> Html Msg
viewSimulatedNode simulatedNode =
    tr []
        [ td [ class "tb-header-label text-white align-middle text-left border-0" ] [ text simulatedNode.simNodeLabel ]
        , td [ class "tb-header-label text-white text-center border-0" ] [ text "-" ]
        , td [ class "tb-header-label text-white align-middle text-right border-0" ] [ text (String.fromFloat simulatedNode.simNodeTruth) ]
        , td [ class "tb-header-label text-white align-middle text-right border-0" ] [ text (String.fromFloat simulatedNode.simNodeIndeterminacy) ]
        , td [ class "tb-header-label text-white align-middle text-right border-0" ] [ text (String.fromFloat simulatedNode.simNodeFalsehood) ]
        , td [ class "tb-header-label text-white align-middle text-right border-0" ]
            [ a
                [ class "tb-header-label text-danger font-weight-bold"
                , type_ "button"
                , onClick (DeleteSimNode simulatedNode.simNodeId)
                ]
                [ text "X" ]
            ]
        ]


viewTargetNodes : List TargetNode -> Html Msg
viewTargetNodes targetNodes =
    table
        [ class "table" ]
        (tr
            [ class "border-bottom border-secondary" ]
            [ td [ class "tb-header-label text-white text-left" ] [ text "Label" ]
            , td [ class "tb-header-label text-white text-right" ] [ text "" ]
            ]
            :: List.map viewTargetNode targetNodes
        )


viewTargetNode : TargetNode -> Html Msg
viewTargetNode targetNode =
    tr []
        [ td [ class "tb-header-label text-white align-middle text-left border-0" ] [ text targetNode.targetNodeLabel ]
        , td [ class "tb-header-label text-white align-middle text-right border-0" ]
            [ a
                [ class "tb-header-label text-danger font-weight-bold"
                , type_ "button"
                , onClick (DeleteTargetNode targetNode.targetNodeId)
                ]
                [ text "X" ]
            ]
        ]



-- SHARED COMPONENTS


viewInputNumber : String -> NeutroField -> (String -> msg) -> Html msg
viewInputNumber p val msg =
    input
        [ type_ "number"
        , class "mx-1"
        , style "width" "41px"
        , placeholder p
        , Html.Attributes.min "0.0"
        , Html.Attributes.max "1.0"
        , step "0.01"
        , required True
        , value (neutroFieldToString val)
        , onInput msg
        ]
        []


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


viewNodesList : String -> String -> String -> (String -> Msg) -> String -> List NeutroNode -> Html Msg
viewNodesList p c size msg label nodes =
    select
        [ onInput msg
        , class c
        , style "width" size
        , placeholder p
        , required True
        , value label
        ]
        (option
            [ class "tb-header-label text-left" ]
            []
            :: List.map viewNodeOpt nodes
        )


viewNodeOpt : NeutroNode -> Html Msg
viewNodeOpt node =
    option [ class "tb-header-label" ] [ text node.label ]


viewFormButton : String -> Html msg
viewFormButton p =
    button
        [ class "btn btn-sm btn-outline-primary w-100 mt-3"
        , type_ "submit"
        , style "border-radius" "2rem"
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


viewControlButton : String -> String -> msg -> Html msg
viewControlButton p c msg =
    button
        [ class "shadow btn btn-sm w-50 mx-auto mb-2 px-1"
        , class c
        , type_ "submit"
        , style "border-radius" "2rem"
        , style "min-width" "180px"
        , onClick msg
        ]
        [ text p ]


viewFormHeader : String -> String -> Msg -> Html Msg
viewFormHeader title cls msg =
    div
        [ class cls ]
        [ button
            [ class "btn mt-1 p-0"
            , type_ "button"
            ]
            [ h6
                [ class "text-white"
                , onClick msg
                ]
                [ text title ]
            ]
        ]


viewTabMenu : Html Msg
viewTabMenu =
    div
        [ class "tab d-flex bg-dark w-100 mt-2" ]
        [ button
            [ class "w-100 text-white float-left p-1"
            , style "cursor" "pointer"
            , onClick DisplayCurState
            ]
            [ text "Current State" ]
        , button
            [ class "w-100 text-white p-1"
            , style "cursor" "pointer"
            , onClick DisplaySimState
            ]
            [ text "Simulation" ]
        ]


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


viewEdgeTable title func nodes model =
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
            [ func nodes ]
        ]


viewSimTable title func nodes model =
    div [ class "m-0 w-100" ]
        [ div
            [ class "d-flex bg-dark w-100 m-0 border-bottom" ]
            [ p
                [ class "p-1 m-0 text-primary" ]
                [ text title ]
            , div
                [ class "w-100 text-right text-primary pr-1"
                , style "cursor" "pointer"
                , onClick DisplaySimTable
                ]
                [ text "▼" ]
            ]
        , div
            [ hidden model.simTableDisplay ]
            [ func nodes ]
        ]


viewTargetTable title func nodes model =
    div [ class "m-0 w-100" ]
        [ div
            [ class "d-flex bg-dark w-100 m-0 border-bottom" ]
            [ p
                [ class "p-1 m-0 text-primary" ]
                [ text title ]
            , div
                [ class "w-100 text-right text-primary pr-1"
                , style "cursor" "pointer"
                , onClick DisplayTargetTable
                ]
                [ text "▼" ]
            ]
        , div
            [ hidden model.targetTableDisplay ]
            [ func nodes ]
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
        , td [ class "tb-header-label text-white align-middle text-right pl-3" ] [ text (String.fromFloat kpi) ]
        ]



-- HELPERS


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


ifIsEnter : msg -> Decode.Decoder msg
ifIsEnter msg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Enter" then
                    Decode.succeed msg

                else
                    Decode.fail "some other key"
            )


isToggled : Bool -> Bool
isToggled formToggle =
    if formToggle == True then
        False

    else
        True
