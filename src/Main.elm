port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Color exposing (Color)
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, hidden, id, placeholder, required, step, style, type_, value)
import Html.Events exposing (..)
import Html.Events.Extra.Mouse as Mouse
import IntDict
import Json.Decode as Decode
import List exposing (range)
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
    Browser.document
        { init = init
        , view = \model -> { title = "NeutroModeler", body = [ view model ] }
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



-- MODEL


type alias Model =
    { nodes : List NeutroNode
    , edges : List NeutroEdge
    , simulatedNodes : List SimulatedNode
    , targetNodes : List TargetNode
    , nodeForm : NodeForm
    , edgeForm : EdgeForm
    , simulationForm : SimulationForm
    , targetNodeForm : TargetNodeForm
    , nodeFormDisplay : Bool
    , edgeFormDisplay : Bool
    , simFormDisplay : Bool
    , targetFormDisplay : Bool
    }



-- TYPES


type alias NodeId =
    Int


type alias NeutroNode =
    { nodeId : NodeId
    , label : String
    , truth : Float
    , indeterminacy : Float
    , falsehood : Float
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
    }


type alias TargetNode =
    { targetNodeId : NodeId
    , targetNodeLabel : String
    , targetNodeTruth : Float
    , targetNodeIndeterminacy : Float
    , targetNodeFalsehood : Float
    }



-- FORMS


type From
    = From (Maybe Int) String


type To
    = To (Maybe Int) String


type NeutroField
    = NeutroField (Maybe Float) String


type alias NodeForm =
    { nodeId : Int
    , label : String
    , truth : NeutroField
    , indeterminacy : NeutroField
    , falsehood : NeutroField
    , hideForm : Bool
    }


type alias EdgeForm =
    { edgeId : Int
    , from : From
    , to : To
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
    }


type alias TargetNodeForm =
    { targetNodeId : Int
    , targetNodeLabel : String
    , targetNodeTruth : NeutroField
    , targetNodeIndeterminacy : NeutroField
    , targetNodeFalsehood : NeutroField
    , hideForm : Bool
    }



-- GRAPH TYPES


type alias Entity =
    Force.Entity NodeId { value : CustomNode }


type alias CustomNode =
    { rank : Int, name : String }



-- MODEL DEFAULTS


hideNodeForm : NodeForm
hideNodeForm =
    { nodeId = 0
    , label = ""
    , truth = NeutroField Nothing ""
    , indeterminacy = NeutroField Nothing ""
    , falsehood = NeutroField Nothing ""
    , hideForm = True
    }


displayNodeForm : NodeForm
displayNodeForm =
    { nodeId = 0
    , label = ""
    , truth = NeutroField Nothing ""
    , indeterminacy = NeutroField Nothing ""
    , falsehood = NeutroField Nothing ""
    , hideForm = False
    }


hideEdgeForm : EdgeForm
hideEdgeForm =
    { edgeId = 0
    , from = From Nothing ""
    , to = To Nothing ""
    , truth = NeutroField Nothing ""
    , indeterminacy = NeutroField Nothing ""
    , falsehood = NeutroField Nothing ""
    , hideForm = True
    }


displayEdgeForm : EdgeForm
displayEdgeForm =
    { edgeId = 0
    , from = From Nothing ""
    , to = To Nothing ""
    , truth = NeutroField Nothing ""
    , indeterminacy = NeutroField Nothing ""
    , falsehood = NeutroField Nothing ""
    , hideForm = False
    }


hideSimulationForm : SimulationForm
hideSimulationForm =
    { simNodeId = 0
    , simNodeLabel = ""
    , simNodeTruth = NeutroField Nothing ""
    , simNodeIndeterminacy = NeutroField Nothing ""
    , simNodeFalsehood = NeutroField Nothing ""
    , hideForm = True
    }


displaySimulationForm : SimulationForm
displaySimulationForm =
    { simNodeId = 0
    , simNodeLabel = ""
    , simNodeTruth = NeutroField Nothing ""
    , simNodeIndeterminacy = NeutroField Nothing ""
    , simNodeFalsehood = NeutroField Nothing ""
    , hideForm = False
    }


hideTargetNodeForm : TargetNodeForm
hideTargetNodeForm =
    { targetNodeId = 0
    , targetNodeLabel = ""
    , targetNodeTruth = NeutroField Nothing ""
    , targetNodeIndeterminacy = NeutroField Nothing ""
    , targetNodeFalsehood = NeutroField Nothing ""
    , hideForm = True
    }


displayTargetNodeForm : TargetNodeForm
displayTargetNodeForm =
    { targetNodeId = 0
    , targetNodeLabel = ""
    , targetNodeTruth = NeutroField Nothing ""
    , targetNodeIndeterminacy = NeutroField Nothing ""
    , targetNodeFalsehood = NeutroField Nothing ""
    , hideForm = False
    }


initModel : Model
initModel =
    { nodes = []
    , edges = []
    , simulatedNodes = []
    , targetNodes = []
    , nodeForm = hideNodeForm
    , edgeForm = hideEdgeForm
    , simulationForm = hideSimulationForm
    , targetNodeForm = hideTargetNodeForm
    , nodeFormDisplay = True
    , edgeFormDisplay = True
    , simFormDisplay = True
    , targetFormDisplay = True
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
    504


colorScale : SequentialScale Color
colorScale =
    Scale.sequential Scale.Color.viridisInterpolator ( 200, 700 )


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
                        , distance = 300
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
        [ strokeWidth 3.5
        , stroke <| Paint <| Scale.convert colorScale source.x
        , x1 source.x
        , y1 source.y
        , x2 target.x
        , y2 target.y
        ]
        []


hexagon ( x, y ) size attrs =
    let
        angle =
            2 * pi / 6

        p =
            range 0 6
                |> List.map toFloat
                |> List.map (\a -> ( x + cos (a * angle) * 2 * size, y + sin (a * angle) * 2 * size ))
                |> points
    in
    polygon
        (p :: attrs)


nodeSize size node =
    hexagon ( node.x, node.y )
        size
        [ fill <| Paint <| Scale.convert colorScale node.x
        ]
        [ TypedSvg.title [] [ text node.value.name ] ]


nodeElement node =
    if node.label.value.rank < 5 then
        nodeSize 16 node.label

    else if node.label.value.rank < 9 then
        nodeSize 28 node.label

    else if modBy 2 node.label.value.rank == 0 then
        g []
            [ nodeSize 36 node.label
            , circle
                [ r 48
                , cx node.label.x
                , cy node.label.y
                , fill PaintNone
                , stroke <| Paint <| Scale.convert colorScale node.label.x
                ]
                []
            ]

    else
        nodeSize 40 node.label



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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddNode ->
            let
                newNode =
                    { nodeId = model.nodeForm.nodeId + 1
                    , label = model.nodeForm.label
                    , truth = model.nodeForm.truth |> neutroFieldToString |> String.toFloat |> Maybe.withDefault 0.0
                    , indeterminacy = model.nodeForm.indeterminacy |> neutroFieldToString |> String.toFloat |> Maybe.withDefault 0.0
                    , falsehood = model.nodeForm.falsehood |> neutroFieldToString |> String.toFloat |> Maybe.withDefault 0.0
                    }

                newForm =
                    { hideNodeForm | nodeId = 0 }
            in
            ( { model
                | nodeForm = newForm
                , nodes =
                    model.nodes ++ [ newNode ]
              }
            , Cmd.none
            )

        AddEdge ->
            let
                newEdge =
                    { edgeId = model.edgeForm.edgeId + 1
                    , from = model.edgeForm.from |> edgeOriginToString |> String.toInt |> Maybe.withDefault 0
                    , to = model.edgeForm.to |> edgeDestinyToString |> String.toInt |> Maybe.withDefault 0
                    , truth = model.edgeForm.truth |> neutroFieldToString |> String.toFloat |> Maybe.withDefault 0.0
                    , indeterminacy = model.edgeForm.indeterminacy |> neutroFieldToString |> String.toFloat |> Maybe.withDefault 0.0
                    , falsehood = model.edgeForm.falsehood |> neutroFieldToString |> String.toFloat |> Maybe.withDefault 0.0
                    }

                newEdgeForm =
                    { hideEdgeForm | edgeId = 0 }
            in
            ( { model
                | edgeForm = newEdgeForm
                , edges =
                    model.edges ++ [ newEdge ]
              }
            , Cmd.none
            )

        AddSimNode ->
            let
                newSimulationNode =
                    { simNodeId = model.simulationForm.simNodeId + 1
                    , simNodeLabel = model.simulationForm.simNodeLabel
                    , simNodeTruth = model.simulationForm.simNodeTruth |> neutroFieldToString |> String.toFloat |> Maybe.withDefault 0.0
                    , simNodeIndeterminacy = model.simulationForm.simNodeIndeterminacy |> neutroFieldToString |> String.toFloat |> Maybe.withDefault 0.0
                    , simNodeFalsehood = model.simulationForm.simNodeFalsehood |> neutroFieldToString |> String.toFloat |> Maybe.withDefault 0.0
                    }

                newSimulationForm =
                    { hideSimulationForm | simNodeId = 0 }
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
                    , targetNodeTruth = model.targetNodeForm.targetNodeTruth |> neutroFieldToString |> String.toFloat |> Maybe.withDefault 0.0
                    , targetNodeIndeterminacy = model.targetNodeForm.targetNodeIndeterminacy |> neutroFieldToString |> String.toFloat |> Maybe.withDefault 0.0
                    , targetNodeFalsehood = model.targetNodeForm.targetNodeFalsehood |> neutroFieldToString |> String.toFloat |> Maybe.withDefault 0.0
                    }

                newTargetNodeForm =
                    { hideTargetNodeForm | targetNodeId = 0 }
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

        UpdateNodeLabel newLabel ->
            let
                oldForm =
                    model.nodeForm

                newForm =
                    { oldForm | label = newLabel }
            in
            ( { model | nodeForm = newForm }, Cmd.none )

        UpdateNodeTruth newTruth ->
            let
                oldForm =
                    model.nodeForm

                newForm =
                    if String.right 1 newTruth == "." then
                        { oldForm | truth = NeutroField Nothing newTruth }

                    else
                        let
                            maybeTruth =
                                newTruth |> String.toFloat
                        in
                        case maybeTruth of
                            Nothing ->
                                { oldForm | truth = NeutroField Nothing newTruth }

                            Just t ->
                                { oldForm | truth = NeutroField (Just t) newTruth }
            in
            ( { model | nodeForm = newForm }, Cmd.none )

        UpdateNodeIndeterminacy newIndeterminacy ->
            let
                oldForm =
                    model.nodeForm

                newForm =
                    if String.right 1 newIndeterminacy == "." then
                        { oldForm | indeterminacy = NeutroField Nothing newIndeterminacy }

                    else
                        let
                            maybeIndeterminacy =
                                newIndeterminacy |> String.toFloat
                        in
                        case maybeIndeterminacy of
                            Nothing ->
                                { oldForm | indeterminacy = NeutroField Nothing newIndeterminacy }

                            Just p ->
                                { oldForm | indeterminacy = NeutroField (Just p) newIndeterminacy }
            in
            ( { model | nodeForm = newForm }, Cmd.none )

        UpdateNodeFalsehood newFalsehood ->
            let
                oldForm =
                    model.nodeForm

                newForm =
                    if String.right 1 newFalsehood == "." then
                        { oldForm | falsehood = NeutroField Nothing newFalsehood }

                    else
                        let
                            maybeFalsehood =
                                newFalsehood |> String.toFloat
                        in
                        case maybeFalsehood of
                            Nothing ->
                                { oldForm | falsehood = NeutroField Nothing newFalsehood }

                            Just p ->
                                { oldForm | falsehood = NeutroField (Just p) newFalsehood }
            in
            ( { model | nodeForm = newForm }, Cmd.none )

        UpdateEdgeFrom newFrom ->
            let
                oldEdgeForm =
                    model.edgeForm

                newEdgeForm =
                    if String.all Char.isDigit newFrom == True then
                        { oldEdgeForm | from = From Nothing newFrom }

                    else
                        let
                            maybeFrom =
                                newFrom |> String.toInt
                        in
                        case maybeFrom of
                            Nothing ->
                                { oldEdgeForm | from = From Nothing newFrom }

                            Just t ->
                                { oldEdgeForm | from = From (Just t) newFrom }
            in
            ( { model | edgeForm = newEdgeForm }, Cmd.none )

        UpdateEdgeTo newTo ->
            let
                oldEdgeForm =
                    model.edgeForm

                newEdgeForm =
                    if String.all Char.isDigit newTo == True then
                        { oldEdgeForm | to = To Nothing newTo }

                    else
                        let
                            maybeTo =
                                newTo |> String.toInt
                        in
                        case maybeTo of
                            Nothing ->
                                { oldEdgeForm | to = To Nothing newTo }

                            Just t ->
                                { oldEdgeForm | to = To (Just t) newTo }
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

        DisplayNodeForm ->
            ( { model
                | nodeFormDisplay =
                    if model.nodeFormDisplay == True then
                        False

                    else
                        True
                , nodeForm =
                    if model.nodeFormDisplay == True then
                        hideNodeForm

                    else
                        displayNodeForm
              }
            , Cmd.none
            )

        DisplayEdgeForm ->
            ( { model
                | edgeFormDisplay =
                    if model.edgeFormDisplay == True then
                        False

                    else
                        True
                , edgeForm =
                    if model.edgeFormDisplay == True then
                        hideEdgeForm

                    else
                        displayEdgeForm
              }
            , Cmd.none
            )

        DisplaySimForm ->
            ( { model
                | simFormDisplay =
                    if model.simFormDisplay == True then
                        False

                    else
                        True
                , simulationForm =
                    if model.simFormDisplay == True then
                        hideSimulationForm

                    else
                        displaySimulationForm
              }
            , Cmd.none
            )

        DisplayTargetForm ->
            ( { model
                | targetFormDisplay =
                    if model.targetFormDisplay == True then
                        False

                    else
                        True
                , targetNodeForm =
                    if model.targetFormDisplay == True then
                        hideTargetNodeForm

                    else
                        displayTargetNodeForm
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "app bg-dark" ]
        [ div
            [ class "bar-scroll col-3 text-center"
            , style "position" "relative"
            ]
            [ div
                [ style "position" "absolute"
                , style "top" "50%"
                , style "transform" "translate(-50%, -50%)"
                , style "display" "inline-block"
                ]
                [ viewNodeForm model model.nodeForm
                , viewEdgeForm model model.edgeForm
                , viewSimulationForm model model.simulationForm
                , viewTargetNodeForm model model.targetNodeForm
                , button
                    [ class "btn btn-sm btn-outline-success w-50 mt-5 px-1"
                    , type_ "submit"
                    , style "border-radius" "2rem"
                    ]
                    [ text "Run" ]
                , button
                    [ class "btn btn-sm btn-outline-danger w-50 mt-5 px-1"
                    , type_ "submit"
                    , style "border-radius" "2rem"
                    ]
                    [ text "Cancel" ]
                ]
            ]
        , div [ class "col-6 bg-dark text-white" ]
            [ svg [ viewBox 0 0 w h ]
                [ g [ TypedSvg.Attributes.class [ "links" ] ] <|
                    List.map (linkElement (initGraph model)) <|
                        Graph.edges (initGraph model)
                , g [ TypedSvg.Attributes.class [ "nodes" ] ] <|
                    List.map nodeElement <|
                        Graph.nodes (initGraph model)
                ]
            ]
        , div
            [ class "shadow bar-scroll col-3 border-left border-secondary" ]
            [ viewTable "Nodes" viewNodes model.nodes
            , viewTable "Edges" viewEdges model.edges
            , viewTable "Simulation" viewSimulatedNodes model.simulatedNodes
            , viewTable "Target" viewTargetNodes model.targetNodes
            ]
        ]



-- FORMS


viewNodeForm : Model -> NodeForm -> Html Msg
viewNodeForm model node =
    div
        [ class "accordion mt-3" ]
        [ div
            [ class "shadow card border-0 text-center"
            , style "border-radius" "2rem"
            , style "width" "180px"
            ]
            [ viewFormHeader "Node" DisplayNodeForm
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
                    [ viewInputNodeLabel "Label" node.label UpdateNodeLabel
                    , viewInputNumber "Tru" node.truth UpdateNodeTruth
                    , viewInputNumber "Ind" node.indeterminacy UpdateNodeIndeterminacy
                    , viewInputNumber "Fal" node.falsehood UpdateNodeFalsehood
                    , viewFormButton "Add Node"
                    ]
                ]
            ]
        ]


viewEdgeForm : Model -> EdgeForm -> Html Msg
viewEdgeForm model edge =
    div
        [ class "accordion mt-3" ]
        [ div
            [ class "shadow card border-0 text-center"
            , style "border-radius" "2rem"
            , style "width" "180px"
            ]
            [ viewFormHeader "Edge" DisplayEdgeForm
            , form
                [ class "collapse show"
                , style "display" "inline-block"
                , hidden model.edgeFormDisplay
                , onSubmit AddEdge
                ]
                [ viewInputEdgeLabel "From" (edgeOriginToString edge.from) UpdateEdgeFrom
                , viewInputEdgeLabel "To" (edgeDestinyToString edge.to) UpdateEdgeTo
                , viewInputNumber "Tru" edge.truth UpdateEdgeTruth
                , viewInputNumber "Ind" edge.indeterminacy UpdateEdgeIndeterminacy
                , viewInputNumber "Fal" edge.falsehood UpdateEdgeFalsehood
                , viewFormButton "Add Edge"
                ]
            ]
        ]


viewSimulationForm : Model -> SimulationForm -> Html Msg
viewSimulationForm model simNode =
    div
        [ class "accordion mt-3"
        , id "accordionExample"
        ]
        [ div
            [ class "shadow card border-0 text-center"
            , style "border-radius" "2rem"
            , style "width" "180px"
            ]
            [ viewFormHeader "Simulate" DisplaySimForm
            , form
                [ class "collapse show"
                , style "display" "inline-block"
                , hidden model.simFormDisplay
                , onSubmit AddSimNode
                ]
                [ div [ class "card-body p-3" ]
                    [ viewInputNodeLabel "Label" simNode.simNodeLabel UpdateSimNodeLabel
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
            [ class "shadow card border-0 text-center"
            , style "border-radius" "2rem"
            , style "width" "180px"
            ]
            [ viewFormHeader "Target" DisplayTargetForm
            , form
                [ class "collapse show"
                , style "display" "inline-block"
                , hidden model.targetFormDisplay
                , onSubmit AddTargetNode
                ]
                [ div [ class "card-body p-3" ]
                    [ viewInputNodeLabel "Label" targetNode.targetNodeLabel UpdateTargetNodeLabel
                    , viewFormButton "Add Target"
                    ]
                ]
            ]
        ]



-- TABLES


viewNodes : List NeutroNode -> Html Msg
viewNodes nodes =
    table
        [ class "table" ]
        (tr
            [ class "border-bottom border-secondary" ]
            [ th [ class "tb-header-label text-white text-left" ] [ text "Label" ]
            , th [ class "tb-header-label text-white text-right" ] [ text "Tru" ]
            , th [ class "tb-header-label text-white text-right" ] [ text "Ind" ]
            , th [ class "tb-header-label text-white text-right" ] [ text "Fal" ]
            , th [ class "tb-header-label text-white text-right" ] [ text "" ]
            ]
            :: List.map viewNode nodes
        )


viewNode : NeutroNode -> Html Msg
viewNode node =
    tr []
        [ td [ class "tb-header-label align-center text-white align-middle text-left border-0" ] [ text node.label ]
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
            [ th [ class "tb-header-label text-white text-right text-center" ] [ text "From" ]
            , th [ class "tb-header-label text-white text-right text-center" ] [ text "To" ]
            , th [ class "tb-header-label text-white text-right text-right" ] [ text "Tru" ]
            , th [ class "tb-header-label text-white text-right text-right" ] [ text "Ind" ]
            , th [ class "tb-header-label text-white text-right text-right" ] [ text "Fal" ]
            , th [ class "tb-header-label text-white text-right text-right" ] [ text "" ]
            ]
            :: List.map viewEdge edges
        )


viewEdge : NeutroEdge -> Html Msg
viewEdge edge =
    tr []
        [ td [ class "tb-header-label text-white align-middle text-center border-0" ] [ text (String.fromInt edge.from) ]
        , td [ class "tb-header-label text-white align-middle text-center border-0" ] [ text (String.fromInt edge.to) ]
        , td [ class "tb-header-label text-white align-middle text-right border-0" ] [ text (String.fromFloat edge.truth) ]
        , td [ class "tb-header-label text-white align-middle text-right border-0" ] [ text (String.fromFloat edge.indeterminacy) ]
        , td [ class "tb-header-label text-white align-middle text-right border-0" ] [ text (String.fromFloat edge.falsehood) ]
        , td [ class "tb-header-label text-white align-middle text-right border-0" ]
            [ button
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
            [ th [ class "tb-header-label text-white text-left" ] [ text "Label" ]
            , th [ class "tb-header-label text-white text-right" ] [ text "Tru" ]
            , th [ class "tb-header-label text-white text-right" ] [ text "Ind" ]
            , th [ class "tb-header-label text-white text-right" ] [ text "Fal" ]
            , th [ class "tb-header-label text-white text-right" ] [ text "" ]
            ]
            :: List.map viewSimulatedNode simulatedNodes
        )


viewSimulatedNode : SimulatedNode -> Html Msg
viewSimulatedNode simulatedNode =
    tr []
        [ td [ class "tb-header-label text-white align-middle text-left border-0" ] [ text simulatedNode.simNodeLabel ]
        , td [ class "tb-header-label text-white align-middle text-right border-0" ] [ text (String.fromFloat simulatedNode.simNodeTruth) ]
        , td [ class "tb-header-label text-white align-middle text-right border-0" ] [ text (String.fromFloat simulatedNode.simNodeIndeterminacy) ]
        , td [ class "tb-header-label text-white align-middle text-right border-0" ] [ text (String.fromFloat simulatedNode.simNodeFalsehood) ]
        , td [ class "tb-header-label text-white align-middle text-right border-0" ]
            [ button
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
            [ th [ class "tb-header-label text-white text-left" ] [ text "Label" ]
            , th [ class "tb-header-label text-white text-right" ] [ text "" ]
            ]
            :: List.map viewTargetNode targetNodes
        )


viewTargetNode : TargetNode -> Html Msg
viewTargetNode targetNode =
    tr []
        [ td [ class "tb-header-label text-white align-middle text-left border-0" ] [ text targetNode.targetNodeLabel ]
        , td [ class "tb-header-label text-white align-middle text-right border-0" ]
            [ button
                [ class "tb-header-label text-danger font-weight-bold"
                , type_ "button"
                , onClick (DeleteTargetNode targetNode.targetNodeId)
                ]
                [ text "X" ]
            ]
        ]



-- GRAPH


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
        , step "0.0001"
        , required True
        , value (neutroFieldToString val)
        , onInput msg
        ]
        []


viewInputNodeLabel : String -> String -> (String -> msg) -> Html msg
viewInputNodeLabel p val msg =
    input
        [ type_ "text"
        , class "mb-3"
        , style "width" "140px"
        , placeholder p
        , required True
        , autofocus True
        , value val
        , onInput msg
        ]
        []


viewInputEdgeLabel : String -> String -> (String -> msg) -> Html msg
viewInputEdgeLabel p val msg =
    input
        [ type_ "text"
        , class "my-2 w-50"
        , placeholder p
        , required True
        , autofocus True
        , value val
        , onInput msg
        ]
        []


viewFormButton : String -> Html msg
viewFormButton p =
    button
        [ class "btn btn-sm btn-outline-primary w-100 mt-3"
        , type_ "submit"
        , style "border-radius" "2rem"
        ]
        [ text p ]


viewFormHeader : String -> Msg -> Html Msg
viewFormHeader title msg =
    div
        [ class "card-header m-0 p-1 bg-primary text-center" ]
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


viewTable : String -> (c -> Html msg) -> c -> Html msg
viewTable title msg model =
    div [ class "m-0 w-100" ]
        [ div
            [ class "bg-dark w-100 m-0" ]
            [ p
                [ class "p-1 m-0 text-white" ]
                [ text title ]
            ]
        , msg model
        ]



-- HELPERS


neutroFieldToString : NeutroField -> String
neutroFieldToString neutroField =
    case neutroField of
        NeutroField Nothing neutro ->
            neutro

        NeutroField (Just _) neutro ->
            neutro


edgeDestinyToString : To -> String
edgeDestinyToString to =
    case to of
        To Nothing destiny ->
            destiny

        To (Just _) destiny ->
            destiny


edgeOriginToString : From -> String
edgeOriginToString from =
    case from of
        From Nothing origin ->
            origin

        From (Just _) origin ->
            origin
