port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Color exposing (Color)
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeId)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import IntDict
import Json.Decode as Json
import List exposing (range)
import Scale exposing (SequentialScale)
import Scale.Color
import Task
import TypedSvg exposing (circle, g, line, polygon, svg, title)
import TypedSvg.Attributes exposing (fill, points, stroke, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..))



-- MODEL


type alias Model =
    { nodes : List NeutroNode
    , edges : List NeutroEdge
    , simulatedNodes : List SimulatedNode
    , targetNodes : List TargetNode
    , form : Form
    , edgeForm : EdgeForm
    , simulationForm : SimulationForm
    , targetNodeForm : TargetNodeForm
    }


type alias Entity =
    Force.Entity NodeId { value : CustomNode }


type alias CustomNode =
    { rank : Int, name : String }


type alias NodeId =
    Int


type alias NeutroNode =
    { id : NodeId
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


type From
    = From (Maybe Int) String


type To
    = To (Maybe Int) String


type NeutroField
    = NeutroField (Maybe Float) String


type alias Form =
    { id : Int
    , label : String
    , truth : NeutroField
    , indeterminacy : NeutroField
    , falsehood : NeutroField
    }


type alias EdgeForm =
    { edgeId : Int
    , from : From
    , to : To
    , truth : NeutroField
    , indeterminacy : NeutroField
    , falsehood : NeutroField
    }


type alias SimulationForm =
    { simNodeId : Int
    , simNodeLabel : String
    , simNodeTruth : NeutroField
    , simNodeIndeterminacy : NeutroField
    , simNodeFalsehood : NeutroField
    }


type alias TargetNodeForm =
    { targetNodeId : Int
    , targetNodeLabel : String
    , targetNodeTruth : NeutroField
    , targetNodeIndeterminacy : NeutroField
    , targetNodeFalsehood : NeutroField
    }


w : Float
w =
    990


h : Float
h =
    504


colorScale : SequentialScale Color
colorScale =
    Scale.sequential Scale.Color.viridisInterpolator ( 200, 700 )


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


defaultForm : Form
defaultForm =
    { id = 0
    , label = ""
    , truth = NeutroField Nothing ""
    , indeterminacy = NeutroField Nothing ""
    , falsehood = NeutroField Nothing ""
    }


defaultEdgeForm : EdgeForm
defaultEdgeForm =
    { edgeId = 0
    , from = From Nothing ""
    , to = To Nothing ""
    , truth = NeutroField Nothing ""
    , indeterminacy = NeutroField Nothing ""
    , falsehood = NeutroField Nothing ""
    }


defaultSimulationForm : SimulationForm
defaultSimulationForm =
    { simNodeId = 0
    , simNodeLabel = ""
    , simNodeTruth = NeutroField Nothing ""
    , simNodeIndeterminacy = NeutroField Nothing ""
    , simNodeFalsehood = NeutroField Nothing ""
    }


defaultTargetNodeForm : TargetNodeForm
defaultTargetNodeForm =
    { targetNodeId = 0
    , targetNodeLabel = ""
    , targetNodeTruth = NeutroField Nothing ""
    , targetNodeIndeterminacy = NeutroField Nothing ""
    , targetNodeFalsehood = NeutroField Nothing ""
    }


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
                        , distance = 100
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


initModel : Model
initModel =
    { nodes = []
    , edges = []
    , simulatedNodes = []
    , targetNodes = []
    , form = defaultForm
    , edgeForm = defaultEdgeForm
    , simulationForm = defaultSimulationForm
    , targetNodeForm = defaultTargetNodeForm
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Cmd.none
    )


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


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
        nodeSize 4 node.label

    else if node.label.value.rank < 9 then
        nodeSize 7 node.label

    else if modBy 2 node.label.value.rank == 0 then
        g []
            [ nodeSize 9 node.label
            , circle
                [ r 12
                , cx node.label.x
                , cy node.label.y
                , fill PaintNone
                , stroke <| Paint <| Scale.convert colorScale node.label.x
                ]
                []
            ]

    else
        nodeSize 10 node.label



-- UPDATE


type Msg
    = NoOp
    | AddNode
    | AddEdge
    | AddSimNode
    | AddTargetNode
      --
    | DeleteNode Int
    | DeleteEdge Int
    | DeleteSimNode Int
    | DeleteTargetNode Int
      --
    | UpdateNodeLabel String
    | UpdateNodeTruth String
    | UpdateNodeIndeterminacy String
    | UpdateNodeFalsehood String
      --
    | UpdateEdgeFrom String
    | UpdateEdgeTo String
    | UpdateEdgeTruth String
    | UpdateEdgeIndeterminacy String
    | UpdateEdgeFalsehood String
      --
    | UpdateSimNodeLabel String
    | UpdateSimNodeTruth String
    | UpdateSimNodeIndeterminacy String
    | UpdateSimNodeFalsehood String
      --
    | UpdateTargetNodeLabel String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddNode ->
            let
                newNode =
                    { id = model.form.id + 1
                    , label = model.form.label
                    , truth = model.form.truth |> neutroFieldToString |> String.toFloat |> Maybe.withDefault 0.0
                    , indeterminacy = model.form.indeterminacy |> neutroFieldToString |> String.toFloat |> Maybe.withDefault 0.0
                    , falsehood = model.form.falsehood |> neutroFieldToString |> String.toFloat |> Maybe.withDefault 0.0
                    }

                newForm =
                    { defaultForm | id = 0 }
            in
            ( { model
                | form = newForm
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
                    { defaultEdgeForm | edgeId = 0 }
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
                    { defaultSimulationForm | simNodeId = 0 }
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
                    { defaultTargetNodeForm | targetNodeId = 0 }
            in
            ( { model
                | targetNodeForm = newTargetNodeForm
                , targetNodes =
                    model.targetNodes ++ [ newTargetNode ]
              }
            , Cmd.none
            )

        DeleteNode nodeId ->
            ( { model | nodes = List.filter (\n -> n.id /= nodeId) model.nodes }
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
                    model.form

                newForm =
                    { oldForm | label = newLabel }
            in
            ( { model | form = newForm }, Cmd.none )

        UpdateNodeTruth newTruth ->
            let
                oldForm =
                    model.form

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
            ( { model | form = newForm }, Cmd.none )

        UpdateNodeIndeterminacy newIndeterminacy ->
            let
                oldForm =
                    model.form

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
            ( { model | form = newForm }, Cmd.none )

        UpdateNodeFalsehood newFalsehood ->
            let
                oldForm =
                    model.form

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
            ( { model | form = newForm }, Cmd.none )

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



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "app" ]
        [ div
            [ class "bar-scroll col-3 border-right border-info" ]
            [ viewNodeForm model.form
            , viewEdgeForm model.edgeForm
            , viewSimulationForm model.simulationForm
            , viewTargetNodeForm model.targetNodeForm
            ]
        , div [ class "col-6 bg-dark text-white" ]
            [ text "Canvas"
            , svg [ viewBox 0 0 w h ]
                [ g [ TypedSvg.Attributes.class [ "links" ] ] <|
                    List.map (linkElement (initGraph model)) <|
                        Graph.edges (initGraph model)
                , g [ TypedSvg.Attributes.class [ "nodes" ] ] <|
                    List.map nodeElement <|
                        Graph.nodes (initGraph model)
                ]
            ]
        , div [ class "bar-scroll col-3 border-right border-info" ]
            [ h3 [ class "title m-3" ] [ text "Nodes:" ]
            , viewNodes model.nodes
            , div [ class "border-top" ]
                [ h3 [ class "title m-3" ] [ text "Edges:" ]
                , viewEdges model.edges
                ]
            , div [ class "border-top" ]
                [ h3 [ class "title m-3" ] [ text "Simulated Nodes:" ]
                , viewSimulatedNodes model.simulatedNodes
                ]
            , div [ class "border-top" ]
                [ h3 [ class "title m-3" ] [ text "Target Nodes:" ]
                , viewTargetNodes model.targetNodes
                ]
            ]
        ]


viewNodeForm : Form -> Html Msg
viewNodeForm node =
    div
        [ class "accordion"
        , id "accordionExample"
        ]
        [ div
            [ class "shadow card m-2 w-100 border-0"
            , style "border-radius" "2rem"
            , style "max-width" "260px"
            ]
            [ div
                [ class "card-header bg-primary"
                , id "headingOne"
                , style "display" "flex"
                ]
                [ h4 [ class "ml-4 pt-2 text-white" ]
                    [ text "Node" ]
                , button [ class "btn ml-5 p-0", type_ "button" ]
                    [ h1 [ class "m-0 ml-1 text-white" ]
                        [ text "+" ]
                    ]
                ]
            , div [ id "collapseOne", class "collapse show" ]
                [ div [ class "card-body p-3" ]
                    [ input
                        [ type_ "text"
                        , class "my-3 w-100"
                        , placeholder "Label"
                        , autofocus True
                        , value node.label
                        , onInput UpdateNodeLabel
                        ]
                        []
                    , input
                        [ type_ "text"
                        , class "w-25 mr-2"
                        , placeholder "Tru"
                        , autofocus True
                        , value (neutroFieldToString node.truth)
                        , onInput UpdateNodeTruth
                        ]
                        []
                    , input
                        [ type_ "text"
                        , class "w-25 mx-3"
                        , placeholder "Ind"
                        , autofocus True
                        , value (neutroFieldToString node.indeterminacy)
                        , onInput UpdateNodeIndeterminacy
                        ]
                        []
                    , input
                        [ type_ "text"
                        , class "w-25 ml-2"
                        , placeholder "Fal"
                        , autofocus True
                        , value (neutroFieldToString node.falsehood)
                        , onInput UpdateNodeFalsehood
                        ]
                        []
                    , button
                        [ class "btn btn-outline-primary w-100 my-2 my-3"
                        , type_ "submit"
                        , style "border-radius" "2rem"
                        , onClick AddNode
                        , disabled (checkFormIsEmpty node)
                        ]
                        [ text "Add Node" ]
                    ]
                ]
            ]
        ]


viewNodes : List NeutroNode -> Html Msg
viewNodes nodes =
    table
        [ class "table m-3" ]
        (tr
            []
            [ th [ class "text-left", scope "col" ] [ text "Label" ]
            , th [ class "text-left", scope "col" ] [ text "Tru" ]
            , th [ class "text-left", scope "col" ] [ text "Ind" ]
            , th [ class "text-left", scope "col" ] [ text "Fal" ]
            , th [ class "text-left", scope "col" ] [ text "Remove" ]
            ]
            :: List.map viewNode nodes
        )


viewNode : NeutroNode -> Html Msg
viewNode node =
    tr []
        [ td [ class "text-left" ] [ text node.label ]
        , td [ class "text-left" ] [ text (String.fromFloat node.truth) ]
        , td [ class "text-left" ] [ text (String.fromFloat node.indeterminacy) ]
        , td [ class "text-left" ] [ text (String.fromFloat node.falsehood) ]
        , td [ class "text-left" ]
            [ button
                [ class "btn btn text-left"
                , type_ "button"
                , onClick (DeleteNode node.id)
                ]
                [ text "X" ]
            ]
        ]


viewEdgeForm : EdgeForm -> Html Msg
viewEdgeForm edge =
    div
        [ class "accordion my-3"
        , id "accordionExample"
        ]
        [ div
            [ class "shadow card m-2 w-100 border-0"
            , style "border-radius" "2rem"
            , style "max-width" "260px"
            ]
            [ div
                [ class "card-header bg-primary"
                , id "headingTwo"
                , style "display" "flex"
                ]
                [ h4
                    [ class "ml-4 pt-2 text-white" ]
                    [ text "Edge" ]
                , button
                    [ class "btn ml-5 p-0"
                    , type_ "button"
                    ]
                    [ h1
                        [ class "m-0 ml-1 text-white" ]
                        [ text "+" ]
                    ]
                ]
            , div
                []
                [ input
                    [ type_ "text"
                    , class "my-2 w-50"
                    , placeholder "From"
                    , autofocus True
                    , value (edgeOriginToString edge.from)
                    , onInput UpdateEdgeFrom
                    ]
                    []
                , input
                    [ type_ "text"
                    , class "my-2 w-50"
                    , placeholder "To"
                    , autofocus True
                    , value (edgeDestinyToString edge.to)
                    , onInput UpdateEdgeTo
                    ]
                    []
                , input
                    [ type_ "text"
                    , class "w-25 mr-2"
                    , placeholder "Tru"
                    , autofocus True
                    , value (neutroFieldToString edge.truth)
                    , onInput UpdateEdgeTruth
                    ]
                    []
                , input
                    [ type_ "text"
                    , class "w-25 mx-3"
                    , placeholder "Ind"
                    , autofocus True
                    , value (neutroFieldToString edge.indeterminacy)
                    , onInput UpdateEdgeIndeterminacy
                    ]
                    []
                , input
                    [ type_ "text"
                    , class "w-25 ml-2"
                    , placeholder "Fal"
                    , autofocus True
                    , value (neutroFieldToString edge.falsehood)
                    , onInput UpdateEdgeFalsehood
                    ]
                    []
                , button
                    [ class "btn btn-outline-primary w-100 my-2 my-3"
                    , type_ "submit"
                    , style "border-radius" "2rem"

                    --, disabled True
                    , onClick AddEdge
                    ]
                    [ text "Add Edge" ]
                ]
            ]
        ]


viewEdges : List NeutroEdge -> Html Msg
viewEdges edges =
    table
        [ class "table m-3" ]
        (tr
            []
            [ th [ class "text-left", scope "col" ] [ text "From" ]
            , th [ class "text-left", scope "col" ] [ text "To" ]
            , th [ class "text-left", scope "col" ] [ text "Tru" ]
            , th [ class "text-left", scope "col" ] [ text "Ind" ]
            , th [ class "text-left", scope "col" ] [ text "Fal" ]
            , th [ class "text-left", scope "col" ] [ text "Remove" ]
            ]
            :: List.map viewEdge edges
        )


viewEdge : NeutroEdge -> Html Msg
viewEdge edge =
    tr []
        [ td [ class "text-left" ] [ text (String.fromInt edge.from) ]
        , td [ class "text-left" ] [ text (String.fromInt edge.to) ]
        , td [ class "text-left" ] [ text (String.fromFloat edge.truth) ]
        , td [ class "text-left" ] [ text (String.fromFloat edge.indeterminacy) ]
        , td [ class "text-left" ] [ text (String.fromFloat edge.falsehood) ]
        , td [ class "text-left" ]
            [ button
                [ class "btn btn text-left"
                , type_ "button"
                , onClick (DeleteEdge edge.edgeId)
                ]
                [ text "X" ]
            ]
        ]


viewSimulationForm : SimulationForm -> Html Msg
viewSimulationForm simulatedNode =
    div
        [ class "accordion my-3"
        , id "accordionExample"
        ]
        [ div
            [ class "shadow card m-2 border-0"
            , style "border-radius" "2rem"
            , style "max-width" "260px"
            ]
            [ div
                [ class "card-header bg-primary"
                , id "headingThree"
                , style "display" "flex"
                ]
                [ h4
                    [ class "ml-4 pt-2 text-white" ]
                    [ text "Simulate" ]
                , button
                    [ class "btn ml-3 p-0"
                    , type_ "button"
                    ]
                    [ h1
                        [ class "m-0 text-white" ]
                        [ text "+" ]
                    ]
                ]
            , div
                [ id "collapseThree"
                , class "collapse show"
                ]
                [ div
                    [ class "card-body p-3" ]
                    [ input
                        [ type_ "text"
                        , class "my-3 w-100"
                        , placeholder "Label"
                        , value simulatedNode.simNodeLabel
                        , onInput UpdateSimNodeLabel
                        ]
                        []
                    , input
                        [ type_ "text"
                        , class "w-25 mr-2"
                        , placeholder "Tru"
                        , value (neutroFieldToString simulatedNode.simNodeTruth)
                        , onInput UpdateSimNodeTruth
                        ]
                        []
                    , input
                        [ type_ "text"
                        , class "w-25 mx-3"
                        , placeholder "Ind"
                        , value (neutroFieldToString simulatedNode.simNodeIndeterminacy)
                        , onInput UpdateSimNodeIndeterminacy
                        ]
                        []
                    , input
                        [ type_ "text"
                        , class "w-25 ml-2"
                        , placeholder "Fal"
                        , value (neutroFieldToString simulatedNode.simNodeFalsehood)
                        , onInput UpdateSimNodeFalsehood
                        ]
                        []
                    , button
                        [ class "btn btn-outline-primary w-100 my-2 my-3"
                        , type_ "submit"
                        , style "border-radius" "2rem"
                        , onClick AddSimNode
                        ]
                        [ text "Add Simulation" ]
                    ]
                ]
            ]
        ]


viewSimulatedNodes : List SimulatedNode -> Html Msg
viewSimulatedNodes simulatedNodes =
    table
        [ class "table m-3" ]
        (tr
            []
            [ th [ class "text-left", scope "col" ] [ text "Label" ]
            , th [ class "text-left", scope "col" ] [ text "Tru" ]
            , th [ class "text-left", scope "col" ] [ text "Ind" ]
            , th [ class "text-left", scope "col" ] [ text "Fal" ]
            , th [ class "text-left", scope "col" ] [ text "Remove" ]
            ]
            :: List.map viewSimulatedNode simulatedNodes
        )


viewSimulatedNode : SimulatedNode -> Html Msg
viewSimulatedNode simulatedNode =
    tr []
        [ td [ class "text-left" ] [ text simulatedNode.simNodeLabel ]
        , td [ class "text-left" ] [ text (String.fromFloat simulatedNode.simNodeTruth) ]
        , td [ class "text-left" ] [ text (String.fromFloat simulatedNode.simNodeIndeterminacy) ]
        , td [ class "text-left" ] [ text (String.fromFloat simulatedNode.simNodeFalsehood) ]
        , td [ class "text-left" ]
            [ button
                [ class "btn btn text-left"
                , type_ "button"
                , onClick (DeleteSimNode simulatedNode.simNodeId)
                ]
                [ text "X" ]
            ]
        ]


viewTargetNodeForm : TargetNodeForm -> Html Msg
viewTargetNodeForm targetNode =
    div
        [ class "accordion my-3"
        , id "accordionExample"
        ]
        [ div
            [ class "shadow card m-2 border-0"
            , style "border-radius" "2rem"
            , style "max-width" "260px"
            ]
            [ div
                [ class "card-header bg-primary"
                , id "headingThree"
                , style "display" "flex"
                ]
                [ h4
                    [ class "ml-4 pt-2 text-white" ]
                    [ text "Target" ]
                , button
                    [ class "btn ml-3 p-0"
                    , type_ "button"
                    ]
                    [ h1
                        [ class "m-0 text-white" ]
                        [ text "+" ]
                    ]
                ]
            , div
                [ id "collapseThree"
                , class "collapse show"
                ]
                [ div
                    [ class "card-body p-3" ]
                    [ input
                        [ type_ "text"
                        , class "my-3 w-100"
                        , placeholder "Label"
                        , value targetNode.targetNodeLabel
                        , onInput UpdateTargetNodeLabel
                        ]
                        []
                    , button
                        [ class "btn btn-outline-primary w-100 my-2 my-3"
                        , type_ "submit"
                        , style "border-radius" "2rem"
                        , onClick AddTargetNode
                        ]
                        [ text "Add Simulation" ]
                    ]
                ]
            ]
        ]


viewTargetNodes : List TargetNode -> Html Msg
viewTargetNodes targetNodes =
    table
        [ class "table m-3" ]
        (tr
            []
            [ th [ class "text-left", scope "col" ] [ text "Label" ]
            , th [ class "text-left", scope "col" ] [ text "Remove" ]
            ]
            :: List.map viewTargetNode targetNodes
        )


viewTargetNode : TargetNode -> Html Msg
viewTargetNode targetNode =
    tr []
        [ td [ class "text-left" ] [ text targetNode.targetNodeLabel ]
        , td [ class "text-left" ]
            [ button
                [ class "btn btn text-left"
                , type_ "button"
                , onClick (DeleteTargetNode targetNode.targetNodeId)
                ]
                [ text "X" ]
            ]
        ]


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



-- HANDLERS/HELPERS


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


checkFormIsEmpty : Form -> Bool
checkFormIsEmpty form =
    if form.label == "" then
        True

    else
        False
