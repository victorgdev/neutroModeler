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
    { nodes : List Node
    , edges : List NeutroEdge
    , form : Form
    }


type alias Entity =
    Force.Entity NodeId { value : CustomNode }


type alias CustomNode =
    { rank : Int, name : String }


type alias NodeId =
    Int


type alias Node =
    { id : NodeId
    , label : String
    , truth : Float
    , indeterminacy : Float
    , falsehood : Float
    }


type alias NeutroEdge =
    { id : Int
    , from : NodeId
    , to : NodeId
    , truth : Float
    , indeterminacy : Float
    , falsehood : Float
    }


type NeutroField
    = NeutroField (Maybe Float) String


type alias Form =
    { id : Int
    , label : String
    , truth : NeutroField
    , indeterminacy : NeutroField
    , falsehood : NeutroField
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


port setStorage : List Node -> Cmd msg


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
    { nodes =
        [ { id = 0
          , label = "A"
          , truth = 0.3
          , indeterminacy = 0.2
          , falsehood = 0.3
          }
        , { id = 0
          , label = "B"
          , truth = 0.3
          , indeterminacy = 0.2
          , falsehood = 0.3
          }
        , { id = 0
          , label = "C"
          , truth = 0.3
          , indeterminacy = 0.2
          , falsehood = 0.3
          }
        ]
    , edges =
        [ { id = 0
          , from = 0
          , to = 1
          , truth = 0.3
          , indeterminacy = 0.2
          , falsehood = 0.3
          }

        -- A->B
        , { id = 0
          , from = 0
          , to = 2
          , truth = 0.3
          , indeterminacy = 0.2
          , falsehood = 0.3
          }

        -- A->C
        , { id = 0
          , from = 1
          , to = 2
          , truth = 0.3
          , indeterminacy = 0.2
          , falsehood = 0.3
          }

        -- B->C
        ]
    , form = defaultForm
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
    | Add
    | Delete String
    | UpdateLabel String
    | UpdateTruth String
    | UpdateIndeterminacy String
    | UpdateFalsehood String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Add ->
            let
                newNode =
                    { id = model.form.id
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

        Delete label ->
            ( { model | nodes = List.filter (\n -> n.label /= label) model.nodes }
            , Cmd.none
            )

        UpdateLabel newLabel ->
            let
                oldForm =
                    model.form

                newForm =
                    { oldForm | label = newLabel }
            in
            ( { model | form = newForm }, Cmd.none )

        UpdateTruth newTruth ->
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

        UpdateIndeterminacy newIndeterminacy ->
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

        UpdateFalsehood newFalsehood ->
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



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "app" ]
        [ viewNavBar
        , div
            [ class "bar-scroll col-3 border-right border-info" ]
            [ viewNodeForm model.form
            , viewEdgeForm
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
        , div [ class "col-3 border-left border-info" ]
            [ h3 [ class "title m-3" ] [ text "Nodes:" ]
            , viewNodes model.nodes
            , div [ class "border-top" ]
                [ h3 [ class "title m-3" ] [ text "Edges:" ]
                , viewEdges model.edges
                ]
            ]
        ]


viewNavBar : Html Msg
viewNavBar =
    nav [ class "navbar navbar-expand-lg navbar-dark bg-dark w-100" ]
        [ a [ class "navbar-brand ml-3", href "#" ]
            [ img
                [ src "https://uploads-ssl.webflow.com/5e4e898f09bfea6abb6f44be/5e4ede46179566acc07948ca_complete.svg"
                , alt "Logo"
                ]
                []
            ]
        , button [ class "navbar-toggler", type_ "button" ]
            [ span [ class "navbar-toggler-icon" ]
                []
            ]
        , div [ class "collapse navbar-collapse", id "navbarNavAltMarkup" ]
            [ div [ class "float-right navbar-nav" ]
                [ a [ class "nav-item nav-link active", href "#" ]
                    [ text "Home" ]
                ]
            , a [ class "nav-item nav-link", href "#" ]
                [ text "Features" ]
            , a [ class "nav-item nav-link", href "#" ]
                [ text "Pricing" ]
            , a [ class "nav-item nav-link disabled", href "#" ]
                [ text "Disabled" ]
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
                        , onInput UpdateLabel
                        ]
                        []
                    , input
                        [ type_ "text"
                        , class "w-25 mr-2"
                        , placeholder "Tru"
                        , autofocus True
                        , value (neutroFieldToString node.truth)
                        , onInput UpdateTruth
                        ]
                        []
                    , input
                        [ type_ "text"
                        , class "w-25 mx-3"
                        , placeholder "Ind"
                        , autofocus True
                        , value (neutroFieldToString node.indeterminacy)
                        , onInput UpdateIndeterminacy
                        ]
                        []
                    , input
                        [ type_ "text"
                        , class "w-25 ml-2"
                        , placeholder "Fal"
                        , autofocus True
                        , value (neutroFieldToString node.falsehood)
                        , onInput UpdateFalsehood
                        ]
                        []
                    , button
                        [ class "btn btn-outline-primary w-100 my-2 my-3"
                        , type_ "submit"
                        , style "border-radius" "2rem"
                        , onClick Add
                        , disabled (checkFormIsEmpty node)
                        ]
                        [ text "Add Node" ]
                    ]
                ]
            ]
        ]


viewNodes : List Node -> Html Msg
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



--viewKeyedNode : Node -> ( String, Html Msg )
--viewKeyedNode node =
--    ( String.fromInt node.id, viewNode node )


viewNode : Node -> Html Msg
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
                , onClick (Delete node.label)
                ]
                [ text "X" ]
            ]
        ]


viewEdgeForm : Html Msg
viewEdgeForm =
    div [ class "col border-top" ]
        [ h3 [ class "title mt-3" ] [ text "Edge Input:" ]
        , div [ class "row" ]
            [ div
                [ class "dropdown m-3" ]
                [ h5 [ class "title" ] [ text "Origin" ]
                , button
                    [ type_ "button"
                    , class "btn btn-secondary dropdown-toggle"
                    , id "dropdownMenuOffset"
                    ]
                    [ span [ class "sr-only" ] [ text "Destiny" ] ]
                ]
            , div
                [ class "dropdown m-3" ]
                [ h5 [ class "title" ] [ text "Destiny" ]
                , button
                    [ type_ "button"
                    , class "btn btn-secondary dropdown-toggle"
                    , id "dropdownMenuOffset"
                    ]
                    [ span [ class "sr-only" ] [ text "Destiny" ] ]
                ]
            ]
        , input
            [ class "form-control mb-3"
            , placeholder "Truth"
            , autofocus True

            -- , value (neutroFieldToString node.truth)
            -- , onInput UpdateTruth
            ]
            []
        , input
            [ class "form-control mb-3"
            , placeholder "Indeterminacy"
            , autofocus True

            -- , value (neutroFieldToString node.indeterminacy)
            -- , onInput UpdateIndeterminacy
            ]
            []
        , input
            [ class "form-control mb-3"
            , placeholder "Falsehood"
            , autofocus True

            -- , value (neutroFieldToString node.falsehood)
            -- , onInput UpdateFalsehood
            ]
            []
        , button
            [ type_ "button"
            , class "btn btn-primary"

            -- , onClick Add
            , disabled True
            ]
            [ text "Add Edge" ]
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


viewKeyedEdge : NeutroEdge -> ( String, Html Msg )
viewKeyedEdge edge =
    ( String.fromInt edge.id, viewEdge edge )


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

                -- , onClick (Delete (String.FromInt edge.id))
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


checkFormIsEmpty : Form -> Bool
checkFormIsEmpty form =
    if form.label == "" then
        True

    else
        False
