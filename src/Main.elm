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


main : Program (Maybe (List Node)) Model Msg
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



-- MODEL


type alias Model =
    { nodes : List Node
    , form : Form
    }


type alias Node =
    { id : Int
    , label : String
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


emptyModel : Model
emptyModel =
    { nodes = []
    , form = defaultForm
    }


defaultForm : Form
defaultForm =
    { id = 0
    , label = ""
    , truth = NeutroField Nothing ""
    , indeterminacy = NeutroField Nothing ""
    , falsehood = NeutroField Nothing ""
    }


init : Maybe (List Node) -> ( Model, Cmd Msg )
init maybeNodes =
    ( { emptyModel | nodes = Maybe.withDefault [] maybeNodes }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | Add
    | Delete Int
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

        Delete id ->
            ( { model | nodes = List.filter (\n -> n.id /= id) model.nodes }
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
        [ div
            [ class "col-4" ]
            [ h3 [ class "title m-3" ] [ text "Node Input:" ]
            , viewInput model.form
            , button
                [ type_ "button"
                , class "btn btn-primary m-3"
                , onClick Add
                , disabled (checkFormIsEmpty model)
                ]
                [ text "Add Node" ]
            , viewNodes model.nodes
            ]
        , div [ class "col-8 bg-dark text-white" ] [ text "Canvas" ]
        ]


viewInput : Form -> Html Msg
viewInput node =
    div [ class "col" ]
        [ input
            [ class "form-control mb-3"
            , placeholder "Label"
            , autofocus True
            , value node.label
            , onInput UpdateLabel
            ]
            []
        , input
            [ class "form-control mb-3"
            , placeholder "Truth"
            , autofocus True
            , value (neutroFieldToString node.truth)
            , onInput UpdateTruth
            ]
            []
        , input
            [ class "form-control mb-3"
            , placeholder "Indeterminacy"
            , autofocus True
            , value (neutroFieldToString node.indeterminacy)
            , onInput UpdateIndeterminacy
            ]
            []
        , input
            [ class "form-control mb-3"
            , placeholder "Falsehood"
            , autofocus True
            , value (neutroFieldToString node.falsehood)
            , onInput UpdateFalsehood
            ]
            []
        ]


neutroFieldToString : NeutroField -> String
neutroFieldToString neutroField =
    case neutroField of
        NeutroField Nothing neutro ->
            neutro

        NeutroField (Just _) neutro ->
            neutro


viewNodes : List Node -> Html Msg
viewNodes nodes =
    table
        [ class "table" ]
        [ tr
            []
            [ th [ scope "col" ]
                [ text "Node" ]
            , th [ scope "col" ]
                [ text "Label" ]
            , th [ scope "col" ]
                [ text "Tru" ]
            , th [ scope "col" ]
                [ text "Ind" ]
            , th [ scope "col" ]
                [ text "Fal" ]
            , th [ scope "col" ]
                [ text "Remove" ]
            ]
        , Keyed.ul [] <| List.map viewKeyedNode nodes
        ]


viewKeyedNode : Node -> ( String, Html Msg )
viewKeyedNode node =
    ( String.fromInt node.id, viewNode node )


viewNode : Node -> Html Msg
viewNode node =
    tr []
        [ td []
            [ text (String.fromInt node.id) ]
        , td []
            [ text node.label ]
        , td []
            [ text (String.fromFloat node.truth) ]
        , td []
            [ text (String.fromFloat node.indeterminacy) ]
        , td []
            [ text (String.fromFloat node.falsehood) ]
        , td []
            [ button
                [ class "close"
                , type_ "button"
                , onClick (Delete node.id)
                ]
                [ text "x" ]
            ]
        ]



-- HANDLERS/HELPERS


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)


checkFormIsEmpty : Model -> Bool
checkFormIsEmpty model =
    if model.form.label == "" then
        True

    else
        False
