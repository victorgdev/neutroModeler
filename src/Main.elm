port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode as Json
import Task


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
    , tru : Float
    , ind : Float
    , fal : Float
    }


type NeutroField
    = NeutroField (Maybe Float) String


type alias Form =
    { id : Int
    , label : String
    , tru : NeutroField
    , ind : NeutroField
    , fal : NeutroField
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
    , tru = NeutroField Nothing ""
    , ind = NeutroField Nothing ""
    , fal = NeutroField Nothing ""
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
    | UpdateTru String
    | UpdateInd String
    | UpdateFal String


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
                    , tru = model.form.tru |> neutroFieldToString |> String.toFloat |> Maybe.withDefault 0.0
                    , ind = model.form.ind |> neutroFieldToString |> String.toFloat |> Maybe.withDefault 0.0
                    , fal = model.form.fal |> neutroFieldToString |> String.toFloat |> Maybe.withDefault 0.0
                    }

                newForm =
                    { defaultForm | id = model.form.id + 1 }
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

        UpdateTru newTruth ->
            let
                oldForm =
                    model.form

                newForm =
                    if String.right 1 newTruth == "." then
                        { oldForm | tru = NeutroField Nothing newTruth }

                    else
                        let
                            maybeTruth =
                                newTruth |> String.toFloat
                        in
                        case maybeTruth of
                            Nothing ->
                                { oldForm | tru = NeutroField Nothing newTruth }

                            Just p ->
                                { oldForm | tru = NeutroField (Just p) newTruth }
            in
            ( { model | form = newForm }, Cmd.none )

        UpdateInd newIndeterminacy ->
            let
                oldForm =
                    model.form

                newForm =
                    if String.right 1 newIndeterminacy == "." then
                        { oldForm | ind = NeutroField Nothing newIndeterminacy }

                    else
                        let
                            maybeIndeterminacy =
                                newIndeterminacy |> String.toFloat
                        in
                        case maybeIndeterminacy of
                            Nothing ->
                                { oldForm | ind = NeutroField Nothing newIndeterminacy }

                            Just p ->
                                { oldForm | ind = NeutroField (Just p) newIndeterminacy }
            in
            ( { model | form = newForm }, Cmd.none )

        UpdateFal newFalsehood ->
            let
                oldForm =
                    model.form

                newForm =
                    if String.right 1 newFalsehood == "." then
                        { oldForm | fal = NeutroField Nothing newFalsehood }

                    else
                        let
                            maybeFalsehood =
                                newFalsehood |> String.toFloat
                        in
                        case maybeFalsehood of
                            Nothing ->
                                { oldForm | fal = NeutroField Nothing newFalsehood }

                            Just p ->
                                { oldForm | fal = NeutroField (Just p) newFalsehood }
            in
            ( { model | form = newForm }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "container-fluid min-vh-100" ]
        [ div
            [ class "row" ]
            [ div
                [ class "col-4" ]
                [ h3 [ class "title" ] [ text "Node Input:" ]
                , viewInput model.form
                , button
                    [ type_ "button"
                    , class "btn btn-primary mb-3"
                    , onClick Add
                    , disabled (checkFormIsEmpty model)
                    ]
                    [ text "Add Node" ]
                , viewNodes model.nodes
                ]
            , div [ class "col-8 bg-dark text-white" ] [ text "Canvas" ]
            ]
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
            , value (neutroFieldToString node.tru)
            , onInput UpdateTru
            ]
            []
        , input
            [ class "form-control mb-3"
            , placeholder "Indeterminacy"
            , autofocus True
            , value (neutroFieldToString node.ind)
            , onInput UpdateInd
            ]
            []
        , input
            [ class "form-control mb-3"
            , placeholder "Falsehood"
            , autofocus True
            , value (neutroFieldToString node.fal)
            , onInput UpdateFal
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
                [ text "Node Id" ]
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
        [ td
            [ scope "row" ]
            [ text (String.fromInt node.id) ]
        , td []
            [ text node.label ]
        , td []
            [ text (String.fromFloat node.tru) ]
        , td []
            [ text (String.fromFloat node.ind) ]
        , td []
            [ text (String.fromFloat node.fal) ]
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
