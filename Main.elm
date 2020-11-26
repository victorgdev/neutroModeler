module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- MODEL
{- TODO

   types

   @Model,
   @GraphModel,
   @nodes,
   @edges,
   @neutroNumber,
-}
-- VIEW
{- Static views

   , @view

      , @viewLeftBar
         , @viewLeftBarTabMenu         - DONE

         , @viewNodeTab
            , @viewNodeForm            - DONE
            , nodeTable                - DONE

         , @viewEdgeTab
            , @viewEdgeForm            - DONE
               , 2x nodeDropdown       - DONE
            , edgeTable                - DONE

         , @viewSimulationTab
            , @viewSimulationForm      - DONE
               , nodeDropdown          - DONE
            , nodeTable                - DONE

      , @viewGraphResultsSection       - DONE
         , @viewGraphResultsMenu       - DONE

         , @viewGraphTab               - DONE

         , @viewResultsTab             - DONE
            , @viewModelKpisTable      - DONE
            , @viewFinalStateTable     - DONE
            , nodeTable                - DONE
            , edgeTable                - DONE

   SHARED VIEWS

   , @viewNodeDropdown  as nodeDropdown
   , @viewNodeTable     as nodeTable
   , @viewEdgeTable     as edgeTable
-}
-- TODO -> Install all the packages needed Force directed graph


app =
    div [ class "app" ]
        [ viewLeftBar
        , viewGraphResultsSection
        ]



-- LEFT BAR SECTION - INPUT
-- viewLeftBar : Model -> Html Msg


viewLeftBar =
    div [ class "left-bar" ]
        [ viewLeftBarMenu
        , viewNodeTab
        , viewEdgeTab
        , viewSimulationTab
        ]



-- viewLeftBarMenu : Model -> Html Msg


viewLeftBarMenu =
    div [ class "tab" ]
        [ button [ class "tablinks" ] [ text "Node" ]
        , button [ class "tablinks" ] [ text "Edge" ]
        , button [ class "tablinks" ] [ text "Simulation" ]
        ]



-- viewNodeTab : Msg -> Html Msg


viewNodeTab =
    div [ class "form" ]
        [ viewNodeForm
        , viewNodeTable
        ]



-- viewEdgeTab : Msg -> Html Msg


viewEdgeTab =
    div [ class "form" ]
        [ viewEdgeForm
        , viewEdgeTable
        ]



-- viewSimulationTab : Msg -> Html Msg


viewSimulationTab =
    div [ class "form" ]
        [ viewSimulationForm
        , viewNodeTable
        ]



-- GRAPH/RESULTS SECTION - OUTPUT


viewGraphResultsSection =
    div [ class "graph-results" ]
        [ viewGraphResultsMenu
        , viewGraphTab
        , viewResultsTab
        ]


viewGraphResultsMenu =
    div [ class "tab" ]
        [ button [ class "tablinks" ] [ text "Graph" ]
        , button [ class "tablinks" ] [ text "Results" ]
        ]



-- viewGraphTab : Model -> Svg Msg


viewGraphTab =
    --  svg [ viewBox 0 0 w h ]
    --      [ Graph.edges model.graph
    --          |> List.map (linkElement model.graph)
    --          |> g [ class [ "links" ] ]
    --      , Graph.nodes model.graph
    --          |> List.map nodeElement
    --          |> g [ class [ "nodes" ] ]
    --      ]
    div [ class "graph-results" ]
        -- only for testing positioning/layout
        []



-- viewResultsTab : Msg -> Html Msg


viewResultsTab =
    div [ class "graph-results" ]
        [ viewKpisTable
        , viewFinalStateTable
        , viewNodeTable
        , viewEdgeTable
        ]


viewKpisTable =
    table []
        [ tr []
            [ th []
                [ text "# Nodes" ]
            , th []
                [ text "# Edges" ]
            , th []
                [ text "# Transmitters" ]
            , th []
                [ text "# Receivers" ]
            , th []
                [ text "# Ordinary" ]
            , th []
                [ text "C/N Score" ]
            , th []
                [ text "Complexity" ]
            , th []
                [ text "Density" ]
            ]
        , tr []
            [ td []
                [ text "1" ]
            , td []
                [ text "1" ]
            , td []
                [ text "1" ]
            , td []
                [ text "1" ]
            , td []
                [ text "1" ]
            , td []
                [ text "1" ]
            , td []
                [ text "1" ]
            , td []
                [ text "1" ]
            ]
        ]


viewFinalStateTable =
    table []
        [ tr []
            [ th []
                [ text "Label" ]
            , th []
                [ text "Result" ]
            , th []
                [ text "Type" ]
            , th []
                [ text "Truth" ]
            , th []
                [ text "Indeterminacy" ]
            , th []
                [ text "Falsehood" ]
            ]
        , tr []
            [ td []
                [ text "Node 1" ]
            , td []
                [ text "Increased" ]
            , td []
                [ text "Receiver" ]
            , td []
                [ text "1" ]
            , td []
                [ text "1" ]
            , td []
                [ text "1" ]
            ]
        ]



-- VIEW HELPERS
-- viewNodeForm : Msg -> Html Msg


viewNodeForm =
    div [ class "form" ]
        [ h3 [ class "form-title" ] [ text "Input Nodes:" ]
        , input
            [ type_ "text"
            , id "label-input"
            , placeholder "Label"
            , autofocus True
            ]
            []
        , input
            [ type_ "text"
            , id "tru-input"
            , placeholder "Truth"
            , autofocus True
            ]
            []
        , input
            [ type_ "text"
            , id "ind-input"
            , placeholder "Indeterminacy"
            , autofocus True
            ]
            []
        , input
            [ type_ "text"
            , id "fal-input"
            , placeholder "Falsehood"
            , autofocus True
            ]
            []
        , button [ class "primary-button" ]
            [ text "Add Node" ]
        ]



-- viewEdgeForm : Msg -> Html Msg


viewEdgeForm =
    div [ class "form" ]
        [ h3 [ class "form-title" ] [ text "Input Edges:" ]
        , div [ id "origin" ]
            [ select [ id "node-dropdown" ]
                [ option [ value "n0" ]
                    [ text "node 0" ]
                , option [ value "n1" ]
                    [ text "node 1" ]
                , option [ value "n2" ]
                    [ text "node 2" ]
                , option [ value "n3" ]
                    [ text "node 3" ]
                ]
            ]
        , div [ id "destiny" ]
            [ select [ id "node-dropdown" ]
                [ option [ value "n0" ]
                    [ text "node 0" ]
                , option [ value "n1" ]
                    [ text "node 1" ]
                , option [ value "n2" ]
                    [ text "node 2" ]
                , option [ value "n3" ]
                    [ text "node 3" ]
                ]
            ]
        , input
            [ type_ "text"
            , id "tru-input"
            , placeholder "Truth"
            , autofocus True
            ]
            []
        , input
            [ type_ "text"
            , id "ind-input"
            , placeholder "Indeterminacy"
            , autofocus True
            ]
            []
        , input
            [ type_ "text"
            , id "fal-input"
            , placeholder "Falsehood"
            , autofocus True
            ]
            []
        , button [ class "primary-button" ]
            [ text "Add Edge" ]
        ]



-- viewSimulationForm : Msg -> Html Msg


viewSimulationForm =
    div [ class "form" ]
        [ h3 [ class "form-title" ] [ text "Input Edges:" ]
        , div [ id "simulation" ]
            [ select [ id "node-dropdown" ]
                [ option [ value "n0" ]
                    [ text "node 0" ]
                , option [ value "n1" ]
                    [ text "node 1" ]
                , option [ value "n2" ]
                    [ text "node 2" ]
                , option [ value "n3" ]
                    [ text "node 3" ]
                ]
            ]
        , input
            [ type_ "text"
            , id "tru-input"
            , placeholder "Truth"
            , autofocus True
            ]
            []
        , input
            [ type_ "text"
            , id "ind-input"
            , placeholder "Indeterminacy"
            , autofocus True
            ]
            []
        , input
            [ type_ "text"
            , id "fal-input"
            , placeholder "Falsehood"
            , autofocus True
            ]
            []
        , button [ class "primary-button" ]
            [ text "Add Node Simulation" ]
        , h3 [ class "form-title" ] [ text "Choose Target Node:" ]
        , div [ id "target-node" ]
            [ select [ id "node-dropdown" ]
                [ option [ value "n0" ]
                    [ text "node 0" ]
                , option [ value "n1" ]
                    [ text "node 1" ]
                , option [ value "n2" ]
                    [ text "node 2" ]
                , option [ value "n3" ]
                    [ text "node 3" ]
                ]
            ]
        , button [ class "primary-button" ]
            [ text "Simulate Model" ]
        ]


viewNodeTable =
    table []
        [ tr []
            [ th []
                [ text "Label" ]
            , th []
                [ text "Tru" ]
            , th []
                [ text "Ind" ]
            , th []
                [ text "Fal" ]
            ]
        , tr []
            [ td []
                [ text "Node 0" ]
            , td []
                [ text "1" ]
            , td []
                [ text "1" ]
            , td []
                [ text "1" ]
            ]
        ]


viewEdgeTable =
    table []
        [ tr []
            [ th []
                [ text "From" ]
            , th []
                [ text "To" ]
            , th []
                [ text "Tru" ]
            , th []
                [ text "Ind" ]
            , th []
                [ text "Fal" ]
            ]
        , tr []
            [ td []
                [ text "Node 0" ]
            , td []
                [ text "Node 1" ]
            , td []
                [ text "1" ]
            , td []
                [ text "1" ]
            , td []
                [ text "1" ]
            ]
        ]



-- viewNodeDropdown =
--     select [ id "node-dropdown" ]
--         [ option [ value "n0" ]
--             [ text "node 0" ]
--         , option [ value "n1" ]
--             [ text "node 1" ]
--         , option [ value "n2" ]
--             [ text "node 2" ]
--         , option [ value "n3" ]
--             [ text "node 3" ]
--         ]
-- FIX
-- main : Program Never Model Msg
-- main =
--     Html.program
--         { init = init
--         , view = view
--         , update = update
--         }


main =
    app



-- UPDATE
