module ViewTest

-- NAVBAR -->
viewNavBar : Html Msg
viewNavBar =
div [ class "row" ]
    [ nav [ class "navbar navbar-expand-lg navbar-dark bg-dark w-100" ]
        [ a [ class "navbar-brand ml-3", href "#" ]
            [ img [ src "
            https://uploads-ssl.webflow.com/5e4e898f09bfea6abb6f44be/5e4ede46179566acc07948ca_complete.svg", alt "Logo" ]
                []
            ]
        , button [ class "navbar-toggler", type_ "button", data-toggle "collapse", data-target "#navbarNavAltMarkup", aria-controls "navbarNavAltMarkup", aria-expanded "false", aria-label "Toggle navigation" ]
            [ span [ class "navbar-toggler-icon" ]
                []
            ]
        , div [ class "collapse navbar-collapse", id "navbarNavAltMarkup" ]
            [ div [ class "float-right navbar-nav" ]
                [ a [ class "nav-item nav-link active", href "#" ]
                    [ text "Home"  span [ class "sr-only" ]
                        [ text "(current)" ]
                    ]
                , a [ class "nav-item nav-link", href "#" ]
                    [ text "Features" ]
                , a [ class "nav-item nav-link", href "#" ]
                    [ text "Pricing" ]
                , a [ class "nav-item nav-link disabled", href "#", tabindex "-1", aria-disabled "true" ]
                    [ text "Disabled" ]
                ]
            ]
        ]
    ]
    <!-- END OF NAVBAR -->

    <div class="row pt-3">

      <!-- MODEL INPUT -->
      <div class="col-3">

-- NODES -->
viewNodeInputFromTest : Html Msg
viewNodeInputFromTest =
        div [ class "accordion", id "accordionExample" ]
    [ div [ class "shadow card m-2 w-100 border-0", style [ ( "border-radius", "2rem" ), ( "max-width", "260px" ) ] ]
        [ div [ class "card-header bg-primary", id "headingOne", style [ ( "display", "flex" ) ] ]
            [ h4 [ class "ml-4 pt-2 text-white" ]
                [ text "Node" ]
            , button [ class "btn ml-5 p-0", type_ "button", data-toggle "collapse", data-target "#collapseOne", aria-expanded "true", aria-controls "collapseOne" ]
                [ h1 [ class "m-0 ml-1 text-white" ]
                    [ text "+" ]
                ]
            ]
        , div [ id "collapseOne", class "collapse show", aria-labelledby "headingOne", data-parent "#accordionExample" ]
            [ div [ class "card-body p-3" ]
                [ input [ type_ "text", class "my-3 w-100", placeholder "Label" ]
                    []
                , input [ type_ "text", class "w-25 mr-2", placeholder "Tru" ]
                    []
                , input [ type_ "text", class "w-25 mx-3", placeholder "Ind" ]
                    []
                , input [ type_ "text", class "w-25 ml-2", placeholder "Fal" ]
                    []
                , button [ class "btn btn-outline-primary w-100 my-2 my-3", type_ "submit", style [ ( "border-radius", "2rem" ) ] ]
                    [ text "Add
                  Node" ]
                ]
            ]
        ]
    ]

-- END OF NODES

-- EDGES -->
viewEdgeInputFromTest : Html Msg
viewEdgeInputFromTest =
    div [ class "accordion my-3", id "accordionExample" ]
        [ div [ class "shadow card m-2 w-100 border-0", style [ ( "border-radius", "2rem" ), ( "max-width", "260px" ) ] ]
            [ div [ class "card-header bg-primary", id "headingTwo", style [ ( "display", "flex" ) ] ]
                [ h4 [ class "ml-4 pt-2 text-white" ]
                    [ text "Edge" ]
                , button [ class "btn ml-5 p-0", type_ "button", data-toggle "collapse", data-target "#collapseTwo", aria-expanded "true", aria-controls "collapseTwo" ]
                    [ h1 [ class "m-0 ml-1 text-white" ]
                        [ text "+" ]
                    ]
                ]
            , div [ id "collapseTwo", class "collapse show", aria-labelledby "headingTwo", data-parent "#accordionExample" ]
                [ div [ class "card-body p-3" ]
                    [ div [ class "row px-3" ]
                        [ div [ class "dropdown mx-3 mb-3" ]
                            [ button [ class "btn btn-secondary dropdown-toggle", style [ ( "border-radius", "2rem" ), ( "min-width", "5rem" ) ], type_ "button", id "dropdownMenuButton", data-toggle "dropdown", aria-haspopup "true", aria-expanded "false" ]
                                [ text "From" ]
                            , div [ class "dropdown-menu", aria-labelledby "dropdownMenuButton", style [ ( "border-radius", "1rem" ) ] ]
                                [ a [ class "dropdown-item", href "#" ]
                                    [ text "N0" ]
                                , a [ class "dropdown-item", href "#" ]
                                    [ text "N1" ]
                                , a [ class "dropdown-item", href "#" ]
                                    [ text "N3" ]
                                ]
                            ]
                        , div [ class "dropdown mx-3 mb-3" ]
                            [ button [ class "btn btn-secondary dropdown-toggle", style [ ( "border-radius", "2rem" ), ( "min-width", "5rem" ) ], type_ "button", id "dropdownMenuButton", data-toggle "dropdown", aria-haspopup "true", aria-expanded "false" ]
                                [ text "To" ]
                            , div [ class "dropdown-menu", aria-labelledby "dropdownMenuButton", style [ ( "border-radius", "1rem" ) ] ]
                                [ a [ class "dropdown-item", href "#" ]
                                    [ text "N0" ]
                                , a [ class "dropdown-item", href "#" ]
                                    [ text "N1" ]
                                , a [ class "dropdown-item", href "#" ]
                                    [ text "N3" ]
                                ]
                            ]
                        ]
                    , input [ type_ "text", class "w-25 mr-2", placeholder "Tru" ]
                        []
                    , input [ type_ "text", class "w-25 mx-3", placeholder "Ind" ]
                        []
                    , input [ type_ "text", class "w-25 ml-2", placeholder "Fal" ]
                        []
                    , button [ class "btn btn-outline-primary w-100 my-2 my-3", type_ "submit", style [ ( "border-radius", "2rem" ) ] ]
                        [ text "Add
                    Edge" ]
                    ]
                ]
            ]
        ]        

-- END OF EDGES -->

-- SIMULATION -->
viewSimulationInputFromTest : Html Msg
viewSimulationInputFromTest =
    div [ class "accordion my-3", id "accordionExample" ]
        [ div [ class "shadow card m-2 border-0", style [ ( "border-radius", "2rem" ), ( "max-width", "260px" ) ] ]
            [ div [ class "card-header bg-primary", id "headingThree", style [ ( "display", "flex" ) ] ]
                [ h4 [ class "ml-4 pt-2 text-white" ]
                    [ text "Simulate" ]
                , button [ class "btn ml-3 p-0", type_ "button", data-toggle "collapse", data-target "#collapseThree", aria-expanded "true", aria-controls "collapseThree" ]
                    [ h1 [ class "m-0 text-white" ]
                        [ text "+" ]
                    ]
                ]
            , div [ id "collapseThree", class "collapse show", aria-labelledby "headingThree", data-parent "#accordionExample" ]
                [ div [ class "card-body p-3" ]
                    [ div [ class "dropdown mb-3" ]
                        [ button [ class "btn btn-secondary dropdown-toggle", type_ "button", id "dropdownMenuButton", data-toggle "dropdown", aria-haspopup "true", aria-expanded "false", style [ ( "border-radius", "2rem" ) ] ]
                            [ text "Node" ]
                        , div [ class "dropdown-menu", aria-labelledby "dropdownMenuButton", style [ ( "border-radius", "1rem" ) ] ]
                            [ a [ class "dropdown-item", href "#" ]
                                [ text "N0" ]
                            , a [ class "dropdown-item", href "#" ]
                                [ text "N1" ]
                            , a [ class "dropdown-item", href "#" ]
                                [ text "N3" ]
                            ]
                        ]
                    , input [ type_ "text", class "w-25 mr-2", placeholder "Tru" ]
                        []
                    , input [ type_ "text", class "w-25 mx-3", placeholder "Ind" ]
                        []
                    , input [ type_ "text", class "w-25 ml-2", placeholder "Fal" ]
                        []
                    , button [ class "btn btn-outline-primary w-100 my-2 my-3", type_ "submit", style [ ( "border-radius", "2rem" ) ] ]
                        [ text "Add
                    Simulation" ]
                    ]
                ]
            ]
        ]        
-- END OF SIMULATION -->

-- TARGET -->
viewTargetInputFromTest : Html Msg
viewTargetInputFromTest =
    div [ class "accordion my-3", id "accordionExample" ]
        [ div [ class "shadow card m-2 border-0", style [ ( "border-radius", "2rem" ), ( "max-width", "260px" ) ] ]
            [ div [ class "card-header bg-primary", id "headingFour", style [ ( "display", "flex" ) ] ]
                [ h4 [ class "ml-3 pt-2 text-white" ]
                    [ text "Target" ]
                , button [ class "btn ml-5 p-0", type_ "button", data-toggle "collapse", data-target "#collapseFour", aria-expanded "true", aria-controls "collapseFour" ]
                    [ h1 [ class "m-0 ml-1 text-white" ]
                        [ text "+" ]
                    ]
                ]
            , div [ id "collapseFour", class "collapse show", aria-labelledby "headingFour", data-parent "#accordionExample" ]
                [ div [ class "card-body p-3" ]
                    [ div [ class "dropdown mb-3" ]
                        [ button [ class "btn btn-secondary dropdown-toggle", type_ "button", id "dropdownMenuButton", data-toggle "dropdown", aria-haspopup "true", aria-expanded "false", style [ ( "border-radius", "2rem" ) ] ]
                            [ text "Node" ]
                        , div [ class "dropdown-menu", aria-labelledby "dropdownMenuButton", style [ ( "border-radius", "1rem" ) ] ]
                            [ a [ class "dropdown-item", href "#" ]
                                [ text "N0" ]
                            , a [ class "dropdown-item", href "#" ]
                                [ text "N1" ]
                            , a [ class "dropdown-item", href "#" ]
                                [ text "N3" ]
                            ]
                        ]
                    , button [ class "btn btn-outline-primary w-100 my-2 my-3", type_ "submit", style [ ( "border-radius", "2rem" ) ] ]
                        [ text "Add
                    Target" ]
                    ]
                ]
            ]
        ]     

-- END OF TARGET

viewRunDeleteBtnFromtest : Html Msg
viewRunDeleteBtnFromtest =
    div [ class "row m-3", style [ ( "width", "100%" ) ] ]
        [ button [ class "shadow btn btn-outline-success m-3", style [ ( "min-width", "6rem" ) ] ]
            [ text "Run" ]
        , button [ class "shadow btn btn-outline-danger m-3", style [ ( "min-width", "6rem" ) ] ]
            [ text "Delete" ]
        ]

viewGraphFromTest : Html Msg
viewGraphFromTest =
    div [ class "col w-75 shadow", style [ ( "max-height", "100vh" ) ] ]
        [ text "Canvas" ]