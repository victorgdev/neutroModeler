

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