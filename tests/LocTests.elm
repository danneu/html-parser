module LocTests exposing (..)

import Expect exposing (Expectation)
import Html.Loc as Q exposing (Loc, Selector)
import Html.Parser exposing (Node(..))
import Test exposing (..)


config =
    Html.Parser.allCharRefs


type alias DirCase =
    { name : String
    , html : String
    , dirs : List Dir
    , expect : Maybe Node
    }


testTraversals : List DirCase -> List Test
testTraversals cases =
    List.map
        (\x ->
            test (x.name ++ " \"" ++ x.html ++ "\" " ++ Debug.toString x.dirs)
                (\_ ->
                    let
                        applyDirs : List Dir -> Maybe Loc -> Maybe Loc
                        applyDirs dirs maybeLoc =
                            maybeLoc
                                |> Maybe.andThen
                                    (\loc ->
                                        case dirs of
                                            [] ->
                                                maybeLoc

                                            dir :: rest ->
                                                let
                                                    fn =
                                                        case dir of
                                                            Up ->
                                                                Q.goUp

                                                            Down ->
                                                                Q.goDown

                                                            Left ->
                                                                Q.goLeft

                                                            Right ->
                                                                Q.goRight

                                                            Next ->
                                                                Q.goNext
                                                in
                                                applyDirs rest (fn loc)
                                    )

                        actual =
                            Html.Parser.run config x.html
                                |> Result.toMaybe
                                |> Maybe.andThen List.head
                                |> Maybe.map Q.toLoc
                                |> applyDirs x.dirs
                                |> Maybe.map Q.toNode
                    in
                    Expect.equal actual x.expect
                )
        )
        cases


type Dir
    = Up
    | Right
    | Down
    | Left
    | Next


zipperTests =
    describe "traversal" <|
        testTraversals
            [ DirCase "single-node-1" "<a/>" [] (Just (Element "a" [] []))
            , DirCase "single-node-2" "<a/>" [ Right ] Nothing
            , DirCase "single-node-3" "<a/>" [ Up ] Nothing
            , DirCase "single-node-4" "<a/>" [ Left ] Nothing
            , DirCase "single-node-5" "<a/>" [ Down ] Nothing
            , DirCase "ab1" "<a><b>c</b></a>" [] (Just (Element "a" [] [ Element "b" [] [ Text "c" ] ]))
            , DirCase "ab2" "<a><b>c</b></a>" [ Down ] (Just (Element "b" [] [ Text "c" ]))
            , DirCase "ab3" "<a><b>c</b></a>" [ Down, Down ] (Just (Text "c"))
            , DirCase "ab4" "<a><b>c</b></a>" [ Down, Down, Up, Up ] (Just (Element "a" [] [ Element "b" [] [ Text "c" ] ]))
            , DirCase "ab5" "<a><b>c</b></a>" [ Down, Down, Up, Up, Up ] Nothing
            , DirCase "ab6" "<a><b><c/><d/></b></a>" [ Down, Down, Right ] (Just (Element "d" [] []))
            , DirCase "basic1" "<html><div>foo</div></html>" [ Down, Down ] (Just (Text "foo"))

            -- Test next() depth-first
            , DirCase "depth-first"
                "<a><b><c></c></b><d></d><e><f></f></e>"
                [ Next ]
                (Just (Element "b" [] [ Element "c" [] [] ]))
            , DirCase "depth-first"
                "<a><b><c></c></b><d></d><e><f></f></e>"
                [ Next, Next ]
                (Just (Element "c" [] []))
            , DirCase "depth-first"
                "<a><b><c></c></b><d></d><e><f></f></e>"
                [ Next, Next, Next ]
                (Just (Element "d" [] []))
            , DirCase "depth-first"
                "<a><b><c></c></b><d></d><e><f></f></e>"
                [ Next, Next, Next, Next ]
                (Just (Element "e" [] [ Element "f" [] [] ]))
            ]


selectAll : String -> String -> Selector -> List Node -> Test
selectAll name html selector expected =
    test (name ++ ": " ++ html) <|
        \_ ->
            let
                loc =
                    html
                        |> Html.Parser.runElement config
                        |> Result.withDefault (Html.Parser.Text "")
                        |> Q.toLoc

                actual =
                    Q.selectAll selector loc
            in
            Expect.equal expected actual


selectorTests =
    let
        html1 =
            """
    <div>
        <article class="comment">
            <div class="author">
                <time id="f" x-has date="..." />
            </div>
            <span x-has/>
        </article>
        <footer id="f"></footer>
    </div>
        """

        time =
            Element "time" [ ( "id", "f" ), ( "x-has", "" ), ( "date", "..." ) ] []
    in
    describe "simple selectors"
        [ selectAll
            "*"
            "<a><b><c></c></b></a>"
            Q.any
            [ Element "a" [] [ Element "b" [] [ Element "c" [] [] ] ]
            , Element "b" [] [ Element "c" [] [] ]
            , Element "c" [] []
            ]
        , selectAll
            ""
            "<a><b><c></c></b></a>"
            (Q.tagEq "b")
            [ Element "b" [] [ Element "c" [] [] ] ]
        , selectAll ""
            html1
            (Q.idEq "f")
            [ time
            , Element "footer" [ ( "id", "f" ) ] []
            ]
        ]


hasAttrSelectorTests =
    let
        html1 =
            """
    <div>
        <article class="comment">
            <div class="author">
                <time id="f" x-has date="..." />
            </div>
            <span x-has/>
        </article>
        <footer id="f"></footer>
    </div>
        """

        time =
            Element "time" [ ( "id", "f" ), ( "x-has", "" ), ( "date", "..." ) ] []

        footer =
            Element "footer" [ ( "id", "f" ) ] []
    in
    describe "selector: hasAttr"
        [ selectAll
            ""
            html1
            (Q.hasAttr "x-has")
            [ time
            , Element "span" [ ( "x-has", "" ) ] []
            ]
        , selectAll
            "nothing selects nothing"
            html1
            (Q.hasAttr "")
            []
        , selectAll
            "whitespace selects nothing"
            html1
            (Q.hasAttr " ")
            []
        , selectAll
            "case-insensitive"
            html1
            (Q.hasAttr "ID")
            [ time, footer ]
        ]


classEqSelectorTests =
    let
        html1 =
            """
        <html>
            <a CLASS="foo bar"></a>
            <b class="BAR FOO BAR"></b>
            <c   clAss=" BaR   adsf  fOo  "/>
            <d class="foo"/>
            <x class="fooooo" />
        </html>
        """

        a =
            Element "a" [ ( "class", "foo bar" ) ] []

        b =
            Element "b" [ ( "class", "BAR FOO BAR" ) ] []

        c =
            Element "c" [ ( "class", " BaR   adsf  fOo  " ) ] []

        d =
            Element "d" [ ( "class", "foo" ) ] []
    in
    describe "selector: classEq"
        [ selectAll
            ""
            html1
            (Q.classEq "foo")
            [ a, b, c, d ]
        , selectAll
            "nothing selects nothing"
            html1
            (Q.classEq "")
            []
        , selectAll
            "whitespace selects nothing"
            html1
            (Q.classEq " ")
            []
        , selectAll
            "class name is case-insensitive"
            html1
            (Q.classEq "FOO")
            [ a, b, c, d ]
        ]



-- TRANSFORMATION


transformTests =
    let
        eq : String -> String -> (Node -> Node) -> Node -> Test
        eq name html xform expected =
            test (name ++ ": " ++ html) <|
                \_ ->
                    let
                        loc =
                            html
                                |> Html.Parser.runElement config
                                |> Result.withDefault (Html.Parser.Text "")
                                |> Q.toLoc

                        actual =
                            xform (Q.toNode loc)
                    in
                    Expect.equal expected actual
    in
    describe "transforming nodes"
        [ eq "append attrs"
            "<a href=example.com>foo</a>"
            (\node ->
                case node of
                    Element tag attrs kids ->
                        Element tag (List.append attrs [ ( "rel", "foo" ) ]) kids

                    _ ->
                        node
            )
            (Element "a" [ ( "href", "example.com" ), ( "rel", "foo" ) ] [ Text "foo" ])
        , eq "xform single attr"
            "<a href=example.com>foo</a>"
            (\node ->
                case node of
                    Element tag attrs kids ->
                        let
                            newAttrs =
                                List.map
                                    (\( k, v ) ->
                                        if k == "href" then
                                            ( k, "https://" ++ v )

                                        else
                                            ( k, v )
                                    )
                                    attrs
                        in
                        Element tag newAttrs kids

                    _ ->
                        node
            )
            (Element "a" [ ( "href", "https://example.com" ) ] [ Text "foo" ])
        ]


walkTests =
    let
        eq : String -> String -> Q.Selector -> (Node -> Node) -> String -> Test
        eq name html selector xform expected =
            test (name ++ ": " ++ html) <|
                \_ ->
                    let
                        loc =
                            html
                                |> Html.Parser.runElement config
                                |> Result.withDefault (Html.Parser.Text "")
                                |> Q.toLoc

                        actual =
                            Q.walk selector xform loc
                                |> Q.toNode
                                |> Html.Parser.nodeToString
                    in
                    Expect.equal expected actual
    in
    describe "walk"
        [ eq
            "simple walk"
            "<a><b><c></c></b></a>"
            -- selector
            (Q.tagEq "c")
            -- transform
            (\node ->
                case node of
                    Element tag attrs kids ->
                        Element tag attrs [ Text "changed" ]

                    _ ->
                        node
            )
            "<a><b><c>changed</c></b></a>"
        , eq
            "multiple changes at different depths of same branch"
            "<a><b><c></c></b></a>"
            -- selector
            Q.any
            -- transform
            (\node ->
                case node of
                    Element tag attrs kids ->
                        Element tag [ ( "x", "" ) ] kids

                    _ ->
                        node
            )
            "<a x><b x><c x></c></b></a>"
        , eq
            "changes in lateral branches"
            "<root><a><b><c></c></b></a><x><y><z></z></y></x></root>"
            -- selector
            Q.any
            -- transform
            (\node ->
                case node of
                    Element tag attrs kids ->
                        case tag of
                            "b" ->
                                Element tag [ ( "foo", "" ) ] kids

                            "y" ->
                                Element tag [ ( "bar", "" ) ] kids

                            _ ->
                                node

                    _ ->
                        node
            )
            "<root><a><b foo><c></c></b></a><x><y bar><z></z></y></x></root>"
        ]
