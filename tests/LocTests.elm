module LocTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.CharRefs
import Html.Loc as Q exposing (Loc)
import Html.Parser exposing (Document, Node(..))
import Parser exposing (DeadEnd)
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
            ]
