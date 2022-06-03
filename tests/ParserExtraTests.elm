module ParserExtraTests exposing (..)

{- Here are test our parser helpers / higher order parsers so that
   we can track down bugs caused by them rather than leaving them mingled with the
   main library logic.
-}

import Expect exposing (Expectation)
import Html.Parser exposing (Document, Node(..))
import Parser exposing (..)
import Parser.Extra exposing (..)
import Test exposing (..)


eq : String -> String -> Parser a -> Result (List DeadEnd) a -> Test
eq name input parser expected =
    test (name ++ ": " ++ input) <|
        \_ ->
            let
                actual =
                    Parser.run parser input
            in
            Expect.equal actual expected


err : String -> String -> Parser a -> Test
err name input parser =
    test (name ++ ": " ++ input) <|
        \_ ->
            let
                actual =
                    Parser.run parser input
            in
            Expect.err actual


caseInsensitiveTokenTests =
    let
        parser =
            succeed ()
                |. caseInsensitiveToken "eEe"
                |. end
    in
    describe "caseInsensitiveToken"
        [ eq "basic1" "eee" parser (Ok ())
        , eq "basic2" "EEE" parser (Ok ())
        , err "expect end" "eeE xyz" parser
        , err "basic4" "nope" parser
        , eq "can orig case with getChompedString"
            "Eee"
            (parser |> getChompedString)
            (Ok "Eee")
        ]


chompUntilLookAheadTests =
    let
        parser =
            chompUntilLookAhead (\c -> c /= 'x') (token "xxx")
                |> getChompedString
    in
    describe "chompUntilLookAhead"
        [ eq "" "abcxxx" parser (Ok "abc")
        , err "requires at least one char consumed"
            "xxx"
            parser
        , eq "unconsumes"
            "abcxxx"
            (succeed Tuple.pair
                |= parser
                |= (token "x" |> getChompedString)
                |. token "x"
                |. token "x"
                |. end
            )
            (Ok ( "abc", "x" ))
        ]
