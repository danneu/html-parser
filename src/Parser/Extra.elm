module Parser.Extra exposing
    ( caseInsensitiveToken
    , chompOneOrMore
    , chompUntilLookAhead
    , ifProgress
    , justOneChar
    , lookAhead
    , oneOrMore
    , zeroOrMore
    )

import Parser exposing (..)


justOneChar : Parser String
justOneChar =
    chompIf (always True)
        |> getChompedString


chompOneOrMore : (Char -> Bool) -> Parser ()
chompOneOrMore predicate =
    chompIf predicate
        |. chompWhile predicate


{-| Chomps until a parser applies but unconsumes that parser's progress.

Pass in a predicate that includes everything until the first char of the
lookAhead parser. This is so that we can short-circuit the slow process
of trying the parser and then consuming just one char over and over again.

e.g. the parser passed to chompUntilLookAhead can be arbitrarily slow,
like parsing the rest of the input before failing somewhere far ahead,
yet all we do in that event is consume a single character and apply
the parser again at offset+1.

-}
chompUntilLookAhead : (Char -> Bool) -> Parser a -> Parser ()
chompUntilLookAhead predicate parser =
    (loop "" <|
        \acc ->
            oneOf
                [ succeed (\s -> Loop (acc ++ s))
                    |= (chompOneOrMore predicate |> getChompedString)
                , succeed (Done acc)
                    |. lookAhead parser
                , succeed (\s -> Loop (acc ++ s))
                    |= justOneChar
                , succeed (Done acc)
                ]
    )
        |> andThen
            (\acc ->
                if String.isEmpty acc then
                    problem "expected some text"

                else
                    succeed ()
            )


{-| Like Parser.token except case-insensitive.
-}
caseInsensitiveToken : String -> Parser ()
caseInsensitiveToken token =
    loop (String.toLower token) <|
        \chars ->
            case String.uncons chars of
                Just ( char, rest ) ->
                    oneOf
                        [ succeed (Loop rest)
                            |. chompIf (\c -> Char.toLower c == char)
                        , problem ("expected case-insensitive char '" ++ String.fromChar char ++ "'")
                        ]

                Nothing ->
                    succeed (Done ())


zeroOrMore : Parser a -> Parser (List a)
zeroOrMore parser =
    loop [] <|
        \acc ->
            oneOf
                [ succeed (\val -> Loop (val :: acc))
                    |= parser
                , succeed (Done (List.reverse acc))
                ]


oneOrMore : String -> Parser a -> Parser (List a)
oneOrMore name parser =
    loop [] <|
        \acc ->
            oneOf
                [ succeed (\val -> Loop (val :: acc))
                    |= parser
                , if List.isEmpty acc then
                    problem ("expecting at least one " ++ name)

                  else
                    succeed (Done (List.reverse acc))
                ]


{-| Create a parser that backtracks on success.
-}
lookAhead : Parser a -> Parser ()
lookAhead parser =
    oneOf
        [ oneOf
            [ parser
                |> backtrackable
                |> andThen (\_ -> commit ())
                |> andThen (\_ -> problem "")
            , succeed
                (parser
                    |> backtrackable
                    |> map (\_ -> ())
                )
            ]
            |> backtrackable
        , succeed (succeed ())
        ]
        |> andThen identity


{-| Loop a parser only if it actually consumes something.

For example, parsers like `spaces` and `chompWhile` will happily
consume 0 input, so when put in a loop the parser will never terminate.

-}
ifProgress : Parser a -> Int -> Parser (Step Int ())
ifProgress parser offset =
    succeed identity
        |. parser
        |= getOffset
        |> map
            (\newOffset ->
                if offset == newOffset then
                    Done ()

                else
                    Loop newOffset
            )
