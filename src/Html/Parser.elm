module Html.Parser exposing
    ( Node(..), Document
    , run, runElement, runDocument
    , nodeToHtml, nodesToHtml, nodeToString, nodesToString, nodeToPrettyString, nodesToPrettyString, documentToString, documentToPrettyString
    )

{-| Leniently parse html5 documents and fragments and then render them
into strings or Elm's virtual dom nodes.


# Definition

@docs Node, Document


# Parse

@docs run, runElement, runDocument


# Render

@docs nodeToHtml, nodesToHtml, nodeToString, nodesToString, nodeToPrettyString, nodesToPrettyString, documentToString, documentToPrettyString

-}

import Hex
import Html
import Html.Attributes
import Html.CharRefs
import Parser exposing (..)


{-| An html node is tree of text, comments, and element nodes.

An element (e.g. `<div foo="bar">hello</div>`) can have attributes and child nodes.

-}
type Node
    = Text String
    | Comment String
    | Element String (List ( String, String )) (List Node)


{-| Parse an html fragment into a list of html nodes.

The html fragment can have multiple top-level nodes.

    run "<div>hi</div><div>bye</div>"
        == Ok
            [ Element "div" [] [ Text "hi" ]
            , Element "div" [] [ Text "bye" ]
            ]

-}
run : String -> Result (List DeadEnd) (List Node)
run input =
    Parser.run parseAll input


{-| Like `run` except it only succeeds when the html input is a
single top-level element, and it always returns a single node.
-}
runElement : String -> Result (List DeadEnd) Node
runElement input =
    Parser.run element input


{-| An html document has a `<!doctype>` and then a root html node.
-}
type alias Document =
    { legacyCompat : Bool
    , root : Node
    }


{-| Like `Parser.token` except token is matched case-insensitive.
-}
caseInsensitiveToken : String -> Parser ()
caseInsensitiveToken string =
    let
        help : String -> Parser.Parser () -> Parser.Parser ()
        help string_ parser =
            case String.uncons string_ of
                Nothing ->
                    parser

                Just ( char, rest ) ->
                    parser
                        |> Parser.andThen
                            (\_ ->
                                oneOf
                                    [ chompIf (\c -> Char.toLower c == Char.toLower char)
                                    , problem ("expected case-insensitive char '" ++ String.fromChar char ++ "'")
                                    ]
                            )
                        |> help rest
    in
    help string (succeed ())


doctypeLegacy : Parser Bool
doctypeLegacy =
    -- https://html.spec.whatwg.org/multipage/syntax.html#doctype-legacy-string
    (succeed identity
        |. chompOneOrMore isSpace
        |. caseInsensitiveToken "SYSTEM"
        |. chompOneOrMore isSpace
        |= (oneOf
                [ token "\""
                , token "'"
                ]
                |> getChompedString
           )
    )
        |> andThen
            (\quote ->
                succeed ()
                    |. token "about:legacy-compat"
                    |. token quote
            )
        |> andThen (\_ -> succeed True)


doctype : Parser Bool
doctype =
    -- https://html.spec.whatwg.org/multipage/syntax.html#the-doctype
    succeed identity
        |. token "<!"
        |. caseInsensitiveToken "DOCTYPE"
        |. chompOneOrMore isSpace
        |. caseInsensitiveToken "html"
        |= oneOf
            [ backtrackable doctypeLegacy
            , succeed False
            ]
        |. chompWhile isSpace
        |. token ">"


{-| Parses `<!doctype html>` and any html nodes after.

Always returns a single root node. Wraps nodes in a root `<html>` node if one is not present.

**Caveat**: If there are multiple top-level nodes and one of them is `<html>`, then this
function will wrap them all in another `<html>` node.

-}
runDocument : String -> Result (List DeadEnd) Document
runDocument input =
    Parser.run document input


document : Parser Document
document =
    succeed Document
        |= doctype
        |. ws
        |= (zeroOrMore node
                |> map
                    (\nodes ->
                        case nodes of
                            [] ->
                                Element "html" [] []

                            ((Element "html" _ _) as root) :: [] ->
                                root

                            other :: [] ->
                                Element "html" [] [ other ]

                            _ ->
                                Element "html" [] nodes
                    )
           )


parseAll : Parser (List Node)
parseAll =
    Parser.loop [] <|
        \acc ->
            oneOf
                [ node |> map (\n -> Loop (mergeText n acc))
                , succeed () |> map (\_ -> Done (List.reverse acc))
                ]


mergeText : Node -> List Node -> List Node
mergeText n nodes =
    case ( n, nodes ) of
        ( Text s, (Text prev) :: rest ) ->
            Text (prev ++ s) :: rest

        _ ->
            n :: nodes


{-| Chomps zero or more space characters or html comments.
-}
ws =
    loop 0 <|
        ifProgress <|
            oneOf
                [ multiComment "<!--" "-->" Nestable
                , chompWhile isSpace
                ]


isSpace : Char -> Bool
isSpace c =
    c == ' ' || c == '\n' || c == '\u{000D}' || c == '\n' || c == '\t' || c == '\u{000C}' || c == '\u{00A0}'



-- ATTRIBUTES


attributeValueUnquoted : Parser String
attributeValueUnquoted =
    let
        isUnquotedValueChar c =
            not (isSpace c) && c /= '"' && c /= '\'' && c /= '=' && c /= '<' && c /= '>' && c /= '`' && c /= '&'
    in
    oneOf
        [ chompOneOrMore isUnquotedValueChar
            |> getChompedString
        , characterReference
        ]
        |> oneOrMore "attribute value"
        |> map (String.join "")


attributeValueQuoted : Char -> Parser String
attributeValueQuoted quote =
    let
        isQuotedValueChar c =
            c /= quote && c /= '&'
    in
    Parser.succeed identity
        |. chompIf ((==) quote)
        |= (oneOf
                [ chompOneOrMore isQuotedValueChar
                    |> getChompedString
                , characterReference
                ]
                |> zeroOrMore
                |> map (String.join "")
           )
        |. chompIf ((==) quote)


attributeKey : Parser String
attributeKey =
    let
        isKeyChar c =
            not (isSpace c) && c /= '"' && c /= '\'' && c /= '>' && c /= '/' && c /= '='
    in
    succeed (++)
        -- Attribute can start with '/' but it's ignored
        |. oneOf
            [ -- backtrackable because open tag can end with "/>"
              backtrackable (chompIf ((==) '/'))
            , succeed ()
            ]
        -- Attribute name can start with '=': https://html.spec.whatwg.org/multipage/parsing.html#before-attribute-name-state
        -- e.g. <a =empty />
        |= oneOf
            [ chompIf ((==) '=') |> map (\_ -> "=")
            , succeed ""
            ]
        |= (chompOneOrMore isKeyChar
                |> getChompedString
                |> map String.toLower
           )


attribute : Parser ( String, String )
attribute =
    succeed Tuple.pair
        |= attributeKey
        |. ws
        |= oneOf
            [ succeed identity
                |. symbol "="
                |. ws
                |= oneOf
                    [ attributeValueUnquoted -- <div foo=bar>
                    , attributeValueQuoted '"' -- <div foo="bar">
                    , attributeValueQuoted '\'' -- <div foo='bar'>
                    ]
            , succeed "" -- <div foo>
            ]
        -- Reminder: Consume trailing whitespace so that following parsers don't need to consume whitespace
        -- and then need to backtrack
        |. ws


tagName : Parser String
tagName =
    chompOneOrMore (\c -> Char.isAlphaNum c || c == '-')
        |> getChompedString
        |> map String.toLower


closeTag : String -> Parser ()
closeTag expectedTag =
    (succeed identity
        |. token "</"
        |= tagName
        |. ws
        |. token ">"
    )
        |> andThen
            (\tag ->
                if tag == expectedTag then
                    succeed ()

                else
                    problem ("found closing tag </" ++ tag ++ "> but expected </" ++ expectedTag ++ ">")
            )


type OpenTagEnd
    = NoClose
    | SelfClose


anyCloseTag : Parser ()
anyCloseTag =
    succeed ()
        |. token "</"
        |. tagName
        |. ws
        |. token ">"


node : Parser Node
node =
    succeed identity
        -- HACK: Ignore unmatched close tags like the browser does
        |. zeroOrMore (backtrackable anyCloseTag)
        |= oneOf
            [ text
            , comment
            , backtrackable element
            , justOneChar |> map Text
            ]


comment : Parser Node
comment =
    succeed Comment
        |. symbol "<!--"
        |= (chompUntil "-->" |> getChompedString)
        |. symbol "-->"


text : Parser Node
text =
    oneOf
        [ succeed Text
            |= backtrackable characterReference
        , succeed Text
            |= (chompOneOrMore (\c -> c /= '<' && c /= '&') |> getChompedString)
        ]


{-| Parse any node unless it's one of the given tags.
-}
notNode : List String -> Parser Node
notNode tags =
    oneOf
        [ lookAhead
            (openTag
                |> andThen
                    (\( tag, _, _ ) ->
                        if List.member tag tags then
                            problem ""

                        else
                            succeed ()
                    )
            )
            |> andThen (\_ -> element)
        , text
        , comment
        ]


openTag : Parser ( String, List ( String, String ), OpenTagEnd )
openTag =
    succeed (\a b c -> ( a, b, c ))
        |. symbol "<"
        |. ws
        |= tagName
        |. ws
        |= zeroOrMore attribute
        |. ws
        |= oneOf
            [ succeed NoClose
                |. symbol ">"
            , succeed SelfClose
                |. symbol "/>"
            ]


{-| Parse one html element including all of its children.

Html elements always have an opening `<tag>`, but they don't always have children nor do
they always have a closing `</tag>`.

The element parser is useful when the html input will only have one top-level element.

-}
element : Parser Node
element =
    openTag
        |> andThen
            (\( tag, attrs, end ) ->
                case end of
                    SelfClose ->
                        succeed (Element tag attrs [])

                    NoClose ->
                        if tag == "script" then
                            succeed (Element tag attrs)
                                |= consumeJavascriptUntilClosingTag

                        else if isVoidTag tag then
                            -- Void element expects no closing tag
                            succeed (Element tag attrs [])

                        else if isAutoclosingTag tag then
                            -- Autoclosing tag is automatically closed by an opening tag of the same name
                            succeed (Element tag attrs)
                                |= oneOf
                                    [ succeed identity
                                        |= zeroOrMore
                                            (if tag == "head" then
                                                notNode [ tag, "body" ]

                                             else
                                                notNode [ tag ]
                                            )
                                        |. oneOf
                                            [ backtrackable (closeTag tag)
                                            , succeed ()
                                            ]
                                    ]

                        else
                            -- Normal elements parse all nodes as children until their closing tag
                            succeed (Element tag attrs)
                                |= (loop [] <|
                                        \acc ->
                                            oneOf
                                                [ backtrackable (closeTag tag) |> map (\_ -> Done (List.reverse acc))
                                                , succeed (\n -> Loop (mergeText n acc))
                                                    |= backtrackable node
                                                , succeed () |> map (\_ -> Done (List.reverse acc))
                                                ]
                                   )
            )



-- CHARACTER REFERENCE


{-| Parse one or more hexadecimal digits into an integer.
-}
base16 : Parser Int
base16 =
    chompOneOrMore Char.isHexDigit
        |> getChompedString
        |> andThen
            (\hex ->
                case Hex.fromString (String.toLower hex) of
                    Ok num ->
                        succeed num

                    Err msg ->
                        problem msg
            )


{-| Parse one or more 0-9 digits into an integer.
-}
base10 : Parser Int
base10 =
    chompOneOrMore Char.isDigit
        |> getChompedString
        |> andThen
            (\digits ->
                String.toInt digits
                    |> Maybe.map succeed
                    |> Maybe.withDefault (problem "bad number")
            )


numericCharacterReference : Parser String
numericCharacterReference =
    let
        codepoint =
            oneOf
                [ succeed identity
                    |. chompIf (\c -> c == 'x' || c == 'X')
                    |= base16
                , succeed identity
                    |= base10
                ]
    in
    succeed identity
        |. chompIf ((==) '#')
        |= (codepoint
                |> andThen
                    (\code ->
                        -- https://html.spec.whatwg.org/multipage/parsing.html#decimal-character-reference-start-state
                        if code == 0 then
                            succeed '�'

                        else if 0xD800 <= code && code <= 0xDFFF then
                            -- Is surrogate
                            succeed '�'

                        else
                            succeed (Char.fromCode code)
                    )
                |> map String.fromChar
           )


namedCharacterReference : Parser String
namedCharacterReference =
    chompOneOrMore Char.isAlpha
        |> getChompedString
        |> map
            (\ref ->
                Html.CharRefs.decode ref
                    |> Maybe.withDefault ("&" ++ ref ++ ";")
            )


characterReference : Parser String
characterReference =
    succeed identity
        |. chompIf ((==) '&')
        |= oneOf
            [ backtrackable numericCharacterReference
                |. chompIf ((==) ';')
            , backtrackable namedCharacterReference
                |. chompIf ((==) ';')
            , succeed "&"
            ]



-- SPECIAL ELEMENTS


isVoidTag : String -> Bool
isVoidTag tag =
    List.member tag voidTags


voidTags : List String
voidTags =
    [ "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "source", "track", "wbr" ]


isAutoclosingTag : String -> Bool
isAutoclosingTag tag =
    List.member tag autoclosingTags


autoclosingTags : List String
autoclosingTags =
    [ "body", "colgroup", "dd", "dt", "head", "html", "li", "option", "p", "tbody", "td", "tfoot", "th", "thead", "tr" ]



-- HELPERS


chompOneOrMore : (Char -> Bool) -> Parser ()
chompOneOrMore predicate =
    Parser.chompIf predicate
        |. Parser.chompWhile predicate


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


zeroOrMore : Parser a -> Parser (List a)
zeroOrMore parser =
    Parser.loop []
        (\acc ->
            oneOf
                [ succeed (\val -> Loop (val :: acc))
                    |= parser
                , succeed (Done (List.reverse acc))
                ]
        )


oneOrMore : String -> Parser a -> Parser (List a)
oneOrMore name parser =
    Parser.loop []
        (\acc ->
            oneOf
                [ succeed (\val -> Loop (val :: acc))
                    |= parser
                , if List.isEmpty acc then
                    problem ("expecting at least one " ++ name)

                  else
                    succeed (Done (List.reverse acc))
                ]
        )


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



-- JAVASCRIPT / <script>


{-| Chomp inside a <script> tag until the next </script>.

This can't be implemented as `chompUntil "</script>"` because
the Javascript inside the script tag may contain the string "</script>".

For example: "<script>alert('</script>')</script>"

-}
consumeJavascriptUntilClosingTag : Parser (List Node)
consumeJavascriptUntilClosingTag =
    Parser.loop [] <|
        \acc ->
            let
                accumulate newNode =
                    case ( acc, newNode ) of
                        ( [], first ) ->
                            Loop [ first ]

                        ( (Text accChunk) :: tail, Text newChunk ) ->
                            -- Merge top-most text node unless HTML comment nodes are interleaved
                            Loop (Text (accChunk ++ newChunk) :: tail)

                        ( nonTextNode :: tail, _ ) ->
                            Loop (newNode :: nonTextNode :: tail)
            in
            Parser.oneOf
                [ -- HTML comments are, albeit considered a bad practice recently,
                  -- allowed inside <script> to hide scripts from really ancient web browser
                  comment
                    |> map accumulate
                , lineComment "//"
                    |> Parser.getChompedString
                    |> Parser.map (Text >> accumulate)
                , Parser.multiComment "/*" "*/" Parser.NotNestable
                    |> Parser.getChompedString
                    |> Parser.map (Text >> accumulate)
                , javaScriptStringLike '"'
                    |> Parser.map (Text >> accumulate)
                , javaScriptStringLike '\''
                    |> Parser.map (Text >> accumulate)
                , javaScriptStringLike '`'
                    |> Parser.map (Text >> accumulate)
                , closeTag "script"
                    |> Parser.map (\() -> Done (List.reverse acc))
                , Parser.chompIf (always True)
                    |> Parser.getChompedString
                    |> Parser.map (Text >> accumulate)
                ]


javaScriptStringLike : Char -> Parser String
javaScriptStringLike terminatorChar =
    let
        terminatorStr =
            String.fromChar terminatorChar
    in
    Parser.succeed identity
        |. Parser.token terminatorStr
        |= Parser.loop "" (stringHelp terminatorChar terminatorStr)
        -- Restoring original shape
        |> Parser.map (\chunk -> terminatorStr ++ chunk ++ terminatorStr)


stringHelp : Char -> String -> String -> Parser (Parser.Step String String)
stringHelp terminatorChar terminatorStr acc =
    Parser.oneOf
        [ Parser.succeed (\char -> Parser.Loop (acc ++ "\\" ++ char))
            |. Parser.token "\\"
            |= justOneChar
        , Parser.token terminatorStr
            |> Parser.map (\_ -> Parser.Done acc)

        -- Orig code caused infinite loop with single terminator char <script>'</script>
        -- , Parser.chompWhile (\char -> char /= '\\' && char /= terminatorChar)
        , chompOneOrMore (\char -> char /= '\\' && char /= terminatorChar)
            |> Parser.getChompedString
            |> Parser.map (\chunk -> Parser.Loop (acc ++ chunk))
        ]


justOneChar : Parser String
justOneChar =
    chompIf (always True)
        |> getChompedString



-- RENDER


openTagToString : String -> List ( String, String ) -> String
openTagToString tag attrs =
    "<"
        ++ tag
        ++ (if List.isEmpty attrs then
                ""

            else
                List.map
                    (\( k, v ) ->
                        if String.isEmpty v then
                            k

                        else
                            k ++ "=\"" ++ v ++ "\""
                    )
                    attrs
                    |> String.join " "
                    |> (\s -> " " ++ s)
           )
        ++ ">"


{-| Convert an html node into a non-pretty string.

    nodeToString (Element "a" [] [ Text "hi" ])
        == "<a>hi</a>"

-}
nodeToString : Node -> String
nodeToString node_ =
    case node_ of
        Text s ->
            s

        Comment s ->
            "<!--" ++ s ++ "-->"

        Element tag attrs kids ->
            if isVoidTag tag && List.isEmpty kids then
                openTagToString tag attrs

            else
                openTagToString tag attrs
                    ++ (List.map nodeToString kids
                            |> String.join ""
                       )
                    ++ "</"
                    ++ tag
                    ++ ">"


{-| Convert multiple html nodes into a non-pretty string.

    nodesToString
        [ Element "a" [] [ Text "hi" ]
        , Element "div" [] [ Element "span" [] [] ]
        ]
        == "<a>hi</a><div><span></span></div>"

-}
nodesToString : List Node -> String
nodesToString nodes =
    List.map nodeToString nodes
        |> String.join ""


{-| Turn a single node into an Elm html node that Elm can render.
-}
nodeToHtml : Node -> Html.Html msg
nodeToHtml node_ =
    case node_ of
        Text s ->
            Html.text s

        Comment _ ->
            Html.text ""

        Element tag attrs kids ->
            Html.node tag
                (List.map (\( k, v ) -> Html.Attributes.attribute k v) attrs)
                (List.map nodeToHtml kids)


{-| Turn a multiple html nodes into Elm html that Elm can render.

    view : Html Msg
    view =
        Html.div
            []
            ("<p>hello world</p>"
                |> Html.Parser.run
                |> Result.map Html.Parser.nodesToHtml
                |> Result.withDefault [ Html.text "parse error" ]
            )

-}
nodesToHtml : List Node -> List (Html.Html msg)
nodesToHtml nodes =
    List.foldr
        (\node_ acc ->
            nodeToHtml node_ :: acc
        )
        []
        nodes


{-| Generate a pretty string for a single html node.
-}
nodeToPrettyString : Node -> String
nodeToPrettyString node_ =
    prettyNode_ 0 node_


prettyNode_ : Int -> Node -> String
prettyNode_ indent node_ =
    let
        pad =
            String.padLeft (indent * 4) ' ' ""
    in
    case node_ of
        Text s ->
            if String.isEmpty (String.trim s) then
                ""

            else
                let
                    ss =
                        s
                            |> String.trim
                            |> String.split "\n"
                            |> List.filter (String.trim >> String.isEmpty >> not)
                            |> String.join ("\n" ++ String.padLeft (indent * 4) ' ' "")
                in
                "\n" ++ pad ++ ss

        Comment s ->
            if String.isEmpty s then
                ""

            else
                "\n" ++ pad ++ "<!--" ++ s ++ "-->"

        Element tag attrs kids ->
            String.trim <|
                String.join ""
                    (List.concat
                        [ [ "\n" ++ pad ++ openTagToString tag attrs ]
                        , List.map (prettyNode_ (indent + 1)) kids
                        , [ if List.isEmpty kids then
                                ""

                            else
                                "\n"
                          ]
                        , [ (if List.isEmpty kids then
                                ""

                             else
                                pad
                            )
                                ++ (if isVoidTag tag && List.isEmpty kids then
                                        ""

                                    else
                                        "</"
                                            ++ tag
                                            ++ ">"
                                   )
                          ]
                        ]
                    )


{-| Turn a node tree into a pretty-printed, indented html string.

    ("<a><b><c>hello</c></b></a>"
        |> Html.Parser.run
        |> Result.map nodesToPrettyString
    )
        == Ok """<a>
        <b>
            <c>
                hello
            </c>
        </b>
    </a>"""

-}
nodesToPrettyString : List Node -> String
nodesToPrettyString nodes =
    List.foldl
        (\node_ acc ->
            acc ++ nodeToPrettyString node_
        )
        ""
        nodes


doctypeToString : Bool -> String
doctypeToString legacyCompat =
    if legacyCompat then
        "<!DOCTYPE html SYSTEM \"about:legacy-compat\">"

    else
        "<!DOCTYPE html>"


{-| Convert a document into a string starting with `<!doctype html>` followed by the root html node.
-}
documentToString : Document -> String
documentToString doc =
    doctypeToString doc.legacyCompat ++ "\n" ++ nodeToString doc.root


{-| Convert a document into a pretty, indented string.
-}
documentToPrettyString : Document -> String
documentToPrettyString doc =
    doctypeToString doc.legacyCompat ++ "\n" ++ nodeToPrettyString doc.root
