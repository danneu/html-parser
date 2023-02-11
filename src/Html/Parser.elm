module Html.Parser exposing
    ( Node(..), Document, Config
    , allCharRefs, noCharRefs, customCharRefs
    , run, runElement, runDocument
    , nodeToHtml, nodesToHtml, nodeToString, nodesToString, nodeToPrettyString, nodesToPrettyString, documentToString, documentToPrettyString
    )

{-| Leniently parse html5 documents and fragments and then render them
into strings or Elm's virtual dom nodes.


# Definition

@docs Node, Document, Config


# Config

@docs allCharRefs, noCharRefs, customCharRefs


# Parse

@docs run, runElement, runDocument


# Render

@docs nodeToHtml, nodesToHtml, nodeToString, nodesToString, nodeToPrettyString, nodesToPrettyString, documentToString, documentToPrettyString

-}

import Dict exposing (Dict)
import Hex
import Html
import Html.Attributes
import Html.CharRefs
import Parser exposing (..)
import Parser.Extra exposing (..)


{-| An html node is tree of text, comments, and element nodes.

An element (e.g. `<div foo="bar">hello</div>`) can have attributes and child nodes.

-}
type Node
    = Text String
    | Comment String
    | Element String (List ( String, String )) (List Node)


{-| Configure the parser. Use the config constructors to create a config object.
-}
type Config
    = Config
        { charRefs : Dict String String
        }


{-| A config with char reference decoding turned on.

This will add ~40kb to your bundle, but it is necessary to decode
entities like `"&Delta;"` into "Δ".

    run allCharRefs "abc&Delta;def"
        == Ok [ text "abcΔdef" ]

-}
allCharRefs : Config
allCharRefs =
    Config { charRefs = Html.CharRefs.all }


{-| A config with char reference decoding turned off.

If you know that the html you are parsing never has named character references,
or if it's sufficient to just consume them as undecoded text, then turning this off will shrink your bundle size.

    run noCharRefs "abc&Delta;def"
        == Ok [ text "abc&Delta;def" ]

-}
noCharRefs : Config
noCharRefs =
    Config { charRefs = Dict.empty }


{-| Provide your own character reference lookup dictionary.

Note that named character references are case sensitive. When providing your own,
you will want to consult the exhaustive `Html.CharRefs.all` dictionary to
see which keys appear multiple times, like "quot" and "QUOT".

Here is an example of providing a small subset of commonly-seen character references.

    config : Html.Parser.Config
    config =
        [ ( "quot", "\"" )
        , ( "QUOT", "\"" )
        , ( "apos", "'" )
        , ( "gt", ">" )
        , ( "GT", ">" )
        , ( "Gt", ">" )
        , ( "lt", "<" )
        , ( "LT", "<" )
        , ( "Lt", "<" )
        , ( "amp", "&" )
        , ( "AMP", "&" )
        , ( "nbsp", "\u{00A0}" )
        ]
        |> Dict.fromList
        |> customCharRefs

    run config "<span>&male; &amp; &female;</span>"
        == Ok (Element "span" [] [Text "&male; & &female;"])

Notice that character references missing from the lookup table are simply parsed as text.

-}
customCharRefs : Dict String String -> Config
customCharRefs dict =
    Config { charRefs = dict }


{-| Parse an html fragment into a list of html nodes.

The html fragment can have multiple top-level nodes.

    run allCharRefs "<div>hi</div><div>bye</div>"
        == Ok
            [ Element "div" [] [ Text "hi" ]
            , Element "div" [] [ Text "bye" ]
            ]

-}
run : Config -> String -> Result (List DeadEnd) (List Node)
run cfg input =
    Parser.run (parseAll cfg) input


{-| Like `run` except it only parses one top-level element and it always returns a single node.
-}
runElement : Config -> String -> Result (List DeadEnd) Node
runElement cfg input =
    Parser.run
        (succeed identity
            |. ws
            |= element cfg
        )
        input


{-| An html document has a `<!doctype>` and then a root html node.
-}
type alias Document =
    { legacyCompat : Bool
    , root : Node
    }


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
runDocument : Config -> String -> Result (List DeadEnd) Document
runDocument cfg input =
    Parser.run (document cfg) input


document : Config -> Parser Document
document cfg =
    succeed Document
        |= doctype
        |. ws
        |= (zeroOrMore (node cfg)
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


parseAll : Config -> Parser (List Node)
parseAll cfg =
    Parser.loop [] <|
        \acc ->
            oneOf
                [ node cfg
                    |> map (\n -> Loop (n :: acc))
                , succeed ()
                    |> map (\_ -> Done (List.reverse acc))
                ]


{-| Chomps zero or more space characters or html comments.
-}
ws : Parser ()
ws =
    loop 0 <|
        ifProgress <|
            oneOf
                [ multiComment "<!--" "-->" NotNestable
                , chompWhile isSpace
                ]


isSpace : Char -> Bool
isSpace c =
    c == ' ' || c == '\n' || c == '\u{000D}' || c == '\t' || c == '\u{000C}' || c == '\u{00A0}'



-- ATTRIBUTES


attributeValueUnquoted : Config -> Parser String
attributeValueUnquoted cfg =
    let
        isLenientUnquotedValueChar c =
            not (isSpace c) && c /= '>' && c /= '&'
    in
    oneOf
        [ chompOneOrMore isLenientUnquotedValueChar
            |> getChompedString
        , characterReference cfg
        ]
        |> oneOrMore "attribute value"
        |> map String.concat


attributeValueQuoted : Config -> Char -> Parser String
attributeValueQuoted cfg quote =
    let
        isQuotedValueChar c =
            c /= quote && c /= '&'
    in
    Parser.succeed identity
        |. chompIf ((==) quote)
        |= (oneOf
                [ chompOneOrMore isQuotedValueChar
                    |> getChompedString
                , characterReference cfg
                ]
                |> zeroOrMore
                |> map String.concat
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


attribute : Config -> Parser ( String, String )
attribute cfg =
    succeed Tuple.pair
        |= attributeKey
        |. ws
        |= oneOf
            [ succeed identity
                |. symbol "="
                |. ws
                |= oneOf
                    [ attributeValueQuoted cfg '"' -- <div foo="bar">
                    , attributeValueQuoted cfg '\'' -- <div foo='bar'>
                    , attributeValueUnquoted cfg -- <div foo=bar>
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


anyCloseTag : Parser String
anyCloseTag =
    succeed identity
        |. token "</"
        |= tagName
        |. ws
        |. token ">"


closeTag : String -> Parser ()
closeTag expectedTag =
    anyCloseTag
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


node : Config -> Parser Node
node cfg =
    succeed identity
        -- HACK: Ignore unmatched close tags like the browser does
        |. zeroOrMore (backtrackable anyCloseTag)
        |= oneOf
            [ text cfg
            , comment
            , element cfg
            ]


comment : Parser Node
comment =
    succeed Comment
        |. symbol "<!--"
        |= (chompUntilEndOr "-->" |> getChompedString)
        |. oneOf [ symbol "-->", succeed () ]


text : Config -> Parser Node
text cfg =
    (loop "" <|
        \acc ->
            oneOf
                [ -- First, text's positive cases
                  succeed (\s -> Loop (acc ++ s))
                    |= characterReference cfg

                -- We want to chomp as much as we can before getting to
                -- the slower lookAhead cases.
                , chompOneOrMore (\c -> c /= '<' && c /= '&')
                    |> getChompedString
                    |> map (\s -> Loop (acc ++ s))

                -- Now, the negative cases
                , succeed (Done acc)
                    |. lookAhead (openTag cfg)
                , succeed (Done acc)
                    |. lookAhead anyCloseTag
                , succeed (Done acc)
                    |. lookAhead comment

                -- Finally, text always consumes
                , succeed (\s -> Loop (acc ++ s))
                    |= justOneChar
                , succeed (Done acc)
                ]
    )
        |> andThen
            (\s ->
                if String.isEmpty s then
                    problem "expected at least one text char"

                else
                    succeed (Text s)
            )


{-| Parse any node unless it's one of the given tags.
-}
notNode : Config -> List String -> Parser Node
notNode cfg tags =
    oneOf
        [ lookAhead
            (openTag cfg
                |> andThen
                    (\( tag, _, _ ) ->
                        if List.member tag tags then
                            problem ""

                        else
                            succeed ()
                    )
            )
            |> andThen (\_ -> element cfg)
        , text cfg
        , comment
        ]


openTag : Config -> Parser ( String, List ( String, String ), OpenTagEnd )
openTag cfg =
    succeed (\a b c -> ( a, b, c ))
        |. symbol "<"
        |= tagName
        |. ws
        |= zeroOrMore (attribute cfg)
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
element : Config -> Parser Node
element cfg =
    openTag cfg
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

                        else if List.member tag escapableRawTextTags then
                            -- Raw text elemements consume text until they find their own closing tag
                            succeed (Element tag attrs)
                                |= (loop [] <|
                                        \acc ->
                                            oneOf
                                                [ succeed (Done (List.reverse acc))
                                                    |. backtrackable (closeTag tag)
                                                , succeed (\n -> Loop (n :: acc))
                                                    |= (chompUntilLookAhead (\c -> c /= '<') (closeTag tag)
                                                            |> getChompedString
                                                            |> map Text
                                                       )
                                                , succeed (Done (List.reverse acc))
                                                ]
                                   )

                        else if isAutoclosingTag tag then
                            -- Autoclosing tag is automatically closed by an opening tag of the same name
                            succeed (Element tag attrs)
                                |= (succeed identity
                                        |= zeroOrMore
                                            (if tag == "head" then
                                                notNode cfg [ tag, "body" ]

                                             else if tag == "td" then
                                                -- https://github.com/danneu/html-parser/issues/5
                                                notNode cfg [ tag, "tr" ]

                                             else
                                                notNode cfg [ tag ]
                                            )
                                        |. oneOf
                                            [ backtrackable (closeTag tag)
                                            , succeed ()
                                            ]
                                   )

                        else
                            -- Normal elements parse all nodes as children until their closing tag
                            succeed (Element tag attrs)
                                |= (loop [] <|
                                        \acc ->
                                            oneOf
                                                [ succeed (Done (List.reverse acc))
                                                    |. backtrackable (closeTag tag)
                                                , succeed (\n -> Loop (n :: acc))
                                                    |= backtrackable (node cfg)
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


namedCharacterReference : Config -> Parser String
namedCharacterReference (Config cfg) =
    chompOneOrMore Char.isAlpha
        |> getChompedString
        |> map
            (\ref ->
                Dict.get ref cfg.charRefs
                    |> Maybe.withDefault ("&" ++ ref ++ ";")
            )


characterReference : Config -> Parser String
characterReference cfg =
    succeed identity
        |. chompIf ((==) '&')
        |= oneOf
            [ backtrackable numericCharacterReference
                |. chompIf ((==) ';')
            , backtrackable (namedCharacterReference cfg)
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


escapableRawTextTags : List String
escapableRawTextTags =
    [ "textarea", "title" ]



-- HELPERS
-- JAVASCRIPT / <script>


{-| Chomp inside a <script> tag until the next </script>

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
        , chompOneOrMore (\char -> char /= '\\' && char /= terminatorChar)
            |> Parser.getChompedString
            |> Parser.map (\chunk -> Parser.Loop (acc ++ chunk))
        ]



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
                            |> String.concat
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
        |> String.concat


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
                |> Html.Parser.run Html.Parser.allCharRefs
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
                String.concat
                    (List.concat
                        [ [ "\n" ++ pad ++ openTagToString tag attrs ]
                        , List.map (prettyNode_ (indent + 1)) kids
                        , [ if List.isEmpty kids then
                                ""

                            else
                                "\n"
                          , (if List.isEmpty kids then
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
        |> Html.Parser.run Html.Parser.allCharRefs
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
