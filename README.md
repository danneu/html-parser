# elm-html-parser

A lenient html5 parser implemented with [Elm](https://elm-lang.org).

A lenient alternative to [hecrj/elm-html-parser](https://package.elm-lang.org/packages/hecrj/html-parser/latest/).

**Experimental**: Also contains undocumented, unpublished, work-in-progress node tree traversal, query, and transformation in `Loc.elm` using a [Zipper](<https://en.wikipedia.org/wiki/Zipper_(data_structure)>) data-structure.

## Usage

-   `run` to parse an html string into a list of html nodes.
-   `runElement` to parse a single html element.
-   `runDocument` to parse `<!doctype html>[...]` into a root node.

```elm
import Html.Parser

"<p class=greeting>hello <strong>world</strong></p>"
|> Html.Parser.run Html.Parser.allCharRefs
-- Ok
--     [ Element "p" [ ("class", "greeting") ]
--          [ Text "hello "
--          , Element "strong" [] [ Text "world" ]
--          ]
--     ]
```

Rendering:

-   `nodeToHtml` or `nodesToHtml` to render parsed nodes into virtual dom nodes that Elm can render.
-   `nodeToString` and `nodesToString` to render parsed nodes into a string.
-   `nodeToPrettyString` and `nodesToPrettyString` to render parsed nodes into indented strings.

## Goals

-   **Leniency**
    -   Avoids validating while parsing
    -   Prefers to immitate browser parsing behavior rather than html5 spec.
    -   Prefers to use the html5 spec only to handle ambiguous cases rather than to prohibit invalid html5
    -   Prefers to fall back to text nodes than short-circuit with parse errors
-   **Handle user-written html**
    -   Users don't write character entities like `&amp;` and `&lt;`. This parser should strive to handle cases like `<p><:</p>` -> `Element "p" [] [ Text "<:" ]`.

## Features / Quirks

-   Characters don't need to be escaped into entities.

    e.g. `<div><:</div>` will parse correctly and doesn't need to be rewritten into `<div>&lt;:</div>`.

-   Tags that should not nest are autoclosed.

    e.g. `<p>a<p>b` -> `<p>a</p><p>b</p>`.

-   Closing tags that have no matching open tags are ignored.

    e.g. `</a><div></div></div></b>` -> `<div></div>`

-   Ignores comments in whitespace positions:

    e.g. `<div <!--comment-->/>` -> `<div/>`

-   Parses comments in text node positions:

    e.g. `div><!--comment--></div>` ->
    `Element "div" [ Comment "comment" ]`

## Differences from existing packages

Currently, there is only one html parser published to Elm packages: [hecrj/elm-html-parser](https://package.elm-lang.org/packages/hecrj/html-parser/latest/).

@hecjr has said that following the html5 spec is a goal of their parser, so their parser is stricter by design and rejects invalid html5.

## Development

`git clone` and `npm install`.

-   `npm test` to run tests
-   `npm docs` to preview docs locally

## Technical notes

### (pre-v2.0.1) Parsing text

**Note: This talks about the `text` parser pre-v2.0.1. Scroll to the next subheader to read about what changed.**

One source of parser complexity is text.

Text in lenient html is basically "anything that wasn't parsed by the other parsers."

This means that you can't have a simple parser like:

```elm
parser : Parser Node
parser =
    oneOf
        [ element
        , comment
        , text
        ]
```

Because how would you define the `text` parser that doesn't underconsume ("parse anything until `'<'`") nor overconsume?

The best way I can think of accomplishing this with `elm/parser` is to, inside a loop, try all of your other parsers and then, if they all fail, consume a single character before looping again.

Something like this:

```elm
parser : Parser (List Node)
parser =
    loop [] <|
        \acc ->
            oneOf
                [ element |> map (\node -> Loop (node :: acc))
                , comment |> map (\node -> Loop (node :: acc))
                , chompIf (\_ -> True)
                    |> map (Text << String.fromChar)
                    |> map (\node -> Loop (node :: acc))
                , succeed ()
                    |> map (\_ -> (Done (List.reverse acc)))
                ]
```

It's not nice and simple anymore. And since it's not possible to make an exhaustive `text` parser, I've had to repeat this kind of logic in various places.

### (v2.0.1) Parsing text

The `text` parser was changed from v2.0.0 to v2.0.1 to be stand-alone meaning that if you apply the `text` parser, it will return a text node that consumes text _up until_ the next non-text node can be parsed, which is pretty useful and DRYs up some text parsing.

The basic idea is to use lookahead like this:

```elm
text : Parser Node
text =
    loop "" <| \acc ->
        oneOf
            [ -- Positive cases for text
              -- We want to be as greedy as possible here to avoid lookahead (slow).
              succeed (\s -> Loop (acc ++ s))
                |= characterReference
            , chompOneOrMore (\c -> c /= '<' && c /= '&')
                |> getChompedString
                |> map (\s -> Loop (acc ++ s))

            -- Now we use lookahead to enumerate negative cases.
            -- Notice how the "match until '<'" case above tries
            -- to avoid ever reaching here.
            , succeed (Done acc)
                |. lookAhead (openTag cfg) -- Starts with "<"
            , succeed (Done acc)
                |. lookAhead anyCloseTag -- Starts with "<"
            , succeed (Done acc)
                |. lookAhead comment -- Starts with "<"

            -- Finally, if we get here, we always consume one character.
            , succeed (\s -> Loop (acc ++ s))
                |= justOneChar
            , succeed (Done acc)
            ]

```

Between v2.0.1 -> v2.0.2, I realized how important it is to enumerate positive
cases so that lookahead is minimized. For example, adding the `chompOneOrMore`
bit above improved my benchmark 700x.

## Special thanks

-   @hecrj and their contributors.
-   @ymtszw for their work on the Javascript `<script>` parser.
