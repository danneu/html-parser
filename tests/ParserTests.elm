module ParserTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.CharRefs
import Html.Parser exposing (Document, Node(..))
import Parser exposing (DeadEnd)
import Test exposing (..)


config =
    Html.Parser.configWithCharRefs


testDoc : List ( String, String, Result (List DeadEnd) Document ) -> List Test
testDoc cases =
    List.map
        (\( name, html, expected ) ->
            test (name ++ ": " ++ html)
                (\_ ->
                    let
                        actual =
                            Html.Parser.runDocument config html
                    in
                    case expected of
                        Ok _ ->
                            Expect.equal actual expected

                        Err _ ->
                            case actual of
                                Err _ ->
                                    Expect.pass

                                _ ->
                                    Expect.equal actual expected
                )
        )
        cases


testStringRoundtrip : List ( String, String, Result (List DeadEnd) String ) -> List Test
testStringRoundtrip cases =
    List.map
        (\( name, html, expected ) ->
            test (name ++ " \"" ++ html ++ "\"")
                (\_ ->
                    let
                        actual =
                            Html.Parser.run config html
                                |> Result.map Html.Parser.nodesToString
                    in
                    case expected of
                        Err _ ->
                            case actual of
                                Err _ ->
                                    Expect.pass

                                _ ->
                                    Expect.equal actual expected

                        Ok _ ->
                            Expect.equal actual expected
                )
        )
        cases


testAll : List ( String, String, Result (List DeadEnd) (List Node) ) -> List Test
testAll cases =
    List.map
        (\( name, html, expected ) ->
            test (name ++ " \"" ++ html ++ "\"")
                (\_ ->
                    let
                        actual =
                            Html.Parser.run config html
                    in
                    case expected of
                        Err _ ->
                            case actual of
                                Err _ ->
                                    Expect.pass

                                _ ->
                                    Expect.equal actual expected

                        Ok _ ->
                            Expect.equal actual expected
                )
        )
        cases


renderStringTests =
    describe "stringify tests" <|
        testStringRoundtrip
            [ ( "basic", "<a></a>", Ok "<a></a>" )
            , ( "basic", "<a>foo</a>", Ok "<a>foo</a>" )
            , ( "basic", "<a> foo </a>", Ok "<a> foo </a>" )
            , ( "basic", "<a><b><c>foo</c></b></a>", Ok "<a><b><c>foo</c></b></a>" )
            , ( "basic", "<A><B><C>foo</C></B></A>", Ok "<a><b><c>foo</c></b></a>" )
            , ( "basic", "<a><!--a-->b<!--c--></a>", Ok "<a><!--a-->b<!--c--></a>" )
            ]


ambiguousTextTests =
    describe "ambiguous text node parsing" <|
        testAll
            [ ( "basic1", "<div>:></div>", Ok [ Element "div" [] [ Text ":>" ] ] )
            , ( "basic2", "<div><:</div>", Ok [ Element "div" [] [ Text "<:" ] ] )
            , ( "basic3", "<:", Ok [ Text "<:" ] )
            , ( "basic4", ":>", Ok [ Text ":>" ] )
            ]


voidTests =
    describe "void nodes" <|
        testAll
            [ ( "without closing tag", "<hr>a", Ok [ Element "hr" [] [], Text "a" ] )
            , ( "with closing tag", "<hr>a</hr>", Ok [ Element "hr" [] [], Text "a" ] )
            ]


documentTests =
    describe "document parsing" <|
        testDoc
            [ ( "", "<!doctype html>", Ok (Document False (Element "html" [] [])) )
            , ( "", "<!DOCTYPE HTML>", Ok (Document False (Element "html" [] [])) )
            , ( "", "<!doctype htmlSYSTEM \"about:legacy-compat\">", Err [] )
            , ( "", "<!doctype html SYSTEM \"about:legacy-compat\">", Ok (Document True (Element "html" [] [])) )
            , ( "", "<!doctype html sYsTem 'about:legacy-compat'>", Ok (Document True (Element "html" [] [])) )
            , ( ""
              , "<!doctype html><head>a<body>b"
              , Ok
                    (Document False
                        (Element "html"
                            []
                            [ Element "head" [] [ Text "a" ]
                            , Element "body" [] [ Text "b" ]
                            ]
                        )
                    )
              )
            , ( ""
              , "<!doctype html><meta charset=\"utf-8\">"
              , Ok
                    (Document False
                        (Element "html"
                            []
                            [ Element "meta" [ ( "charset", "utf-8" ) ] []
                            ]
                        )
                    )
              )
            ]


basicCommentTests =
    describe "basic comment parsing" <|
        testAll
            [ ( "basic1", "<!---->", Ok [ Comment "" ] )
            , ( "basic2", "<!-- -->", Ok [ Comment " " ] )
            , ( "basic3", "<!--x-->", Ok [ Comment "x" ] )
            , ( "basic4", "<a><!--x--></a>", Ok [ Element "a" [] [ Comment "x" ] ] )
            , ( "basic5", "<!--a--><a><!--b--></a><!--c-->", Ok [ Comment "a", Element "a" [] [ Comment "b" ], Comment "c" ] )
            , ( "basic6", "<!---->-->", Ok [ Comment "", Text "-->" ] )
            ]


basicElementTests =
    describe "basic element parsing" <|
        testAll
            [ ( "my-basic1", "<a>:></a>", Ok [ Element "a" [] [ Text ":>" ] ] )

            -- , ( "my-basic2", "<a><:</a>", Ok [ Element "a" [] [ Text "<:" ] ] )
            -- Tests from hecrj/elm-html-parser
            , ( "basic1", "<a></a>", Ok [ Element "a" [] [] ] )
            , ( "basic2", "<a></a >", Ok [ Element "a" [] [] ] )
            , ( "basic3", "<A></A >", Ok [ Element "a" [] [] ] )
            , ( "basic4", " <a></a> ", Ok [ Text " ", Element "a" [] [], Text " " ] )
            , ( "basic5", "a<a></a>b", Ok [ Text "a", Element "a" [] [], Text "b" ] )
            , ( "basic6", "<A></A>", Ok [ Element "a" [] [] ] )
            ]


basicAttributeTests =
    describe "basic attribute parsing" <|
        testAll
            [ ( "unquoted1", "<div a=b/></div>", Ok [ Element "div" [ ( "a", "b/" ) ] [] ] )
            , ( "unquoted2", "<div a=b />", Ok [ Element "div" [ ( "a", "b" ) ] [] ] )
            , ( "single-quoted", "<div a='b'/>", Ok [ Element "div" [ ( "a", "b" ) ] [] ] )
            , ( "double-quoted", "<div a=\"b\"/>", Ok [ Element "div" [ ( "a", "b" ) ] [] ] )
            , ( "key-only1", "<div a></div>", Ok [ Element "div" [ ( "a", "" ) ] [] ] )
            , ( "key-only2", "<div a/>", Ok [ Element "div" [ ( "a", "" ) ] [] ] )
            , ( "everything"
              , "<div a=b c='d' e=\"f\" g/>"
              , Ok
                    [ Element "div"
                        [ ( "a", "b" )
                        , ( "c", "d" )
                        , ( "e", "f" )
                        , ( "g", "" )
                        ]
                        []
                    ]
              )
            ]


autoclosingTests =
    describe "autoclosing elements" <|
        testAll
            [ ( "p-basic1", "<p>a<p>b", Ok [ Element "p" [] [ Text "a" ], Element "p" [] [ Text "b" ] ] )
            , ( "li-basic", "<li><li>", Ok [ Element "li" [] [], Element "li" [] [] ] )
            , ( "li-basic", "<ul><li><li></ul>", Ok [ Element "ul" [] [ Element "li" [] [], Element "li" [] [] ] ] )
            , ( "li-basic", "<li>a<li>b</li>", Ok [ Element "li" [] [ Text "a" ], Element "li" [] [ Text "b" ] ] )
            , ( "li-comment"
              , "<li>a<!--c--><li>b<!--d--></li>"
              , Ok
                    [ Element "li" [] [ Text "a", Comment "c" ]
                    , Element "li" [] [ Text "b", Comment "d" ]
                    ]
              )

            -- TODO
            -- , ( "li-comment-backtrack"
            --   , "<li>a<!-c<li>b<!-d</li>"
            --   , Ok
            --         [ Element "li" [] [ Text "a<!-c" ]
            --         , Element "li" [] [ Text "b<!-d" ]
            --         ]
            --   )
            , ( "li-basic", "<li>a</li><li>b", Ok [ Element "li" [] [ Text "a" ], Element "li" [] [ Text "b" ] ] )
            , ( "li-basic1", "<li>a</li><li>b</li>", Ok [ Element "li" [] [ Text "a" ], Element "li" [] [ Text "b" ] ] )
            , ( "li-basic2"
              , "<li>a<li>b</li>c</li>"
              , Ok
                    [ Element "li" [] [ Text "a" ]
                    , Element "li" [] [ Text "b" ]
                    , Text "c"
                    ]
              )
            , ( "li-basic3"
              , "<li>a<ul><li>b</li></ul>c</li>"
              , Ok
                    [ Element "li"
                        []
                        [ Text "a"
                        , Element "ul"
                            []
                            [ Element "li" [] [ Text "b" ]
                            ]
                        , Text "c"
                        ]
                    ]
              )

            -- Not valid html, but the parser should still parse it.
            , ( "head1"
              , "<head>a<head>b"
              , Ok
                    [ Element "head" [] [ Text "a" ]
                    , Element "head" [] [ Text "b" ]
                    ]
              )

            -- Unlike the previous test, here's an example of where the parser must invoke the html5
            -- spec only to disambiguate where <body> should be a child vs. sibling
            -- of the unended <head> element.
            , ( "head2"
              , "<head><title>hello</title><body>"
              , Ok
                    [ Element "head"
                        []
                        [ Element "title" [] [ Text "hello" ] ]
                    , Element "body" [] []
                    ]
              )
            ]


basicNestingTests =
    describe "nested elements" <|
        testAll
            [ ( "abc"
              , "<a><b><c></c></b></a>"
              , Ok
                    [ Element "a"
                        []
                        [ Element "b"
                            []
                            [ Element "c" [] []
                            ]
                        ]
                    ]
              )
            , ( "nested <ul> where all <li> are closed </li>"
              , """<ul><li>a</li><li>b<ul><li>x</li><li>y</li></ul></li><li>c</li></ul>"""
              , Ok
                    [ Element "ul"
                        []
                        [ Element "li" [] [ Text "a" ]
                        , Element "li"
                            []
                            [ Text "b"
                            , Element "ul"
                                []
                                [ Element "li" [] [ Text "x" ]
                                , Element "li" [] [ Text "y" ]
                                ]
                            ]
                        , Element "li" [] [ Text "c" ]
                        ]
                    ]
              )
            , ( "nested <ul> where zero <li> are closed with </li>"
              , """<ul><li>a<li>b<ul><li>x<li>y</ul><li>c</ul>"""
              , Ok
                    [ Element "ul"
                        []
                        [ Element "li" [] [ Text "a" ]
                        , Element "li"
                            []
                            [ Text "b"
                            , Element "ul"
                                []
                                [ Element "li" [] [ Text "x" ]
                                , Element "li" [] [ Text "y" ]
                                ]
                            ]
                        , Element "li" [] [ Text "c" ]
                        ]
                    ]
              )
            ]


voidElementTests : Test
voidElementTests =
    describe "void elements" <|
        testAll
            [ ( "invalid", "<hr></hr>", Ok [ Element "hr" [] [] ] )
            , ( "valid1", "<hr>", Ok [ Element "hr" [] [] ] )
            , ( "valid2", "<hr/>", Ok [ Element "hr" [] [] ] )
            ]


textNodeTests : Test
textNodeTests =
    describe "text node parsing" <|
        testAll
            [ ( "empty", "", Ok [] )
            , ( "space", " ", Ok [ Text " " ] )
            , ( "basic1", "1", Ok [ Text "1" ] )
            , ( "basic2", "a", Ok [ Text "a" ] )
            , ( "basic3", "1a", Ok [ Text "1a" ] )
            , ( "basic4", "^", Ok [ Text "^" ] )
            , ( "decode1", "&", Ok [ Text "&" ] )
            , ( "decode2", "&amp;", Ok [ Text "&" ] )
            , ( "decode3", "&lt;", Ok [ Text "<" ] )
            , ( "decode4", "&gt;", Ok [ Text ">" ] )
            , ( "decode5", "&apos;", Ok [ Text "'" ] )
            , ( "decode6", "&#38;", Ok [ Text "&" ] )
            , ( "decode7", "&#x26;", Ok [ Text "&" ] )
            , ( "decode8", "&#x3E;", Ok [ Text ">" ] )
            , ( "decode9", "&#383;", Ok [ Text "ſ" ] )
            , ( "decodeA", "&nbsp;", Ok [ Text "\u{00A0}" ] )
            , ( "decodeB", "&nbsp;&nbsp;", Ok [ Text "\u{00A0}\u{00A0}" ] )
            , ( "decodeC", "a&nbsp;b", Ok [ Text "a\u{00A0}b" ] )
            , ( "decodeD", "a&nbsp;&nbsp;b", Ok [ Text "a\u{00A0}\u{00A0}b" ] )
            , ( "decodeE", """<img alt="&lt;">""", Ok [ Element "img" [ ( "alt", "<" ) ] [] ] )
            , ( "decodeF", "&#0038;", Ok [ Text "&" ] )
            ]


scriptTests : Test
scriptTests =
    describe "<script> node" <|
        testAll
            [ ( "basic1", "<script></script>", Ok [ Element "script" [] [] ] )
            , ( "basic2", "<script>foo</script>", Ok [ Element "script" [] [ Text "foo" ] ] )

            -- Copy browser behavior
            , ( "basic3", "<script></script></script>", Ok [ Element "script" [] [] ] )
            , ( "basic4", "<script><script></script>", Ok [ Element "script" [] [ Text "<script>" ] ] )
            , ( "attrs1", "<script src=index.js></script>", Ok [ Element "script" [ ( "src", "index.js" ) ] [] ] )
            , ( "js1", "<script>'</script>'</script>", Ok [ Element "script" [] [ Text "'</script>'" ] ] )
            , ( "js2", "<script>\"</script>\"</script>", Ok [ Element "script" [] [ Text "\"</script>\"" ] ] )
            , ( "js3", "<script>`</script>`</script>", Ok [ Element "script" [] [ Text "`</script>`" ] ] )
            , ( "js4", "<script>x < 42 || x > 42</script>", Ok [ Element "script" [] [ Text "x < 42 || x > 42" ] ] )
            , ( "comment1", "<script>\n//</script>\n</script>", Ok [ Element "script" [] [ Text "\n//</script>\n" ] ] )
            , ( "comment2", "<script>\n/*\n</script>\n*/\n</script>", Ok [ Element "script" [] [ Text "\n/*\n</script>\n*/\n" ] ] )
            ]



-- TESTS FROM hecrj/elm-html-parser


testParseAll : String -> List Node -> (() -> Expectation)
testParseAll s astList =
    \_ ->
        Expect.equal (Ok astList) (Html.Parser.run config s)


testParse : String -> Node -> (() -> Expectation)
testParse input expected =
    \_ ->
        case Html.Parser.run config input of
            Err message ->
                Expect.fail (Parser.deadEndsToString message)

            Ok actual ->
                Expect.equal (Ok actual) (Ok [ expected ])


hecrjNodeTests : Test
hecrjNodeTests =
    describe "Node"
        [ test "basic1" (testParse "<a></a>" (Element "a" [] []))
        , test "basic2" (testParse "<a></a >" (Element "a" [] []))
        , test "basic3" (testParse "<A></A >" (Element "a" [] []))
        , test "basic4" (testParseAll " <a></a> " [ Text " ", Element "a" [] [], Text " " ])
        , test "basic5" (testParseAll "a<a></a>b" [ Text "a", Element "a" [] [], Text "b" ])
        , test "basic6" (testParse "<A></A>" (Element "a" [] []))
        , test "basic7" (testParse "<a>a</a>" (Element "a" [] [ Text "a" ]))
        , test "basic8" (testParse "<a> a </a>" (Element "a" [] [ Text " a " ]))
        , test "basic10" (testParse "<br>" (Element "br" [] []))
        , test "basic11" (testParse "<a><a></a></a>" (Element "a" [] [ Element "a" [] [] ]))
        , test "basic12" (testParse "<a> <a> </a> </a>" (Element "a" [] [ Text " ", Element "a" [] [ Text " " ], Text " " ]))
        , test "basic13" (testParse "<a> <br> </a>" (Element "a" [] [ Text " ", Element "br" [] [], Text " " ]))
        , test "basic14" (testParse "<a><a></a><a></a></a>" (Element "a" [] [ Element "a" [] [], Element "a" [] [] ]))
        , test "basic15" (testParse "<a><a><a></a></a></a>" (Element "a" [] [ Element "a" [] [ Element "a" [] [] ] ]))
        , test "basic16" (testParse "<a><a></a><b></b></a>" (Element "a" [] [ Element "a" [] [], Element "b" [] [] ]))
        , test "basic17" (testParse "<h1></h1>" (Element "h1" [] []))
        , test "start-only-tag1" (testParse "<br>" (Element "br" [] []))
        , test "start-only-tag2" (testParse "<BR>" (Element "br" [] []))
        , test "start-only-tag3" (testParse "<br >" (Element "br" [] []))
        , test "start-only-tag4" (testParse "<BR >" (Element "br" [] []))
        , test "start-only-tag5" (testParse "<a> <br> </a>" (Element "a" [] [ Text " ", Element "br" [] [], Text " " ]))
        , test "start-only-tag6" (testParse "<a><br><br></a>" (Element "a" [] [ Element "br" [] [], Element "br" [] [] ]))
        , test "start-only-tag7" (testParse "<a><br><img><hr><meta></a>" (Element "a" [] [ Element "br" [] [], Element "img" [] [], Element "hr" [] [], Element "meta" [] [] ]))
        , test "start-only-tag8" (testParse "<a>foo<br>bar</a>" (Element "a" [] [ Text "foo", Element "br" [] [], Text "bar" ]))
        , test "self-closing-tag1" (testParse "<br/>" (Element "br" [] []))
        , test "self-closing-tag2" (testParse "<br />" (Element "br" [] []))
        , test "self-closing-tag3" (testParse "<link href=\"something\" rel=\"something else\"/>" (Element "link" [ ( "href", "something" ), ( "rel", "something else" ) ] []))
        , test "web-component-tag" (testParse "<a-web-component></a-web-component>" (Element "a-web-component" [] []))
        ]


hecrjAttributeTests : Test
hecrjAttributeTests =
    describe "Attribute"
        [ test "basic1" (testParse """<a href="example.com"></a>""" (Element "a" [ ( "href", "example.com" ) ] []))
        , test "basic2" (testParse """<a href='example.com'></a>""" (Element "a" [ ( "href", "example.com" ) ] []))
        , test "basic3" (testParse """<a href=example.com></a>""" (Element "a" [ ( "href", "example.com" ) ] []))
        , test "basic4" (testParse """<a HREF=example.com></a>""" (Element "a" [ ( "href", "example.com" ) ] []))
        , test "basic5" (testParse """<a href=bare></a>""" (Element "a" [ ( "href", "bare" ) ] []))
        , test "basic6" (testParse """<a href="example.com?a=b&amp;c=d"></a>""" (Element "a" [ ( "href", "example.com?a=b&c=d" ) ] []))
        , test "basic7" (testParse """<a href="example.com?a=b&c=d"></a>""" (Element "a" [ ( "href", "example.com?a=b&c=d" ) ] []))
        , test "basic8" (testParse """<input max=100 min = 10.5>""" (Element "input" [ ( "max", "100" ), ( "min", "10.5" ) ] []))
        , test "basic9" (testParse """<input disabled>""" (Element "input" [ ( "disabled", "" ) ] []))
        , test "basic10" (testParse """<input DISABLED>""" (Element "input" [ ( "disabled", "" ) ] []))
        , test "basic11" (testParse """<meta http-equiv=Content-Type>""" (Element "meta" [ ( "http-equiv", "Content-Type" ) ] []))
        , test "basic12" (testParse """<input data-foo2="a">""" (Element "input" [ ( "data-foo2", "a" ) ] []))
        , test "basic13" (testParse """<html xmlns:v="urn:schemas-microsoft-com:vml"></html>""" (Element "html" [ ( "xmlns:v", "urn:schemas-microsoft-com:vml" ) ] []))
        , test "basic14" (testParse """<link rel=stylesheet
        href="">""" (Element "link" [ ( "rel", "stylesheet" ), ( "href", "" ) ] []))

        -- Invalid attribute names shouldn't be parsed: https://github.com/elm/html/issues/46
        , test "invalid character" (testParse """<p\u{00A0} ></p>""" (Element "p" [] []))
        ]


hecrjScriptTests : Test
hecrjScriptTests =
    describe "Script"
        [ test "script1" (testParse """<script></script>""" (Element "script" [] []))
        , test "script2" (testParse """<SCRIPT></SCRIPT>""" (Element "script" [] []))
        , test "script3" (testParse """<script src="script.js">foo</script>""" (Element "script" [ ( "src", "script.js" ) ] [ Text "foo" ]))
        , test "script4" (testParse """<script>var a = 0 < 1; b = 1 > 0;</script>""" (Element "script" [] [ Text "var a = 0 < 1; b = 1 > 0;" ]))
        , test "script5" (testParse """<script><!----></script>""" (Element "script" [] [ Comment "" ]))
        , test "script6" (testParse """<script>a<!--</script><script>-->b</script>""" (Element "script" [] [ Text "a", Comment "</script><script>", Text "b" ]))
        , test "style" (testParse """<style>a<!--</style><style>-->b</style>""" (Element "style" [] [ Text "a", Comment "</style><style>", Text "b" ]))
        ]


hecrjCommentTests : Test
hecrjCommentTests =
    describe "Comment"
        [ test "basic1" (testParse """<!---->""" (Comment ""))
        , test "basic2" (testParse """<!--<div></div>-->""" (Comment "<div></div>"))
        , test "basic3" (testParse """<div><!--</div>--></div>""" (Element "div" [] [ Comment "</div>" ]))
        , test "basic4" (testParse """<!--<!---->""" (Comment "<!--"))
        , test "basic5" (testParse """<!--foo\t\u{000D}
        -->""" (Comment "foo\t\u{000D}\n        "))
        ]


svgTests =
    test "self-closing svg path"
        (testParse
            """<svg viewBox="0 0 20 20" fill="currentColor" aria-hidden="true"><path fill-rule="evenodd" d="1 2 3" clip-rule="evenodd" /></svg>"""
            (Element "svg"
                [ ( "viewbox", "0 0 20 20" )
                , ( "fill", "currentColor" )
                , ( "aria-hidden", "true" )
                ]
                [ Element "path"
                    [ ( "fill-rule", "evenodd" )
                    , ( "d", "1 2 3" )
                    , ( "clip-rule", "evenodd" )
                    ]
                    []
                ]
            )
        )



-- https://github.com/taoqf/node-html-parser/blob/main/test/tests/html.js


nodeHtmlParserTests =
    describe "taoqf/node-html-parser tests" <|
        testAll
            [ ( "test1"
              , "<p id=\"id\"><a class='cls'>Hello</a><ul><li><li></ul><span></span></p>"
              , Ok
                    [ Element "p"
                        [ ( "id", "id" ) ]
                        [ Element "a"
                            [ ( "class", "cls" ) ]
                            [ Text "Hello"
                            ]
                        , Element "ul"
                            []
                            [ Element "li" [] []
                            , Element "li" [] []
                            ]
                        , Element "span" [] []
                        ]
                    ]
              )
            , ( "test2"
              , "<DIV><a><img/></A><p></P></div>"
              , Ok
                    [ Element "div"
                        []
                        [ Element "a"
                            []
                            [ Element "img" [] []
                            ]
                        , Element "p" [] []
                        ]
                    ]
              )
            , ( "test3"
              , "<div><a><img/></a><p></p></div>"
              , Ok
                    [ Element "div"
                        []
                        [ Element "a"
                            []
                            [ Element "img" [] []
                            ]
                        , Element "p" [] []
                        ]
                    ]
              )
            , ( "test4"
              , "<div><a><!-- my comment --></a></div>"
              , Ok
                    [ Element "div"
                        []
                        [ Element "a"
                            []
                            [ Comment " my comment "
                            ]
                        ]
                    ]
              )
            , ( "test5"
              , "<div><!--<a></a>--></div>"
              , Ok
                    [ Element "div"
                        []
                        [ Comment "<a></a>"
                        ]
                    ]
              )
            , ( "test6"
              , "<picture><source srcset=\"/images/example-1.jpg 1200w, /images/example-2.jpg 1600w\" sizes=\"100vw\"><img src=\"/images/example.jpg\" alt=\"Example\"/></picture>"
              , Ok
                    [ Element "picture"
                        []
                        [ Element "source"
                            [ ( "srcset", "/images/example-1.jpg 1200w, /images/example-2.jpg 1600w" )
                            , ( "sizes", "100vw" )
                            ]
                            []
                        , Element "img" [ ( "src", "/images/example.jpg" ), ( "alt", "Example" ) ] []
                        ]
                    ]
              )
            , ( "test7"
              , "<script>1</script><style>2&amp;</style>"
              , Ok
                    [ Element "script" [] [ Text "1" ]
                    , Element "style" [] [ Text "2&" ]
                    ]
              )
            ]



-- JSOUP TESTS
-- https://github.com/jhy/jsoup/blob/master/src/test/java/org/jsoup/parser/AttributeParseTest.java


jsoupAttributeTests =
    describe "(from jsoup) attributes" <|
        testAll
            [ ( "parses rough attribute string"
              , "<a id=\"123\" class=\"baz = 'bar'\" style = 'border: 2px'qux zim foo = 12 mux=18 />"
              , Ok
                    [ Element "a"
                        [ ( "id", "123" )
                        , ( "class", "baz = 'bar'" )
                        , ( "style", "border: 2px" )
                        , ( "qux", "" )
                        , ( "zim", "" )
                        , ( "foo", "12" )
                        , ( "mux", "18" )
                        ]
                        []
                    ]
              )
            , ( "handles newlines and returns"
              , --   "<a\r\nfoo='bar\r\nqux'\r\nbar\r\n=\r\ntwo>One</a>"
                "<a\u{000D}\nfoo='bar\u{000D}\nqux'\u{000D}\nbar\u{000D}\n=\u{000D}\ntwo>One</a>"
              , Ok
                    [ Element "a"
                        [ ( "foo", "bar\u{000D}\nqux" )
                        , ( "bar", "two" )
                        ]
                        [ Text "One" ]
                    ]
              )
            , ( "parses empty string", "<a />", Ok [ Element "a" [] [] ] )

            -- https://html.spec.whatwg.org/multipage/parsing.html#before-attribute-name-state
            , ( "can start with '='"
              , "<a =empty />"
              , Ok [ Element "a" [ ( "=empty", "" ) ] [] ]
              )
            , ( "strict attribute unescapes"
              , -- "<a id=1 href='?foo=bar&mid&lt=true'>One</a> <a id=2 href='?foo=bar&lt;qux&lg=1'>Two</a>"
                "<a id=1 href='?foo=bar&mid&lt=true'>One</a> <a id=2 href='?foo=bar&lt;qux&lg=1'>Two</a>"
              , Ok
                    [ Element "a"
                        [ ( "id", "1" )
                        , ( "href", "?foo=bar&mid&lt=true" )
                        ]
                        [ Text "One" ]
                    , Text " "
                    , Element "a"
                        [ ( "id", "2" )
                        , ( "href", "?foo=bar<qux&lg=1" )
                        ]
                        [ Text "Two" ]
                    ]
              )
            , ( "more attribute unescapes"
              , "<a href='&wr_id=123&mid-size=true&ok=&wr'>Check</a>"
              , Ok
                    [ Element "a"
                        [ ( "href", "&wr_id=123&mid-size=true&ok=&wr" )
                        ]
                        [ Text "Check" ]
                    ]
              )
            , ( "drops slash from attribute"
              , "<img /onerror='doMyJob' /a /=b/>"
              , Ok
                    [ Element "img"
                        [ ( "onerror", "doMyJob" )
                        , ( "a", "" )
                        , ( "=b", "" )
                        ]
                        []
                    ]
              )
            ]



-- TODO: https://github.com/jhy/jsoup/blob/master/src/test/java/org/jsoup/parser/HtmlParserTest.java
