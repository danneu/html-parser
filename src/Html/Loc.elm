module Html.Loc exposing
    ( Loc, toLoc, toNode
    , goUp, goDown, goLeft, goRight, goNext
    , select, selectAll
    , any, tagEq, idEq, classEq, attrEq, attrContains, attrEndsWith, attrStartsWith, hasAttr, or, and, not
    , directChild, anyChild, directSibling, anySibling, lastChild
    , Selector, walk
    )

{-| EXPERIMENTAL!!! This module provides functions for traversing, querying, and updating the html tree.

This is implemented by turning the `Html.Parser.Node` tree into a zipper data-structure: `Html.Parser.Loc`.


# Definition

@docs Loc, toLoc, toNode


# Traversal

@docs goUp, goDown, goLeft, goRight, goNext


# Query

@docs select, selectAll


# Basic selectors

@docs any, tagEq, idEq, classEq, attrEq, attrContains, attrEndsWith, attrStartsWith, hasAttr, or, and, not


# Advanced selectors

@docs directChild, anyChild, directSibling, anySibling, lastChild

-}

import Html.Parser exposing (Node(..))


type Path
    = Top
    | PNode String (List ( String, String )) (List Node) Path (List Node)


{-| A location represents the path into a tree.

From a location, you can get the html node at that location or traverse into
the node at direction up, down, left, or right. All of the query functions
take a location as an argument.

All of the functions in this module operator on a location.

-}
type Loc
    = Loc Node Path


{-| Convert a node into a location for further querying.

The location starts off at the tree's root.

    Html.Parser.runElement "<html><div>foo</div></html>"
        |> Maybe.map toLoc

-}
toLoc : Node -> Loc
toLoc root =
    Loc root Top


{-| Get the node at the given location.

    (Html.Parser.runElement "<html><div>foo</div></html>"
        |> Maybe.map toLoc
        |> Maybe.map goDown
        |> Maybe.map goDown
        |> Maybe.map toNode
    )
        == Just (Text "foo")

-}
toNode : Loc -> Node
toNode (Loc node _) =
    node


{-| Move down into the node's first child.
-}
goDown : Loc -> Maybe Loc
goDown (Loc t p) =
    case t of
        Text _ ->
            Nothing

        Comment _ ->
            Nothing

        Element _ _ [] ->
            Nothing

        Element tag attrs (t1 :: trees) ->
            Just (Loc t1 (PNode tag attrs [] p trees))


{-| Move into the node's next sibling.
-}
goRight : Loc -> Maybe Loc
goRight (Loc t p) =
    case p of
        Top ->
            Nothing

        PNode _ _ _ _ [] ->
            Nothing

        PNode tag attrs left up (r :: right) ->
            Just (Loc r (PNode tag attrs (t :: left) up right))


{-| Move into the node's previous sibling.
-}
goLeft : Loc -> Maybe Loc
goLeft (Loc t p) =
    case p of
        Top ->
            Nothing

        PNode _ _ [] _ _ ->
            Nothing

        PNode tag attrs (l :: left) up right ->
            Just (Loc l (PNode tag attrs left up (t :: right)))


{-| Move into the node's parent.
-}
goUp : Loc -> Maybe Loc
goUp (Loc t p) =
    case p of
        Top ->
            Nothing

        PNode tag attrs left up right ->
            Just (Loc (Element tag attrs (List.reverse left ++ (t :: right))) up)


{-| Move into the node at the next available depth-first position.

This function lets us do a depth-first walk of all nodes by repeatedly calling `goNext` on
nodes until nothing is returned.

Depth-first walk can be summarized:

  - If the node has a child, then return the child.
  - If there is no child, then return the next sibling.
  - If there is no sibling, then move up into each ancestor until an ancestor has a next sibling.

-}
goNext : Loc -> Maybe Loc
goNext loc =
    case goDown loc of
        Just next ->
            Just next

        Nothing ->
            case goRight loc of
                Just next ->
                    Just next

                Nothing ->
                    -- Go up until up->right works
                    let
                        help loc_ =
                            goUp loc_
                                |> Maybe.andThen
                                    (\up ->
                                        case goRight up of
                                            Nothing ->
                                                help up

                                            Just upRight ->
                                                Just upRight
                                    )
                    in
                    help loc



-- SEARCH


type alias Selector =
    Loc -> Bool


selectLocHelp : Selector -> Maybe Loc -> Maybe Loc
selectLocHelp selector maybeLoc =
    case maybeLoc of
        Nothing ->
            Nothing

        Just loc ->
            if selector loc then
                Just loc

            else
                selectLocHelp selector (goNext loc)


{-| Select the first loc that matches selector.
-}
selectLoc : Selector -> Loc -> Maybe Loc
selectLoc selector loc =
    selectLocHelp selector (Just loc)


{-| Return the first node (depth-first) that matches the selector.

Stops traversing once it finds a match.

-}
select : Selector -> Loc -> Maybe Node
select selector loc =
    selectLoc selector loc
        |> Maybe.map toNode


{-| Return a list of all locs (depth-first) that match the selector.
-}
selectLocs : (Loc -> Bool) -> Loc -> List Loc
selectLocs selector loc =
    let
        help : Maybe Loc -> List Loc -> List Loc
        help maybeLoc acc =
            case maybeLoc of
                Nothing ->
                    List.reverse acc

                Just loc_ ->
                    if selector loc_ then
                        help (goNext loc_) (loc_ :: acc)

                    else
                        help (goNext loc_) acc
    in
    help (Just loc) []


{-| Return a list of all nodes (depth-first) that match the selector.
-}
selectAll : (Loc -> Bool) -> Loc -> List Node
selectAll selector loc =
    selectLocs selector loc
        |> List.map toNode



-- SELECTORS
--
-- A selector is a function that takes a Loc and returns Bool


{-| Returns true if node has an attribute of the given name.

    hasAttr "title"
        == CSS selector "[title]"

    and [ tag "a", hasAttr "title" ]
        == CSS selector "a[title]"

-}
hasAttr : String -> (Loc -> Bool)
hasAttr key =
    \loc ->
        case toNode loc of
            Element _ attrs _ ->
                List.any (\( k, _ ) -> String.toLower k == String.toLower key) attrs

            _ ->
                False


{-| Returns true if node has given id.

    id "foo"
        == CSS selector "#foo"

    and [ tag "div", id "foo" ]
        == CSS selector "div#foo"

-}
idEq : String -> (Loc -> Bool)
idEq key =
    attrEq "id" key


{-| splitClasses " aaa bbb ccc " == ["aaa", "bbb", "ccc"]

Avoiding regex dependency for now.

Could use regex:

    nonwhitespace =
        Regex.fromString "\\S+" |> Maybe.withDefault Regex.never

    Regex.find nonwhitespace input
    |> List.map .match

-}
splitClasses : String -> List String
splitClasses input =
    String.split " " input
        |> List.filter (Basics.not << String.isEmpty)


{-| Returns true if node has a given class.

    class "warning"
        == CSS selector ".warning"

-}
classEq : String -> (Loc -> Bool)
classEq target_ =
    \loc ->
        let
            target =
                String.toLower target_
        in
        case toNode loc of
            Element _ attrs _ ->
                let
                    help : List ( String, String ) -> Bool
                    help subattrs =
                        case subattrs of
                            [] ->
                                False

                            ( "class", v ) :: rest ->
                                if List.any (\cls -> cls == target) (splitClasses (String.toLower v)) then
                                    True

                                else
                                    help rest

                            _ :: rest ->
                                help rest
                in
                help attrs

            _ ->
                False


{-| Matches all nodes.

Equivalent to the "\*" CSS selector.

    directChild (tag "div") any
        == CSS selector "div > *"

-}
any : Loc -> Bool
any =
    \_ -> True


{-| Returns true if node is an element of a given tag.

    tag "div"
        == CSS selector "div"

    or [ tag "h1", tag "h2" ]
        == CSS selector "h1,h2"

-}
tagEq : String -> (Loc -> Bool)
tagEq tag =
    \loc ->
        case toNode loc of
            Element t _ _ ->
                t == tag

            _ ->
                False


{-| Returns true if node has attribute of given value.

    attr "type" "text"
        == CSS selector "[type=\"text\"]"

    and [ tag "input", attr "type" "text" ]
        == CSS selector "input[type=\"text\""]"

Note: This function only does a simple (==) comparison to the attribute value, so `attr "class" "foo"` does not match `<div class="foo bar"/>`.

Use the more intelligent `class` function for class matching.

-}
attrEq : String -> String -> (Loc -> Bool)
attrEq key val =
    -- TODO: Do I need to downcase attr keys?
    \loc ->
        case toNode loc of
            Element _ allAttrs _ ->
                let
                    help : List ( String, String ) -> Bool
                    help attrs =
                        case attrs of
                            [] ->
                                False

                            ( k, v ) :: rest ->
                                if k == key && v == val then
                                    True

                                else
                                    help rest
                in
                help allAttrs

            _ ->
                False


getAttrsByKey : String -> Node -> List ( String, String )
getAttrsByKey key node =
    case node of
        Element _ attrs _ ->
            List.filter (\( k, _ ) -> k == key) attrs

        _ ->
            []


attrContains : String -> String -> (Loc -> Bool)
attrContains key substring =
    \loc ->
        getAttrsByKey key (toNode loc)
            |> List.any (\( _, v ) -> String.contains substring v)


attrStartsWith : String -> String -> (Loc -> Bool)
attrStartsWith key prefix =
    \loc ->
        getAttrsByKey key (toNode loc)
            |> List.any (\( _, v ) -> String.startsWith prefix v)


attrEndsWith : String -> String -> (Loc -> Bool)
attrEndsWith key suffix =
    \loc ->
        getAttrsByKey key (toNode loc)
            |> List.any (\( _, v ) -> String.endsWith suffix v)


{-| Returns true if the node and its immediate left-sibling match their relative selectors.

    directSibling (tag "h1") (tag "h2")
        == CSS selector "h1 + h2"

-}
directSibling : (Loc -> Bool) -> (Loc -> Bool) -> (Loc -> Bool)
directSibling leftSelector rightSelector =
    \loc ->
        if rightSelector loc then
            case goLeft loc of
                Nothing ->
                    False

                Just left ->
                    leftSelector left

        else
            False


{-| Returns true if the node matches a selector AND its immediate parent matches a selector.

    directChild (tag "h1") any
        == CSS selector "h1 > *"

    directChild (tag "h1") (and [ any, class "warning" ])
        == CSS selector "h1 > *.warning"

-}
directChild : (Loc -> Bool) -> (Loc -> Bool) -> (Loc -> Bool)
directChild parentSelector childSelector =
    \loc ->
        if childSelector loc then
            case goUp loc of
                Nothing ->
                    False

                Just parentLoc ->
                    parentSelector parentLoc

        else
            False


{-| Return true if the node matches a selector and ANY of its ancestors match a selector.

    anyChild (tag "h1") (class "foo")
        == CSS selector "h1 .foo"

    anyChild (tag "h1") (and [ any, class "warning" ])
        == CSS selector "h1 *.warning"

-}
anyChild : (Loc -> Bool) -> (Loc -> Bool) -> (Loc -> Bool)
anyChild parentSelector childSelector =
    let
        help loc_ =
            case goUp loc_ of
                Nothing ->
                    False

                Just up ->
                    if parentSelector up then
                        True

                    else
                        help up
    in
    \loc ->
        if childSelector loc then
            help loc

        else
            False


{-| Returns true `rightSelector` matches the node and `leftSelector` matches _any_ of the node's left siblings.

Equivalent to "~" in CSS.

    anySibling (tag "p") (tag "ul")
        == CSS selector "p ~ ul"

-}
anySibling : (Loc -> Bool) -> (Loc -> Bool) -> (Loc -> Bool)
anySibling leftSelector rightSelector =
    let
        help loc_ =
            case goLeft loc_ of
                Nothing ->
                    False

                Just left ->
                    if leftSelector left then
                        True

                    else
                        help left
    in
    \loc ->
        if rightSelector loc then
            help loc

        else
            False


{-| Returns true if any selectors return true.

Equvalent to the comma in CSS selectors.

    or [ tag "a", tag "b" ]
        == CSS selector "a, b"

-}
or : List (Loc -> Bool) -> (Loc -> Bool)
or selectors =
    \loc ->
        List.any (\selector -> selector loc) selectors


{-| Returns true if all selectors return true.

    and [ tag "input", class "success", hasAttr "disabled" ]
        == CSS selector "input.success[disabled]"

-}
and : List (Loc -> Bool) -> (Loc -> Bool)
and selectors =
    \loc ->
        List.all (\selector -> selector loc) selectors


{-| Matches nodes that do not match the given selector.

    and [ tag "div", not (class "main") ]
        == CSS selector "div:not(.main)"

-}
not : (Loc -> Bool) -> (Loc -> Bool)
not selector =
    \loc ->
        Basics.not (selector loc)


{-| Returns true if node is the last child of its parent that matches the selector.

    lastChild (tag "p")
        == CSS selector "p:last-child"

    lastChild (and [ tag "p", class "foo" ])
        == Impossible in CSS?

    TODO: Should I follow the CSS selectors (lastChild "p") or should I make my
    selectors more powerful (lastChild selector)? I think it may be confusing if my
    selectors are too different from their CSS counterparts if I'm going to reuse the same names.

-}
lastChild : (Loc -> Bool) -> (Loc -> Bool)
lastChild selector =
    \loc ->
        if selector loc then
            let
                help loc_ =
                    case goRight loc_ of
                        Nothing ->
                            True

                        Just right ->
                            if selector right then
                                False

                            else
                                help right
            in
            help loc

        else
            False



-- UPDATING


insertRight : Node -> Loc -> Maybe Loc
insertRight newNode (Loc t p) =
    case p of
        Top ->
            Nothing

        PNode tag attrs left up right ->
            Just (Loc t (PNode tag attrs left up (newNode :: right)))


insertLeft : Node -> Loc -> Maybe Loc
insertLeft newNode (Loc t p) =
    case p of
        Top ->
            Nothing

        PNode tag attrs left up right ->
            Just (Loc t (PNode tag attrs (newNode :: left) up right))


insertDown : Node -> Loc -> Maybe Loc
insertDown newNode (Loc t p) =
    case t of
        Text _ ->
            Nothing

        Comment _ ->
            Nothing

        Element tag attrs kids ->
            Just (Loc newNode (PNode tag attrs [] p kids))


{-| Delete the node at given location.

The new selected node will be the right sibling, if possible,
else the left sibling, else the parent node.

-}
delete : Loc -> Maybe Loc
delete (Loc _ p) =
    case p of
        Top ->
            Nothing

        PNode tag attrs left up (r :: right) ->
            Just (Loc r (PNode tag attrs left up right))

        PNode tag attrs (l :: left) up [] ->
            Just (Loc l (PNode tag attrs left up []))

        PNode tag attrs [] up [] ->
            -- Parent has no more children
            Just (Loc (Element tag attrs []) up)


{-| Is there another solution?
-}
goRoot : Loc -> Loc
goRoot loc =
    case goUp loc of
        Nothing ->
            loc

        Just parent ->
            goRoot parent


update : (Node -> Node) -> Loc -> Loc
update xform (Loc n p) =
    Loc (xform n) p


{-| -}
walk : (Loc -> Bool) -> (Node -> Node) -> Loc -> Loc
walk selector xform loc =
    let
        newLoc =
            if selector loc then
                update xform loc

            else
                loc
    in
    case goNext newLoc of
        Nothing ->
            goRoot newLoc

        Just next ->
            walk selector xform next
