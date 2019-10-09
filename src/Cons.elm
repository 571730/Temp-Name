port module Cons exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode as Json

main : Program (Maybe Model) Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Consultant Timer", body = [view model] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }

port setStorage : Model -> Cmd msg

updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage newModel, cmds ]
        )

-- MODEL

type alias Model =
    { entries : List Entry
    , field : String
    , uid : Int
    }

type alias Entry =
    { name : String
    , id : Int
    }

emptyModel : Model
emptyModel =
    { entries = []
    , field = ""
    , uid = 0
    }

newEntry : String -> Int -> Entry
newEntry name id =
    { name = name
    , id = id
    }


init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
    ( Maybe.withDefault emptyModel maybeModel
    , Cmd.none
    )

-- UPDATE

type Msg
    = NoOp
    | UpdateField String
    | Add
    | Delete Int
    | DeleteComplete

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateField str ->
            ( { model | field = str }
            , Cmd.none
            )

        Add ->
            ( { model
                | uid = model.uid + 1
                , field = ""
                , entries =
                    if String.isEmpty model.field then
                        model.entries
                    else
                        model.entries ++ [ newEntry model.field model.uid ]
                }
            , Cmd.none
            )
        Delete id ->
            ( { model | entries = List.filter (\t -> t.id /= id ) model.entries }
            , Cmd.none
            )

        DeleteComplete ->
            ( { model | entries = [] }
            , Cmd.none
            )


-- VIEW

view : Model -> Html Msg
view model =
    div
        [ class "outer-container" ]
        [ section
            [ class "ConsultantApp" ]
            [ viewInput model.field
            , viewEntries model.entries
            ]
        ]


viewInput : String -> Html Msg
viewInput project =
    header
        [ class "header" ]
        [ h1 [] [text "Projects"]
        , input
            [ class "newproject"
            , placeholder "Add a project"
            , autofocus True
            , value project
            , name "newProject"
            , onInput UpdateField
            , onEnter Add
            ]
            []
        ]

onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
            Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)


-- VIEW ALL ENTRIES

viewEntries : List Entry -> Html Msg
viewEntries entries =
    section
        [ class "main"
        ]
        [ Keyed.ul [ class "project-list" ] <|
            List.map viewKeyedEntry (entries)
        ]


viewKeyedEntry : Entry -> ( String, Html Msg )
viewKeyedEntry project =
    ( String.fromInt project.id, viewEntry project )


viewEntry : Entry -> Html Msg
viewEntry project =
    li
        []
        [ div
            [ class "view" ]
            [ p
                [ class "toggle"
                ]
                [ text project.name ]
            , button
                [ class "destroy"
                , onClick (Delete project.id)
                ]
                []
            ]
        ]


