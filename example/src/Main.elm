module Main exposing (main)

import Base64
import Browser exposing (application)
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Bytes
import Bytes.Decode
import Bytes.Encode
import Html exposing (a, div, h1, p, text, textarea)
import Html.Attributes exposing (href, id, style)
import Html.Events exposing (onInput)
import Task
import Url exposing (Url)


encodeUrl : String -> String
encodeUrl text =
    Bytes.Encode.string text
        |> Bytes.Encode.encode
        |> Base64.fromBytes
        |> Maybe.withDefault ""


decodeUrl : String -> String
decodeUrl url =
    Base64.toBytes url
        |> Maybe.andThen
            (\bytes ->
                Bytes.Decode.decode
                    (Bytes.Decode.string (Bytes.width bytes))
                    bytes
            )
        |> Maybe.withDefault ""


type alias Model =
    { key : Nav.Key
    , urlPath : String
    , text : String
    }


type Msg
    = SetText String
    | RequestUrl Browser.UrlRequest
    | SetUrl Url
    | NoOp


main =
    application
        { init = init
        , subscriptions = always Sub.none
        , update = update
        , onUrlChange = SetUrl
        , onUrlRequest = RequestUrl
        , view = view
        }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    let
        text =
            url.fragment
                |> Maybe.map decodeUrl
                |> Maybe.withDefault ""
    in
    ( { key = key
      , urlPath = url.path
      , text = text
      }
    , Cmd.batch
        [ Task.attempt (always NoOp) (Dom.focus "textarea")
        , updateUrl key url.path text
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetText newText ->
            ( { model | text = newText }, updateUrl model.key model.urlPath newText )

        RequestUrl (Browser.External url) ->
            ( model, Nav.load url )

        RequestUrl (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key <| Url.toString url )

        SetUrl url ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateUrl : Nav.Key -> String -> String -> Cmd Msg
updateUrl key path text =
    Nav.replaceUrl key (path ++ "#" ++ encodeUrl text)


view : Model -> Browser.Document Msg
view model =
    { title = "B64 Url"
    , body =
        [ div
            [ style "font-family" "Helvetica, Arial, sans-serif"
            , style "padding" "1vw 5vw 3vw 5vw"
            ]
            [ h1 [] [ text "danfishgold/base64-bytes Demo" ]
            , p []
                [ a [ href "https://github.com/danfishgold/base64-bytes/blob/master/example/src/Main.elm" ]
                    [ text "View demo code on GitHub" ]
                ]
            , p []
                [ a [ href "https://package.elm-lang.org/packages/danfishgold/base64-bytes/latest" ]
                    [ text "View package on package.elm-lang.org" ]
                ]
            , p
                []
                [ text "Enter text below and it will be encoded into "
                , a [ href "https://en.wikipedia.org/wiki/Base64" ] [ text "Base64" ]
                , text " in the url. "
                , text "Load the same url in a new tab and the encoded text will be "
                , text "decoded back into the text field."
                ]
            ]
        , textarea
            [ id "textarea"
            , onInput SetText
            , style "width" "70vw"
            , style "height" "50vh"
            , style "font-family" "Helvetica, Arial, sans-serif"
            , style "font-size" "1em"
            , style "padding" "5px"
            , style "background" "#eeeeee"
            , style "border" "none"
            , style "display" "block"
            , style "margin-left" "auto"
            , style "margin-right" "auto"
            ]
            [ text model.text ]
        ]
    }
