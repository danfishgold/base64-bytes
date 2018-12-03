module Main exposing (main)

import Base64
import Browser exposing (application)
import Browser.Navigation as Nav
import Bytes
import Bytes.Decode
import Bytes.Encode
import Html exposing (text, textarea)
import Html.Events exposing (onInput)
import Url exposing (Url)


encodeUrl : String -> String
encodeUrl text =
    Bytes.Encode.string text
        |> Bytes.Encode.encode
        |> Base64.fromBytes
        |> Maybe.withDefault []
        |> String.fromList


decodeUrl : String -> String
decodeUrl url =
    url
        |> String.toList
        |> Base64.toBytes
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
    ( { key = key
      , urlPath = url.path
      , text =
            url.fragment
                |> Maybe.map decodeUrl
                |> Maybe.withDefault ""
      }
    , Cmd.none
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


updateUrl : Nav.Key -> String -> String -> Cmd Msg
updateUrl key path text =
    Nav.replaceUrl key (path ++ "#" ++ encodeUrl text)


view : Model -> Browser.Document Msg
view model =
    { title = "B64 Url"
    , body = [ textarea [ onInput SetText ] [ text model.text ] ]
    }
