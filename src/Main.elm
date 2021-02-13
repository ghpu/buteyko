module Main exposing (..)

import Browser
import Duration
import Html exposing (Html)
import Html.Events
import Html.Attributes
import Json.Decode
import Json.Encode
import List.Nonempty exposing (Nonempty(..))
import Task
import Time
import Browser.Dom
import Browser.Events
import Svg
import Svg.Attributes


type alias Model =
    { round : Int
    , scene : ( Int, Int )
    , exercise : ExerciseState
    }


type ExerciseState
    = Preparation
    | BreathHolding
    | Recovery Time.Posix Time.Posix


type Msg
    = Frame Time.Posix
    | GotViewport Browser.Dom.Viewport
    | Resize Int Int
    | Press


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Model 0 ( 320, 240 ) Preparation
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.exercise ) of
        ( GotViewport vp, _ ) ->
            ( { model | scene = ( round vp.scene.width, round vp.scene.height ) }, Cmd.none )

        ( Resize width height, _ ) ->
            ( { model | scene = ( width, height ) }, Cmd.none )

        ( Frame time, Recovery start last ) ->
            let
                duration =
                    round (Duration.inMilliseconds (Duration.from start time))
            in
                ( { model | exercise = (Recovery start time) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        ( first, second, third ) =
            case model.exercise of
                _ ->
                    ( "active", "inactive", "inactive" )
    in
        Html.div []
            [ Html.text "Instructions"
            , Html.ol []
                [ Html.li [] [ Html.text "Close your mouth" ]
                , Html.li [ Html.Attributes.class first ] [ Html.text "Breathe slowly and lightly through the nose." ]
                , Html.li [ Html.Attributes.class first ] [ Html.text "Breathe in." ]
                , Html.li [ Html.Attributes.class first ] [ Html.text " Breathe out then hold your breath and press and hold the button." ]
                , Html.li [ Html.Attributes.class second ] [ Html.text "Nod or sway the head lot of times." ]
                , Html.li [ Html.Attributes.class second ] [ Html.text "When you are out of breath, release the button" ]
                , Html.li [ Html.Attributes.class third ] [ Html.text "Take 30s-1m to recover by breathing slowly and lightly through your nose." ]
                ]
            , Html.text "Repeat the exercise 6 times."
            , Html.button
                [ Html.Events.onClick Press ]
                [ Html.text "Press and hold" ]
            , Html.text ("Number of repetitions done : " ++ (String.fromInt model.round) ++ "/6")
            ]


progress model =
    Svg.svg
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "400"
        , Svg.Attributes.width "400"
        , Svg.Attributes.height "32"
        , Svg.Attributes.viewBox "0 0 400 32"
        ]
        [ Svg.rect [ Svg.Attributes.x "0", Svg.Attributes.y "0", Svg.Attributes.width "400", Svg.Attributes.height "32", Svg.Attributes.rx "16", Svg.Attributes.ry "16", Svg.Attributes.fill "white", Svg.Attributes.fillOpacity "20%" ] []
        , Svg.rect [ Svg.Attributes.x "0", Svg.Attributes.y "0", Svg.Attributes.width (String.fromInt 200), Svg.Attributes.height "32", Svg.Attributes.rx "16", Svg.Attributes.ry "16", Svg.Attributes.fill "white", Svg.Attributes.fillOpacity "50%" ] []
        ]


subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrame Frame
        , Browser.Events.onResize Resize
        ]


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
