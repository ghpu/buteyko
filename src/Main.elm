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
import Html.Events.Extra.Pointer as Pointer


type alias Model =
    { round : Int
    , scene : ( Int, Int )
    , exercise : ExerciseState
    }


type ExerciseState
    = Preparation
    | BreathHolding
    | Recovery ( Maybe Time.Posix, Maybe Time.Posix )


type Msg
    = Frame Time.Posix
    | GotViewport Browser.Dom.Viewport
    | Resize Int Int
    | Press
    | Release


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

        ( Frame time, Recovery ( os, ot ) ) ->
            let
                ( s, t ) =
                    case ( os, ot ) of
                        ( Nothing, Nothing ) ->
                            ( Just time, Nothing )

                        ( start, _ ) ->
                            ( start, Just time )

                duration =
                    case ( s, t ) of
                        ( Just ss, Just tt ) ->
                            round (Duration.inMilliseconds (Duration.from ss tt))

                        _ ->
                            0
            in
                if duration > 30000 then
                    ( { model | exercise = Preparation, round = model.round + 1 }, Cmd.none )
                else
                    ( { model | exercise = (Recovery ( s, t )) }, Cmd.none )

        ( Press, Preparation ) ->
            ( { model | exercise = BreathHolding }, Cmd.none )

        ( Release, BreathHolding ) ->
            ( { model | exercise = Recovery ( Nothing, Nothing ) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        ( first, second, third ) =
            case model.exercise of
                Preparation ->
                    ( "active", "inactive", "inactive" )

                BreathHolding ->
                    ( "inactive", "active", "inactive" )

                Recovery _ ->
                    ( "inactive", "inactive", "active" )
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
            , progress model
            , Html.button
                [ Pointer.onDown (\event -> Press), Pointer.onUp (\event -> Release) ]
                [ Html.text "Press and hold" ]
            , Html.text ("Number of repetitions done : " ++ (String.fromInt model.round) ++ "/6")
            ]


progress model =
    let
        width =
            600

        duration =
            case model.exercise of
                Recovery ( Just s, Just t ) ->
                    round (Duration.inMilliseconds (Duration.from s t)) // 1000 * width // 60

                _ ->
                    0
    in
        Svg.svg
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "left" "0"
            , Html.Attributes.style "top" "400"
            , Svg.Attributes.width "400"
            , Svg.Attributes.height "32"
            , Svg.Attributes.viewBox "0 0 400 32"
            ]
            [ Svg.rect [ Svg.Attributes.x "0", Svg.Attributes.y "0", Svg.Attributes.width (String.fromInt width), Svg.Attributes.height "32", Svg.Attributes.rx "16", Svg.Attributes.ry "16", Svg.Attributes.fill "red", Svg.Attributes.fillOpacity "20%" ] []
            , Svg.rect [ Svg.Attributes.x "0", Svg.Attributes.y "0", Svg.Attributes.width (String.fromInt duration), Svg.Attributes.height "32", Svg.Attributes.rx "16", Svg.Attributes.ry "16", Svg.Attributes.fill "red", Svg.Attributes.fillOpacity "50%" ] []
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
