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
    , lang : String
    }


type ExerciseState
    = Preparation
    | BreathHolding
    | Recovery ( Maybe Time.Posix, Maybe Time.Posix )
    | LongRecovery ( Maybe Time.Posix, Maybe Time.Posix )


type Msg
    = Frame Time.Posix
    | GotViewport Browser.Dom.Viewport
    | Resize Int Int
    | Press
    | Release


init : String -> ( Model, Cmd Msg )
init lang =
    ( Model 0 ( 320, 240 ) Preparation lang
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
                    ( { model | exercise = LongRecovery ( s, t ), round = model.round + 1 }, Cmd.none )
                else
                    ( { model | exercise = Recovery ( s, t ) }, Cmd.none )

        ( Frame time, LongRecovery ( Just s, Just t ) ) ->
            let
                duration =
                    round (Duration.inMilliseconds (Duration.from s time))
            in
                if duration > 60000 then
                    ( { model | exercise = Preparation }, Cmd.none )
                else
                    ( { model | exercise = LongRecovery ( Just s, Just time ) }, Cmd.none )

        ( Press, Preparation ) ->
            ( { model | exercise = BreathHolding }, Cmd.none )

        ( Press, LongRecovery _ ) ->
            ( { model | exercise = BreathHolding }, Cmd.none )

        ( Release, BreathHolding ) ->
            ( { model | exercise = Recovery ( Nothing, Nothing ) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view_fr : Model -> Html Msg
view_fr model =
    let
        ( first, second, third ) =
            case model.exercise of
                Preparation ->
                    ( "active", "inactive", "inactive" )

                BreathHolding ->
                    ( "inactive", "active", "inactive" )

                Recovery _ ->
                    ( "inactive", "inactive", "active" )

                LongRecovery _ ->
                    ( "active", "inactive", "inactive" )
    in
        Html.div []
            [ Html.text "Instructions"
            , Html.ol []
                [ Html.li [] [ Html.text "Fermez la bouche" ]
                , Html.li [ Html.Attributes.class first ] [ Html.text "Respirez lentement, légèrement et profondément par le nez" ]
                , Html.li [ Html.Attributes.class first ] [ Html.text "Inspirez" ]
                , Html.li [ Html.Attributes.class first ] [ Html.text " Expirez puis retenez votre souffle, enfoncer et maintenez le bouton" ]
                , Html.li [ Html.Attributes.class second ] [ Html.text "Incliner lentement la tête de haut en bas, de droite à gauche le plus de fois possible" ]
                , Html.li [ Html.Attributes.class second ] [ Html.text "Lorsque vous n'en pouvez plus, relâchez le bouton et respirez par le nez" ]
                , Html.li [ Html.Attributes.class third ] [ Html.text "Continuez à respirer lentement par le nez, et prenez de 30 secondes à 1 minute pour récupérer." ]
                ]
            , Html.text "Répétez l'exercice 6 fois."
            , Html.br [] []
            , Html.button
                [ Pointer.onDown (\event -> Press), Pointer.onUp (\event -> Release) ]
                [ Html.text "Appuyez et maintenez" ]
            , Html.br [] []
            , progress model "fr"
            , Html.br [] []
            , Html.text ("Nombre de répétitions : " ++ (String.fromInt model.round) ++ "/ 6")
            ]


view model =
    case model.lang of
        "fr" ->
            view_fr model

        _ ->
            view_en model


view_en : Model -> Html Msg
view_en model =
    let
        ( first, second, third ) =
            case model.exercise of
                Preparation ->
                    ( "active", "inactive", "inactive" )

                BreathHolding ->
                    ( "inactive", "active", "inactive" )

                Recovery _ ->
                    ( "inactive", "inactive", "active" )

                LongRecovery _ ->
                    ( "active", "inactive", "active" )
    in
        Html.div []
            [ Html.text "Instructions"
            , Html.ol []
                [ Html.li [] [ Html.text "Close your mouth" ]
                , Html.li [ Html.Attributes.class first ] [ Html.text "Breathe slowly and lightly through the nose" ]
                , Html.li [ Html.Attributes.class first ] [ Html.text "Breathe in" ]
                , Html.li [ Html.Attributes.class first ] [ Html.text " Breathe out then hold your breath and press and hold the button" ]
                , Html.li [ Html.Attributes.class second ] [ Html.text "Nod or sway the head lot of times" ]
                , Html.li [ Html.Attributes.class second ] [ Html.text "When you are out of breath, release the button" ]
                , Html.li [ Html.Attributes.class third ] [ Html.text "Take 30s-1m to recover by breathing slowly and lightly through your nose" ]
                ]
            , Html.text "Repeat the exercise 6 times."
            , Html.br [] []
            , Html.button
                [ Pointer.onDown (\event -> Press), Pointer.onUp (\event -> Release) ]
                [ Html.text "Press and hold" ]
            , Html.br [] []
            , progress model "en"
            , Html.br [] []
            , Html.text ("Number of repetitions done : " ++ (String.fromInt model.round) ++ "/ 6")
            ]


progress model lang =
    let
        width =
            600

        duration =
            case model.exercise of
                Recovery ( Just s, Just t ) ->
                    round (Duration.inMilliseconds (Duration.from s t)) // 1000 * width // 60

                LongRecovery ( Just s, Just t ) ->
                    round (Duration.inMilliseconds (Duration.from s t)) // 1000 * width // 60

                _ ->
                    0

        color =
            if duration > 30 * width // 60 then
                "green"
            else
                "red"

        recoveringText =
            if lang == "fr" then
                "Temps de récupération"
            else
                "Recovering time"
    in
        Svg.svg
            [ Svg.Attributes.width "400"
            , Svg.Attributes.height "32"
            , Svg.Attributes.viewBox "0 0 600 32"
            , Svg.Attributes.pointerEvents "none"
            ]
            [ Svg.rect [ Svg.Attributes.x "0", Svg.Attributes.y "0", Svg.Attributes.width (String.fromInt (width // 2 + 16)), Svg.Attributes.height "32", Svg.Attributes.rx "16", Svg.Attributes.ry "16", Svg.Attributes.fill "red", Svg.Attributes.fillOpacity "20%" ]
                []
            , Svg.rect
                [ Svg.Attributes.x (String.fromInt (width // 2)), Svg.Attributes.y "0", Svg.Attributes.width (String.fromInt (width // 2 - 16)), Svg.Attributes.height "32", Svg.Attributes.rx "16", Svg.Attributes.ry "16", Svg.Attributes.fill "green", Svg.Attributes.fillOpacity "20%" ]
                []
            , Svg.text_ [ Svg.Attributes.textAnchor "middle", Svg.Attributes.x "50%", Svg.Attributes.y "65%" ] [ Svg.text recoveringText ]
            , Svg.rect [ Svg.Attributes.x "0", Svg.Attributes.y "0", Svg.Attributes.width (String.fromInt duration), Svg.Attributes.height "32", Svg.Attributes.rx "16", Svg.Attributes.ry "16", Svg.Attributes.fill color, Svg.Attributes.fillOpacity "50%" ] []
            ]


subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrame Frame
        , Browser.Events.onResize Resize
        ]


main : Platform.Program String Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
