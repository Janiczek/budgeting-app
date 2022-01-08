module Icons exposing
    ( arrowRight
    , check
    , equals
    , minus
    , plus
    , xmark
    )

import Html exposing (Html)
import Html.Attributes as Attrs
import Svg
import Svg.Attributes as SvgAttrs


fontAwesomeIcon : String -> String -> Html msg
fontAwesomeIcon viewBox path =
    Svg.svg
        [ Attrs.attribute "aria-hidden" "true"
        , Attrs.attribute "focusable" "false"
        , Attrs.attribute "role" "img"
        , SvgAttrs.viewBox viewBox
        , SvgAttrs.width "16"
        , SvgAttrs.height "16"
        ]
        [ Svg.path
            [ SvgAttrs.fill "currentColor"
            , SvgAttrs.d path
            ]
            []
        ]


plus : Html msg
plus =
    fontAwesomeIcon
        "0 0 448 512"
        "M432 256c0 17.69-14.33 32.01-32 32.01H256v144c0 17.69-14.33 31.99-32 31.99s-32-14.3-32-31.99v-144H48c-17.67 0-32-14.32-32-32.01s14.33-31.99 32-31.99H192v-144c0-17.69 14.33-32.01 32-32.01s32 14.32 32 32.01v144h144C417.7 224 432 238.3 432 256z"


minus : Html msg
minus =
    fontAwesomeIcon
        "0 0 448 512"
        "M400 288h-352c-17.69 0-32-14.32-32-32.01s14.31-31.99 32-31.99h352c17.69 0 32 14.3 32 31.99S417.7 288 400 288z"


equals : Html msg
equals =
    fontAwesomeIcon
        "0 0 448 512"
        "M48 192h352c17.69 0 32-14.32 32-32s-14.31-31.1-32-31.1h-352c-17.69 0-32 14.31-32 31.1S30.31 192 48 192zM400 320h-352c-17.69 0-32 14.31-32 31.1s14.31 32 32 32h352c17.69 0 32-14.32 32-32S417.7 320 400 320z"


arrowRight : Html msg
arrowRight =
    fontAwesomeIcon
        "0 0 448 512"
        "M438.6 278.6l-160 160C272.4 444.9 264.2 448 256 448s-16.38-3.125-22.62-9.375c-12.5-12.5-12.5-32.75 0-45.25L338.8 288H32C14.33 288 .0016 273.7 .0016 256S14.33 224 32 224h306.8l-105.4-105.4c-12.5-12.5-12.5-32.75 0-45.25s32.75-12.5 45.25 0l160 160C451.1 245.9 451.1 266.1 438.6 278.6z"


xmark : Html msg
xmark =
    fontAwesomeIcon
        "0 0 320 512"
        "M310.6 361.4c12.5 12.5 12.5 32.75 0 45.25C304.4 412.9 296.2 416 288 416s-16.38-3.125-22.62-9.375L160 301.3L54.63 406.6C48.38 412.9 40.19 416 32 416S15.63 412.9 9.375 406.6c-12.5-12.5-12.5-32.75 0-45.25l105.4-105.4L9.375 150.6c-12.5-12.5-12.5-32.75 0-45.25s32.75-12.5 45.25 0L160 210.8l105.4-105.4c12.5-12.5 32.75-12.5 45.25 0s12.5 32.75 0 45.25l-105.4 105.4L310.6 361.4z"


check : Html msg
check =
    fontAwesomeIcon
        "0 0 512 512"
        "M480 128c0 8.188-3.125 16.38-9.375 22.62l-256 256C208.4 412.9 200.2 416 192 416s-16.38-3.125-22.62-9.375l-128-128C35.13 272.4 32 264.2 32 256c0-18.28 14.95-32 32-32c8.188 0 16.38 3.125 22.62 9.375L192 338.8l233.4-233.4C431.6 99.13 439.8 96 448 96C465.1 96 480 109.7 480 128z"
