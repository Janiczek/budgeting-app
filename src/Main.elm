port module Main exposing (main)

import Browser
import Browser.Dom
import Data.Bucket as Bucket exposing (Bucket, BucketId, BucketIdTag)
import Data.Category as Category exposing (Category, CategoryId, CategoryIdTag)
import Data.Id
import Data.IdDict as IdDict exposing (IdDict)
import Data.IdSet as IdSet exposing (IdSet)
import Data.Money as Money exposing (Money)
import Html exposing (Attribute, Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Html.Events.Extra as Events
import Icons
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Random
import Task


port saveToLocalStorage : String -> Cmd msg


type alias Flags =
    { idSeed : Int
    , savedModel : Maybe String
    }


type alias Model =
    { categories : IdDict CategoryIdTag Category
    , buckets : IdDict BucketIdTag Bucket

    --
    , categoriesOrder : List CategoryId
    , bucketsOrder : IdDict CategoryIdTag (List BucketId)

    --
    , newCategoryInput : String
    , newBucketInputs : IdDict CategoryIdTag String

    --
    , bucketMoneyOps : IdDict BucketIdTag MoneyOp

    --
    , idSeed : Random.Seed
    }


type Msg
    = SetNewCategoryInput String
    | SetNewBucketInput CategoryId String
    | AddCategory String
    | AddBucket CategoryId String
    | RemoveCategory CategoryId
    | RemoveBucket BucketId
    | StartMoneyOp BucketId MoneyOp
    | SetMoneyOpInput BucketId String
    | SelectMoneyOpTargetBucket BucketId BucketId
    | FinishMoneyOp BucketId
    | CancelMoneyOp BucketId
    | FocusAttempted
    | ResetModel


type MoneyOp
    = SubtractM String
    | AddM String
    | SetM String
    | MoveToM (Maybe BucketId) String


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update_
        , view = view
        , subscriptions = subscriptions
        }


type alias SavedModel =
    { categories : IdDict CategoryIdTag Category
    , buckets : IdDict BucketIdTag Bucket
    , categoriesOrder : List CategoryId
    , bucketsOrder : IdDict CategoryIdTag (List BucketId)
    }


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    Decode.map4 SavedModel
        (Decode.field "categories" (IdDict.decoder Category.decoder))
        (Decode.field "buckets" (IdDict.decoder Bucket.decoder))
        (Decode.field "categoriesOrder" (Decode.list Data.Id.decoder))
        (Decode.field "bucketsOrder" (IdDict.decoder (Decode.list Data.Id.decoder)))


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        savedModel =
            flags.savedModel
                |> Maybe.andThen
                    (\savedModelString ->
                        savedModelString
                            |> Decode.decodeString savedModelDecoder
                            |> Result.toMaybe
                    )
                |> Maybe.withDefault initSavedModel
    in
    ( initModel (Random.initialSeed flags.idSeed) savedModel
    , Cmd.batch
        [ focus AddCategoryInput
        , saveToLocalStorage (encodeSavedModel savedModel)
        ]
    )


initModel : Random.Seed -> SavedModel -> Model
initModel idSeed savedModel =
    { categories = savedModel.categories
    , buckets = savedModel.buckets

    --
    , categoriesOrder = savedModel.categoriesOrder
    , bucketsOrder = savedModel.bucketsOrder

    --
    , newCategoryInput = ""
    , newBucketInputs = IdDict.empty

    --
    , bucketMoneyOps = IdDict.empty

    --
    , idSeed = idSeed
    }


initSavedModel : SavedModel
initSavedModel =
    { categories = IdDict.empty
    , buckets = IdDict.empty
    , categoriesOrder = []
    , bucketsOrder = IdDict.empty
    }


type DomId
    = AddCategoryInput
    | AddBucketInput CategoryId
    | MoneyOpInput BucketId


domIdToString : DomId -> String
domIdToString domId_ =
    case domId_ of
        AddCategoryInput ->
            "add-category"

        AddBucketInput categoryId ->
            "add-bucket-" ++ Data.Id.unwrap categoryId

        MoneyOpInput bucketId ->
            "money-op-" ++ Data.Id.unwrap bucketId


domId : DomId -> Attribute msg
domId =
    Attrs.id << domIdToString


focus : DomId -> Cmd Msg
focus id =
    domIdToString id
        |> Browser.Dom.focus
        |> Task.attempt (\_ -> FocusAttempted)


encodeSavedModel : SavedModel -> String
encodeSavedModel model =
    [ ( "categories", IdDict.encode Category.encode model.categories )
    , ( "buckets", IdDict.encode Bucket.encode model.buckets )
    , ( "categoriesOrder", Encode.list Data.Id.encode model.categoriesOrder )
    , ( "bucketsOrder", IdDict.encode (Encode.list Data.Id.encode) model.bucketsOrder )
    ]
        |> Encode.object
        |> Encode.encode 0


update_ : Msg -> Model -> ( Model, Cmd Msg )
update_ msg model =
    let
        ( newModel, cmd ) =
            update msg model

        saveCmd : Cmd Msg
        saveCmd =
            saveToLocalStorage (encodeSavedModel (getSavedModel newModel))
    in
    ( newModel
    , Cmd.batch [ cmd, saveCmd ]
    )


getSavedModel : Model -> SavedModel
getSavedModel model =
    { categories = model.categories
    , buckets = model.buckets
    , categoriesOrder = model.categoriesOrder
    , bucketsOrder = model.bucketsOrder
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResetModel ->
            ( initModel model.idSeed initSavedModel
            , Cmd.none
            )

        FocusAttempted ->
            ( model, Cmd.none )

        SetNewCategoryInput input_ ->
            ( { model | newCategoryInput = input_ }
            , Cmd.none
            )

        SetNewBucketInput categoryId input_ ->
            ( { model
                | newBucketInputs =
                    model.newBucketInputs
                        |> IdDict.insert categoryId input_
              }
            , Cmd.none
            )

        AddCategory categoryName ->
            if String.isEmpty categoryName then
                ( model, Cmd.none )

            else
                let
                    ( newCategoryId, newIdSeed ) =
                        Random.step Data.Id.generator model.idSeed
                in
                ( { model
                    | categories =
                        model.categories
                            |> IdDict.insert newCategoryId
                                { id = newCategoryId
                                , name = categoryName
                                }
                    , categoriesOrder = model.categoriesOrder ++ [ newCategoryId ]
                    , newCategoryInput = ""
                    , idSeed = newIdSeed
                  }
                , focus <| AddBucketInput newCategoryId
                )

        AddBucket categoryId bucketName ->
            if String.isEmpty bucketName then
                ( model, Cmd.none )

            else
                let
                    ( newBucketId, newIdSeed ) =
                        Random.step Data.Id.generator model.idSeed
                in
                ( { model
                    | buckets =
                        model.buckets
                            |> IdDict.insert newBucketId
                                { id = newBucketId
                                , categoryId = categoryId
                                , name = bucketName
                                , value = Money.zero
                                }
                    , bucketsOrder =
                        model.bucketsOrder
                            |> IdDict.update categoryId
                                (Maybe.map (\order -> Just (order ++ [ newBucketId ]))
                                    >> Maybe.withDefault (Just [ newBucketId ])
                                )
                    , newBucketInputs =
                        model.newBucketInputs
                            |> IdDict.insert categoryId ""
                    , idSeed = newIdSeed
                  }
                , Cmd.none
                )

        RemoveCategory categoryId ->
            -- TODO modal with moving the money somewhere?
            -- TODO or at least confirm modal
            let
                affectedBuckets : IdSet BucketIdTag
                affectedBuckets =
                    model.buckets
                        |> IdDict.filter (\_ bucket -> bucket.categoryId == categoryId)
                        |> IdDict.keys
                        |> IdSet.fromList
            in
            ( { model
                | categories = IdDict.remove categoryId model.categories
                , buckets = IdDict.filter (\bucketId _ -> not (IdSet.member bucketId affectedBuckets)) model.buckets
                , categoriesOrder = List.filter ((/=) categoryId) model.categoriesOrder
                , bucketsOrder = IdDict.remove categoryId model.bucketsOrder
              }
            , Cmd.none
            )

        RemoveBucket bucketId ->
            case IdDict.get bucketId model.buckets of
                Nothing ->
                    ( model, Cmd.none )

                Just bucket ->
                    -- TODO modal with moving the money somewhere?
                    -- TODO or at least confirm modal
                    ( { model
                        | buckets = IdDict.filter (\id _ -> id /= bucketId) model.buckets
                        , bucketsOrder = IdDict.update bucket.categoryId (Maybe.map (List.filter ((/=) bucketId))) model.bucketsOrder
                      }
                    , Cmd.none
                    )

        StartMoneyOp bucketId moneyOp ->
            ( { model | bucketMoneyOps = IdDict.insert bucketId moneyOp model.bucketMoneyOps }
            , focus <| MoneyOpInput bucketId
            )

        CancelMoneyOp bucketId ->
            ( { model | bucketMoneyOps = IdDict.remove bucketId model.bucketMoneyOps }
            , Cmd.none
            )

        SelectMoneyOpTargetBucket bucketId targetBucketId ->
            ( { model
                | bucketMoneyOps =
                    IdDict.update bucketId
                        (Maybe.map
                            (\op ->
                                case op of
                                    MoveToM _ input_ ->
                                        MoveToM (Just targetBucketId) input_

                                    _ ->
                                        op
                            )
                        )
                        model.bucketMoneyOps
              }
            , Cmd.none
            )

        SetMoneyOpInput bucketId input_ ->
            ( { model
                | bucketMoneyOps =
                    IdDict.update bucketId
                        (Maybe.map
                            (\op ->
                                case op of
                                    AddM _ ->
                                        AddM input_

                                    SubtractM _ ->
                                        SubtractM input_

                                    SetM _ ->
                                        SetM input_

                                    MoveToM targetBucketId _ ->
                                        MoveToM targetBucketId input_
                            )
                        )
                        model.bucketMoneyOps
              }
            , Cmd.none
            )

        FinishMoneyOp bucketId ->
            Maybe.map2 Tuple.pair
                (IdDict.get bucketId model.bucketMoneyOps)
                (IdDict.get bucketId model.buckets)
                |> Maybe.andThen
                    (\( moneyOp, bucket ) ->
                        let
                            singleBucketOp : (Money -> Money) -> ( Model, Cmd Msg )
                            singleBucketOp moneyFn =
                                let
                                    newBucket : Bucket
                                    newBucket =
                                        { bucket | value = moneyFn bucket.value }
                                in
                                ( { model
                                    | buckets =
                                        model.buckets
                                            |> IdDict.insert bucketId newBucket
                                    , bucketMoneyOps = IdDict.remove bucketId model.bucketMoneyOps
                                  }
                                , Cmd.none
                                )
                        in
                        case moneyOp of
                            SubtractM valueString ->
                                Money.fromString valueString
                                    |> Maybe.map (singleBucketOp << Money.subtract)

                            AddM valueString ->
                                Money.fromString valueString
                                    |> Maybe.map (singleBucketOp << Money.add)

                            SetM valueString ->
                                Money.fromString valueString
                                    |> Maybe.map (singleBucketOp << (\value _ -> value))

                            MoveToM targetBucketId valueString ->
                                Maybe.map2
                                    (\value targetBucket ->
                                        let
                                            newSourceBucket : Bucket
                                            newSourceBucket =
                                                { bucket
                                                    | value =
                                                        bucket.value
                                                            |> Money.subtract value
                                                }

                                            newTargetBucket : Bucket
                                            newTargetBucket =
                                                { targetBucket
                                                    | value =
                                                        targetBucket.value
                                                            |> Money.add value
                                                }
                                        in
                                        ( { model
                                            | buckets =
                                                model.buckets
                                                    |> IdDict.insert bucketId newSourceBucket
                                                    |> IdDict.insert targetBucket.id newTargetBucket
                                            , bucketMoneyOps = IdDict.remove bucketId model.bucketMoneyOps
                                          }
                                        , Cmd.none
                                        )
                                    )
                                    (Money.fromString valueString)
                                    (targetBucketId |> Maybe.andThen (\id -> IdDict.get id model.buckets))
                    )
                |> Maybe.withDefault ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        totalValue : Money
        totalValue =
            model.buckets
                |> IdDict.values
                |> List.map .value
                |> List.foldl Money.add Money.zero

        addCategory : Msg
        addCategory =
            AddCategory model.newCategoryInput
    in
    Html.div
        [ Attrs.class "p-2 flex flex-col gap-2 tabular-nums" ]
        [ Html.div
            [ Attrs.class "flex justify-between" ]
            [ Html.span
                [ Attrs.class "font-bold" ]
                [ Html.text "Budgeting" ]
            , Html.div [ Attrs.class "flex gap-2" ]
                [ Html.text "Total value: "
                , valuePill totalValue
                ]
            ]
        , Html.div
            [ Attrs.class "flex flex-col gap-2" ]
            (model.categoriesOrder
                |> List.filterMap (\categoryId -> IdDict.get categoryId model.categories)
                |> List.map (categoryView model)
            )
        , Html.div
            [ Attrs.class "flex gap-2" ]
            [ input
                [ Events.onInput SetNewCategoryInput
                , Events.onEnter addCategory
                , Attrs.placeholder "New category name"
                , domId AddCategoryInput
                ]
                model.newCategoryInput
            , button
                Sky
                [ Events.onClick addCategory ]
                [ Html.text "Add category" ]
            ]
        , debugView "categories" model.categories
        , debugView "buckets" model.buckets
        , debugView "categoriesOrder" model.categoriesOrder
        , debugView "bucketsOrder" model.bucketsOrder
        , Html.div []
            [ button
                Orange
                [ Events.onClick ResetModel
                ]
                [ Icons.trashCan
                , Html.text "Nuke the model"
                ]
            ]
        ]


debugView : String -> a -> Html msg
debugView label value =
    Html.div
        [ Attrs.class "flex flex-col gap-2 p-2 border border-fuchsia-100 bg-fuchsia-50" ]
        [ Html.div
            [ Attrs.class "text-fuchsia-700 text-[14px]" ]
            [ Html.text "Debugging "
            , Html.code
                [ Attrs.class "border border-fuchsia-200 bg-fuchsia-100 px-1 text-[12px]" ]
                [ Html.text label ]
            , Html.text ":"
            ]
        , Html.pre
            [ Attrs.class "break-all whitespace-pre-wrap overflow-x-hidden p-2 bg-fuchsia-100 text-[12px] text-fuchsia-600" ]
            [ Html.text <| Debug.toString value ]
        ]


categoryView : Model -> Category -> Html Msg
categoryView model category =
    let
        buckets : List Bucket
        buckets =
            model.bucketsOrder
                |> IdDict.get category.id
                |> Maybe.withDefault []
                |> List.filterMap (\bucketId -> IdDict.get bucketId model.buckets)

        newBucketInput : String
        newBucketInput =
            model.newBucketInputs
                |> IdDict.get category.id
                |> Maybe.withDefault ""

        addBucket : Msg
        addBucket =
            AddBucket category.id newBucketInput
    in
    Html.div
        [ Attrs.class "p-2 border bg-slate-50 flex flex-col gap-2" ]
        [ Html.div
            [ Attrs.class "flex justify-between" ]
            [ Html.div
                [ Attrs.class "font-semibold" ]
                [ Html.text category.name ]
            , Html.div
                [ Attrs.class "flex gap-1" ]
                [ button
                    Orange
                    [ Events.onClick <| RemoveCategory category.id ]
                    [ Icons.xmark ]
                ]
            ]
        , Html.div
            [ Attrs.class "flex flex-col gap-1" ]
            (buckets
                |> List.map (bucketView model)
            )
        , Html.div
            [ Attrs.class "flex gap-2" ]
            [ input
                [ Events.onInput <| SetNewBucketInput category.id
                , Events.onEnter addBucket
                , Attrs.placeholder "New bucket name"
                , domId <| AddBucketInput category.id
                ]
                newBucketInput
            , button
                Sky
                [ Events.onClick addBucket ]
                [ Html.text "Add bucket" ]
            ]
        ]


bucketView : Model -> Bucket -> Html Msg
bucketView model bucket =
    let
        moneyOp : Maybe MoneyOp
        moneyOp =
            IdDict.get bucket.id model.bucketMoneyOps

        singleBucketView : String -> String -> Html Msg
        singleBucketView valueString placeholder =
            Html.div
                [ Attrs.class "flex gap-1" ]
                [ input
                    [ Events.onInput <| SetMoneyOpInput bucket.id
                    , Events.onEnter <| FinishMoneyOp bucket.id
                    , Attrs.placeholder placeholder
                    , domId <| MoneyOpInput bucket.id
                    ]
                    valueString
                , button
                    Sky
                    [ Events.onClick <| CancelMoneyOp bucket.id ]
                    [ Icons.xmark ]
                , button
                    Sky
                    [ Events.onClick <| FinishMoneyOp bucket.id
                    , Attrs.disabled <| not <| isValidNumber valueString
                    ]
                    [ Icons.check ]
                ]

        targetBucketOptionView : Maybe BucketId -> Bucket -> Html Msg
        targetBucketOptionView selectedTargetBucketId targetBucket =
            Html.option
                [ Attrs.selected <| selectedTargetBucketId == Just targetBucket.id
                , Attrs.value <| Data.Id.unwrap targetBucket.id
                ]
                [ Html.text <| targetBucket.name ]
    in
    Html.div
        [ Attrs.class "px-2 py-1 border bg-slate-100 flex justify-between" ]
        [ Html.div
            [ Attrs.class "font-semibold" ]
            [ Html.text bucket.name ]
        , Html.div
            [ Attrs.class "flex gap-2" ]
            [ valuePill bucket.value
            , case moneyOp of
                Nothing ->
                    Html.div
                        [ Attrs.class "flex gap-1 align-stretch" ]
                        [ button
                            Sky
                            [ Events.onClick <| StartMoneyOp bucket.id (SubtractM "") ]
                            [ Icons.minus ]
                        , button
                            Sky
                            [ Events.onClick <| StartMoneyOp bucket.id (AddM "") ]
                            [ Icons.plus ]
                        , button
                            Sky
                            [ Events.onClick <| StartMoneyOp bucket.id (SetM "") ]
                            [ Icons.equals ]
                        , button
                            Sky
                            [ Events.onClick <| StartMoneyOp bucket.id (MoveToM Nothing "") ]
                            [ Icons.arrowRight ]
                        ]

                Just (SubtractM valueString) ->
                    singleBucketView valueString
                        "Amount to subtract"

                Just (AddM valueString) ->
                    singleBucketView valueString
                        "Amount to add"

                Just (SetM valueString) ->
                    singleBucketView valueString
                        "Amount to set"

                Just (MoveToM targetBucketId valueString) ->
                    let
                        otherBuckets : List Bucket
                        otherBuckets =
                            model.buckets
                                |> IdDict.values
                                |> List.filter (.id >> (/=) bucket.id)

                        targetBucketMsgDecoder : Decoder Msg
                        targetBucketMsgDecoder =
                            Decode.at [ "target", "value" ] Data.Id.decoder
                                |> Decode.map (SelectMoneyOpTargetBucket bucket.id)
                    in
                    Html.div
                        [ Attrs.class "flex gap-1" ]
                        [ input
                            [ Events.onInput <| SetMoneyOpInput bucket.id
                            , Events.onEnter <| FinishMoneyOp bucket.id
                            , Attrs.placeholder "Amount to move"
                            , domId <| MoneyOpInput bucket.id
                            ]
                            valueString
                        , Html.select
                            [ Attrs.class "appearance-none border px-2 bg-sky-100 rounded border-sky-300 border hover:bg-sky-200 hover:border-sky-400"
                            , Events.on "change" targetBucketMsgDecoder
                            ]
                            (Html.option
                                [ Attrs.disabled True
                                , Attrs.selected <| targetBucketId == Nothing
                                ]
                                [ Html.text "Move where? ▼" ]
                                :: List.map (targetBucketOptionView targetBucketId) otherBuckets
                            )
                        , button
                            Sky
                            [ Events.onClick <| CancelMoneyOp bucket.id ]
                            [ Icons.xmark ]
                        , button
                            Sky
                            [ Events.onClick <| FinishMoneyOp bucket.id
                            , Attrs.disabled <| not <| isValidNumber valueString
                            ]
                            [ Icons.check ]
                        ]
            , button
                Orange
                [ Events.onClick <| RemoveBucket bucket.id ]
                [ Icons.xmark ]
            ]
        ]


isValidNumber : String -> Bool
isValidNumber valueString =
    case Money.fromString valueString of
        Just value ->
            let
                ( whole, cents ) =
                    Money.toParts value
            in
            whole >= 0

        Nothing ->
            False


type ButtonColor
    = Sky
    | Orange


buttonColor : ButtonColor -> Attribute msg
buttonColor color =
    case color of
        Sky ->
            Attrs.class "bg-sky-200 border-sky-400 text-sky-700 hover:bg-sky-300 hover:border-sky-500"

        Orange ->
            Attrs.class "bg-orange-200 border-orange-400 text-orange-700 hover:bg-orange-300 hover:border-orange-500"


button : ButtonColor -> List (Attribute msg) -> List (Html msg) -> Html msg
button color attrs contents =
    Html.button
        (Attrs.class "border px-2 rounded border flex flex-row gap-1 items-center disabled:cursor-not-allowed disabled:bg-gray-200 disabled:border-gray-400 disabled:text-gray-400 disabled:hover:bg-gray-300"
            :: buttonColor color
            :: attrs
        )
        contents


input : List (Attribute msg) -> String -> Html msg
input attrs value =
    Html.input
        (Attrs.value value
            :: Attrs.class "border px-2 bg-sky-100 rounded border-sky-300 border hover:bg-sky-200 hover:border-sky-400"
            :: attrs
        )
        []


valuePill : Money -> Html msg
valuePill money =
    Html.div
        [ Attrs.class "rounded bg-lime-200 px-2 border border-lime-400 text-lime-600 hover:bg-lime-300 hover:border-lime-500" ]
        [ Html.text <| Money.toString money ]
