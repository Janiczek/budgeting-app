module Main exposing (main)

import Browser
import Browser.Dom
import Data.Bucket as Bucket exposing (Bucket, BucketId, BucketIdTag)
import Data.Category exposing (Category, CategoryId, CategoryIdTag)
import Data.Id
import Data.IdDict as IdDict exposing (IdDict)
import Data.IdSet as IdSet exposing (IdSet)
import Data.Money as Money exposing (Money)
import Html exposing (Attribute, Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Html.Events.Extra as Events
import Icons
import Random
import Task


type alias Flags =
    ()


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
    | FinishMoneyOp BucketId
    | CancelMoneyOp BucketId
    | FocusAttempted


type MoneyOp
    = SubtractM String
    | AddM String
    | SetM String
    | MoveToM (Maybe BucketId) String


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { categories = IdDict.empty
      , buckets = IdDict.empty

      --
      , categoriesOrder = []
      , bucketsOrder = IdDict.empty

      --
      , newCategoryInput = ""
      , newBucketInputs = IdDict.empty

      --
      , bucketMoneyOps = IdDict.empty

      --
      , idSeed = Random.initialSeed 0
      }
    , focus AddCategoryInput
    )


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
        [ Attrs.class "p-2 flex flex-col gap-2" ]
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
        ]


categoryView : Model -> Category -> Html Msg
categoryView model category =
    let
        buckets : List Bucket
        buckets =
            model.buckets
                |> IdDict.filter (\_ bucket -> bucket.categoryId == category.id)
                |> IdDict.values

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
                [ Attrs.class "flex gap-1" ]
                [ Html.text "Category: "
                , Html.span
                    [ Attrs.class "font-semibold" ]
                    [ Html.text category.name ]
                ]
            , Html.div
                [ Attrs.class "flex gap-1" ]
                [ button
                    Orange
                    [ Events.onClick <| RemoveCategory category.id ]
                    [ Icons.xmark ]
                ]
            ]
        , Html.div
            [ Attrs.class "flex flex-col pl-4" ]
            (buckets
                |> List.map (bucketView model)
            )
        , Html.div
            [ Attrs.class "flex gap-2 pl-4" ]
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
                    [ Events.onClick <| FinishMoneyOp bucket.id ]
                    [ Icons.check ]
                ]
    in
    Html.div
        [ Attrs.class "flex justify-between" ]
        [ Html.div
            [ Attrs.class "flex gap-2" ]
            [ Html.text "Bucket: "
            , Html.span
                [ Attrs.class "font-semibold" ]
                [ Html.text bucket.name ]
            ]
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
                    Html.div [] [ Html.text "TODO move money" ]
            , button
                Orange
                [ Events.onClick <| RemoveBucket bucket.id ]
                [ Icons.xmark ]
            ]
        ]


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
        (Attrs.class "border px-2 rounded border"
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
