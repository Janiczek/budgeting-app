port module Main exposing (main)

import Browser
import Browser.Dom
import Data.Bucket as Bucket exposing (Bucket, BucketId, BucketIdTag)
import Data.Category as Category exposing (Category, CategoryId, CategoryIdTag)
import Data.Id
import Data.IdDict as IdDict exposing (IdDict)
import Data.IdSet as IdSet exposing (IdSet)
import Data.Money as Money exposing (Money)
import File exposing (File)
import File.Download
import File.Select
import Html exposing (Attribute, Html)
import Html.Attributes as Attrs
import Html.Attributes.Extra as Attrs
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
    , toBeBudgeted : Money

    --
    , categoriesOrder : List CategoryId
    , bucketsOrder : IdDict CategoryIdTag (List BucketId)

    --
    , newCategoryInput : String
    , newBucketInputs : IdDict CategoryIdTag String
    , categoryRenameInputs : IdDict CategoryIdTag String
    , bucketRenameInputs : IdDict BucketIdTag String

    --
    , bucketMoneyOps : IdDict BucketIdTag MoneyOp
    , toBeBudgetedMoneyOp : Maybe MoneyOp

    --
    , idSeed : Random.Seed
    }


type Msg
    = -- bucket/category management
      SetNewCategoryInput String
    | SetNewBucketInput CategoryId String
    | AddCategory String
    | AddBucket CategoryId String
    | RemoveCategory CategoryId
    | RemoveBucket BucketId
      -- money operations
    | StartMoneyOp BucketType MoneyOp
    | SetMoneyOpInput BucketType String
    | SelectMoneyOpOtherBucket BucketType BucketType
    | FinishMoneyOp BucketType
    | CancelMoneyOp BucketType
      -- focus
    | FocusAttempted
      -- export/import
    | Export
    | ImportButtonClicked
    | ImportFileSelected File
    | ImportJson String
      -- renaming buckets
    | SetRenameBucketInput BucketId String
    | CancelRenamingBucket BucketId
    | FinishRenamingBucket BucketId
      -- renaming categories
    | SetRenameCategoryInput CategoryId String
    | CancelRenamingCategory CategoryId
    | FinishRenamingCategory CategoryId


type MoneyOp
    = SubtractM String
    | AddM String
    | SetM String
    | MoveToM (Maybe BucketType) String
    | TakeFromM (Maybe BucketType) String


type BucketType
    = ToBeBudgeted
    | NormalBucket BucketId


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
    , toBeBudgeted : Money
    , categoriesOrder : List CategoryId
    , bucketsOrder : IdDict CategoryIdTag (List BucketId)
    }


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    Decode.map5 SavedModel
        (Decode.field "categories" (IdDict.decoder Category.decoder))
        (Decode.field "buckets" (IdDict.decoder Bucket.decoder))
        (Decode.field "toBeBudgeted" Money.decoder)
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
    , toBeBudgeted = savedModel.toBeBudgeted

    --
    , categoriesOrder = savedModel.categoriesOrder
    , bucketsOrder = savedModel.bucketsOrder

    --
    , newCategoryInput = ""
    , newBucketInputs = IdDict.empty
    , categoryRenameInputs = IdDict.empty
    , bucketRenameInputs = IdDict.empty

    --
    , bucketMoneyOps = IdDict.empty
    , toBeBudgetedMoneyOp = Nothing

    --
    , idSeed = idSeed
    }


initSavedModel : SavedModel
initSavedModel =
    { categories = IdDict.empty
    , buckets = IdDict.empty
    , toBeBudgeted = Money.zero
    , categoriesOrder = []
    , bucketsOrder = IdDict.empty
    }


type DomId
    = AddCategoryInput
    | AddBucketInput CategoryId
    | MoneyOpInput BucketType


domIdToString : DomId -> String
domIdToString domId_ =
    case domId_ of
        AddCategoryInput ->
            "add-category"

        AddBucketInput categoryId ->
            "add-bucket-" ++ Data.Id.unwrap categoryId

        MoneyOpInput bucketType ->
            "money-op-" ++ bucketTypeToString bucketType


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
    , ( "toBeBudgeted", Money.encode model.toBeBudgeted )
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
    , toBeBudgeted = model.toBeBudgeted
    , categoriesOrder = model.categoriesOrder
    , bucketsOrder = model.bucketsOrder
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FocusAttempted ->
            ( model, Cmd.none )

        Export ->
            ( model
            , File.Download.string
                "budgeting.json"
                "application/json"
                (encodeSavedModel <| getSavedModel model)
            )

        ImportButtonClicked ->
            ( model
            , File.Select.file [ "application/json" ] ImportFileSelected
            )

        ImportFileSelected file ->
            ( model
            , Task.perform ImportJson (File.toString file)
            )

        ImportJson jsonString ->
            ( jsonString
                |> Decode.decodeString savedModelDecoder
                |> Result.map (initModel model.idSeed)
                |> Result.withDefault model
            , Cmd.none
            )

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
            let
                affectedBucketsDict : IdDict BucketIdTag Bucket
                affectedBucketsDict =
                    model.buckets
                        |> IdDict.filter (\_ bucket -> bucket.categoryId == categoryId)

                affectedBucketIds : IdSet BucketIdTag
                affectedBucketIds =
                    affectedBucketsDict
                        |> IdDict.keys
                        |> IdSet.fromList

                lostValue : Money
                lostValue =
                    affectedBucketsDict
                        |> IdDict.values
                        |> List.map .value
                        |> List.foldl Money.add Money.zero
            in
            ( { model
                | categories = IdDict.remove categoryId model.categories
                , buckets = IdDict.filter (\bucketId _ -> not (IdSet.member bucketId affectedBucketIds)) model.buckets
                , categoriesOrder = List.filter ((/=) categoryId) model.categoriesOrder
                , bucketsOrder = IdDict.remove categoryId model.bucketsOrder
                , toBeBudgeted = Money.add lostValue model.toBeBudgeted
              }
            , Cmd.none
            )

        RemoveBucket bucketId ->
            case IdDict.get bucketId model.buckets of
                Nothing ->
                    ( model, Cmd.none )

                Just bucket ->
                    ( { model
                        | buckets = IdDict.filter (\id _ -> id /= bucketId) model.buckets
                        , bucketsOrder = IdDict.update bucket.categoryId (Maybe.map (List.filter ((/=) bucketId))) model.bucketsOrder
                        , toBeBudgeted = Money.add bucket.value model.toBeBudgeted
                      }
                    , Cmd.none
                    )

        StartMoneyOp bucketType moneyOp ->
            ( case bucketType of
                ToBeBudgeted ->
                    { model | toBeBudgetedMoneyOp = Just moneyOp }

                NormalBucket bucketId ->
                    { model | bucketMoneyOps = IdDict.insert bucketId moneyOp model.bucketMoneyOps }
            , focus <| MoneyOpInput bucketType
            )

        CancelMoneyOp bucketType ->
            ( model
                |> closeMoneyOp bucketType
            , Cmd.none
            )

        SelectMoneyOpOtherBucket bucketType otherBucketType ->
            let
                fn op =
                    case op of
                        MoveToM _ input_ ->
                            MoveToM (Just otherBucketType) input_

                        TakeFromM _ input_ ->
                            TakeFromM (Just otherBucketType) input_

                        _ ->
                            op
            in
            ( case bucketType of
                ToBeBudgeted ->
                    { model | toBeBudgetedMoneyOp = Maybe.map fn model.toBeBudgetedMoneyOp }

                NormalBucket bucketId ->
                    { model | bucketMoneyOps = IdDict.update bucketId (Maybe.map fn) model.bucketMoneyOps }
            , Cmd.none
            )

        SetMoneyOpInput bucketType input_ ->
            let
                fn op =
                    case op of
                        AddM _ ->
                            AddM input_

                        SubtractM _ ->
                            SubtractM input_

                        SetM _ ->
                            SetM input_

                        MoveToM otherBucketId _ ->
                            MoveToM otherBucketId input_

                        TakeFromM otherBucketId _ ->
                            TakeFromM otherBucketId input_
            in
            ( case bucketType of
                ToBeBudgeted ->
                    { model | toBeBudgetedMoneyOp = Maybe.map fn model.toBeBudgetedMoneyOp }

                NormalBucket bucketId ->
                    { model | bucketMoneyOps = IdDict.update bucketId (Maybe.map fn) model.bucketMoneyOps }
            , Cmd.none
            )

        FinishMoneyOp bucketType ->
            getMoneyOp bucketType model
                |> Maybe.andThen
                    (\moneyOp ->
                        let
                            singleBucketOp : (Money -> Money) -> ( Model, Cmd Msg )
                            singleBucketOp moneyFn =
                                ( case bucketType of
                                    ToBeBudgeted ->
                                        { model
                                            | toBeBudgeted = moneyFn model.toBeBudgeted
                                            , toBeBudgetedMoneyOp = Nothing
                                        }

                                    NormalBucket bucketId ->
                                        { model
                                            | buckets =
                                                model.buckets
                                                    |> IdDict.update bucketId (Maybe.map (\bucket -> { bucket | value = moneyFn bucket.value }))
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

                            MoveToM targetBucketType valueString ->
                                Maybe.map2
                                    (\value targetBucketType_ ->
                                        ( model
                                            |> updateMoney bucketType (Money.subtract value)
                                            |> updateMoney targetBucketType_ (Money.add value)
                                            |> closeMoneyOp bucketType
                                            |> closeMoneyOp targetBucketType_
                                        , Cmd.none
                                        )
                                    )
                                    (Money.fromString valueString)
                                    targetBucketType

                            TakeFromM sourceBucketType valueString ->
                                Maybe.map2
                                    (\value sourceBucketType_ ->
                                        ( model
                                            |> updateMoney bucketType (Money.add value)
                                            |> updateMoney sourceBucketType_ (Money.subtract value)
                                            |> closeMoneyOp bucketType
                                            |> closeMoneyOp sourceBucketType_
                                        , Cmd.none
                                        )
                                    )
                                    (Money.fromString valueString)
                                    sourceBucketType
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        SetRenameBucketInput bucketId bucketName ->
            ( { model
                | bucketRenameInputs =
                    model.bucketRenameInputs
                        |> IdDict.insert bucketId bucketName
              }
            , Cmd.none
            )

        CancelRenamingBucket bucketId ->
            ( { model | bucketRenameInputs = IdDict.remove bucketId model.bucketRenameInputs }
            , Cmd.none
            )

        FinishRenamingBucket bucketId ->
            ( IdDict.get bucketId model.bucketRenameInputs
                |> Maybe.map
                    (\newName ->
                        { model
                            | bucketRenameInputs = IdDict.remove bucketId model.bucketRenameInputs
                            , buckets =
                                model.buckets
                                    |> IdDict.update bucketId (Maybe.map (\bucket -> { bucket | name = newName }))
                        }
                    )
                |> Maybe.withDefault model
            , Cmd.none
            )

        SetRenameCategoryInput categoryId categoryName ->
            ( { model
                | categoryRenameInputs =
                    model.categoryRenameInputs
                        |> IdDict.insert categoryId categoryName
              }
            , Cmd.none
            )

        CancelRenamingCategory categoryId ->
            ( { model | categoryRenameInputs = IdDict.remove categoryId model.categoryRenameInputs }
            , Cmd.none
            )

        FinishRenamingCategory categoryId ->
            ( IdDict.get categoryId model.categoryRenameInputs
                |> Maybe.map
                    (\newName ->
                        { model
                            | categoryRenameInputs = IdDict.remove categoryId model.categoryRenameInputs
                            , categories =
                                model.categories
                                    |> IdDict.update categoryId (Maybe.map (\category -> { category | name = newName }))
                        }
                    )
                |> Maybe.withDefault model
            , Cmd.none
            )


updateMoney : BucketType -> (Money -> Money) -> Model -> Model
updateMoney bucketType moneyFn model =
    case bucketType of
        ToBeBudgeted ->
            { model | toBeBudgeted = moneyFn model.toBeBudgeted }

        NormalBucket bucketId ->
            { model
                | buckets =
                    model.buckets
                        |> IdDict.update bucketId
                            (Maybe.map (\bucket -> { bucket | value = moneyFn bucket.value }))
            }


closeMoneyOp : BucketType -> Model -> Model
closeMoneyOp bucketType model =
    case bucketType of
        ToBeBudgeted ->
            { model | toBeBudgetedMoneyOp = Nothing }

        NormalBucket bucketId ->
            { model | bucketMoneyOps = IdDict.remove bucketId model.bucketMoneyOps }


getMoneyOp : BucketType -> Model -> Maybe MoneyOp
getMoneyOp bucketType model =
    case bucketType of
        ToBeBudgeted ->
            model.toBeBudgetedMoneyOp

        NormalBucket bucketId ->
            IdDict.get bucketId model.bucketMoneyOps


subscriptions : Model -> Sub Msg
subscriptions _ =
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
                |> Money.add model.toBeBudgeted

        addCategory : Msg
        addCategory =
            AddCategory model.newCategoryInput

        categoriesAndBuckets : List ( Category, List Bucket )
        categoriesAndBuckets =
            sortedCategoriesAndBuckets model
    in
    Html.div
        [ Attrs.class "p-2 flex flex-col gap-2 tabular-nums text-[14px]" ]
        [ Html.div
            [ Attrs.class "flex justify-between" ]
            [ Html.span
                [ Attrs.class "text-xl font-bold" ]
                [ Html.text "Budgeting" ]
            , Html.div [ Attrs.class "flex flex-col gap-2 items-end" ]
                [ Html.div [ Attrs.class "flex gap-2" ]
                    [ Html.text "Total value: "
                    , valuePill
                        Nothing
                        totalValue
                    ]
                , Html.div [ Attrs.class "flex gap-2" ]
                    [ Html.text <| toBeBudgetedName ++ ": "
                    , valuePill
                        (model.toBeBudgeted
                            |> Money.complementToPositive
                            |> Money.toString
                            |> TakeFromM Nothing
                            |> StartMoneyOp ToBeBudgeted
                            |> Just
                        )
                        model.toBeBudgeted
                    , moneyOpView
                        { startMoneyOp = StartMoneyOp ToBeBudgeted
                        , selectOtherBucket = SelectMoneyOpOtherBucket ToBeBudgeted
                        , setMoneyOpInput = SetMoneyOpInput ToBeBudgeted
                        , finishMoneyOp = FinishMoneyOp ToBeBudgeted
                        , cancelMoneyOp = CancelMoneyOp ToBeBudgeted
                        , inputDomId = MoneyOpInput ToBeBudgeted
                        , currentBucket = ToBeBudgeted
                        , categoriesAndBuckets = categoriesAndBuckets
                        , bucketName = getBucketName model
                        }
                        model.toBeBudgetedMoneyOp
                    ]
                ]
            ]
        , Html.div
            [ Attrs.class "flex flex-col gap-2" ]
            (categoriesAndBuckets
                |> List.map (categoryView model categoriesAndBuckets)
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
        , Html.div
            [ Attrs.class "flex gap-2" ]
            [ button
                Orange
                [ Events.onClick ImportButtonClicked ]
                [ Html.text "Import" ]
            , button
                Orange
                [ Events.onClick Export ]
                [ Html.text "Export" ]
            ]
        ]


categoryView : Model -> List ( Category, List Bucket ) -> ( Category, List Bucket ) -> Html Msg
categoryView model categoriesAndBuckets ( category, buckets ) =
    let
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
            [ Attrs.class "group flex justify-between" ]
            [ case IdDict.get category.id model.categoryRenameInputs of
                Nothing ->
                    Html.div
                        [ Attrs.class "flex gap-2" ]
                        [ Html.div
                            [ Attrs.class "font-semibold" ]
                            [ Html.text category.name ]
                        , button
                            Inline
                            [ Events.onClick <| SetRenameCategoryInput category.id category.name
                            , Attrs.class "invisible group-hover:visible"
                            ]
                            [ Icons.pencil ]
                        ]

                Just newName ->
                    Html.div
                        [ Attrs.class "flex gap-1" ]
                        [ input
                            [ Events.onInput <| SetRenameCategoryInput category.id
                            , Events.onEnter <| FinishRenamingCategory category.id
                            , Attrs.placeholder "New category name"
                            ]
                            newName
                        , button
                            Sky
                            [ Events.onClick <| CancelRenamingCategory category.id ]
                            [ Icons.xmark ]
                        , button
                            Sky
                            [ Events.onClick <| FinishRenamingCategory category.id
                            , Attrs.disabled <| String.isEmpty newName
                            ]
                            [ Icons.check ]
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
            [ Attrs.class "flex flex-col gap-1" ]
            (List.map (bucketView model categoriesAndBuckets) buckets)
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


sortedCategoriesAndBuckets : Model -> List ( Category, List Bucket )
sortedCategoriesAndBuckets model =
    model.categoriesOrder
        |> List.filterMap (\categoryId -> IdDict.get categoryId model.categories)
        |> List.map
            (\category ->
                ( category
                , model.bucketsOrder
                    |> IdDict.get category.id
                    |> Maybe.withDefault []
                    |> List.filterMap (\bucketId -> IdDict.get bucketId model.buckets)
                )
            )


otherBucketOptionsView :
    { currentBucket : BucketType
    , selectedOtherBucket : Maybe BucketType
    , bucketName : BucketType -> String
    }
    -> ( Category, List Bucket )
    -> List (Html msg)
otherBucketOptionsView config ( category, buckets ) =
    Html.option
        [ Attrs.disabled True ]
        [ Html.text <| "[ " ++ category.name ++ " ]" ]
        :: List.map (.id >> NormalBucket >> otherBucketOptionView config) buckets


toBeBudgetedValue : String
toBeBudgetedValue =
    "to-be-budgeted"


bucketTypeToString : BucketType -> String
bucketTypeToString bucketType =
    case bucketType of
        ToBeBudgeted ->
            toBeBudgetedValue

        NormalBucket bucketId ->
            Data.Id.unwrap bucketId


bucketTypeDecoder : Decoder BucketType
bucketTypeDecoder =
    Decode.string
        |> Decode.map
            (\value ->
                if value == toBeBudgetedValue then
                    ToBeBudgeted

                else
                    NormalBucket <| Data.Id.fromString value
            )


otherBucketOptionView :
    { currentBucket : BucketType
    , selectedOtherBucket : Maybe BucketType
    , bucketName : BucketType -> String
    }
    -> BucketType
    -> Html msg
otherBucketOptionView config otherBucket =
    Html.option
        [ Attrs.selected <| config.selectedOtherBucket == Just otherBucket
        , Attrs.disabled <| config.currentBucket == otherBucket
        , Attrs.value <| bucketTypeToString otherBucket
        ]
        [ Html.text <| config.bucketName otherBucket ]


bucketView : Model -> List ( Category, List Bucket ) -> Bucket -> Html Msg
bucketView model categoriesAndBuckets bucket =
    let
        moneyOp : Maybe MoneyOp
        moneyOp =
            IdDict.get bucket.id model.bucketMoneyOps
    in
    Html.div
        [ Attrs.class "group px-2 py-1 border bg-slate-100 flex justify-between hover:bg-sky-100" ]
        [ case IdDict.get bucket.id model.bucketRenameInputs of
            Nothing ->
                Html.div
                    [ Attrs.class "flex gap-2" ]
                    [ Html.div
                        [ Attrs.class "font-semibold" ]
                        [ Html.text bucket.name ]
                    , button
                        Inline
                        [ Events.onClick <| SetRenameBucketInput bucket.id bucket.name
                        , Attrs.class "invisible group-hover:visible"
                        ]
                        [ Icons.pencil ]
                    ]

            Just newName ->
                Html.div
                    [ Attrs.class "flex gap-1" ]
                    [ input
                        [ Events.onInput <| SetRenameBucketInput bucket.id
                        , Events.onEnter <| FinishRenamingBucket bucket.id
                        , Attrs.placeholder "New bucket name"
                        ]
                        newName
                    , button
                        Sky
                        [ Events.onClick <| CancelRenamingBucket bucket.id ]
                        [ Icons.xmark ]
                    , button
                        Sky
                        [ Events.onClick <| FinishRenamingBucket bucket.id
                        , Attrs.disabled <| String.isEmpty newName
                        ]
                        [ Icons.check ]
                    ]
        , Html.div
            [ Attrs.class "flex gap-2" ]
            [ valuePill
                (bucket.value
                    |> Money.complementToPositive
                    |> Money.toString
                    |> TakeFromM Nothing
                    |> StartMoneyOp (NormalBucket bucket.id)
                    |> Just
                )
                bucket.value
            , moneyOpView
                { startMoneyOp = StartMoneyOp <| NormalBucket bucket.id
                , selectOtherBucket = SelectMoneyOpOtherBucket <| NormalBucket bucket.id
                , setMoneyOpInput = SetMoneyOpInput <| NormalBucket bucket.id
                , finishMoneyOp = FinishMoneyOp <| NormalBucket bucket.id
                , cancelMoneyOp = CancelMoneyOp <| NormalBucket bucket.id
                , inputDomId = MoneyOpInput <| NormalBucket bucket.id
                , categoriesAndBuckets = categoriesAndBuckets
                , currentBucket = NormalBucket bucket.id
                , bucketName = getBucketName model
                }
                moneyOp
            , button
                Orange
                [ Events.onClick <| RemoveBucket bucket.id ]
                [ Icons.xmark ]
            ]
        ]


toBeBudgetedName : String
toBeBudgetedName =
    "To be budgeted"


getBucketName : Model -> BucketType -> String
getBucketName model bucketType =
    case bucketType of
        ToBeBudgeted ->
            toBeBudgetedName

        NormalBucket bucketId ->
            IdDict.get bucketId model.buckets
                |> Maybe.map .name
                |> Maybe.withDefault "BUG: can't find bucket name!"


moneyOpView :
    { startMoneyOp : MoneyOp -> msg
    , selectOtherBucket : BucketType -> msg
    , setMoneyOpInput : String -> msg
    , finishMoneyOp : msg
    , cancelMoneyOp : msg
    , inputDomId : DomId
    , categoriesAndBuckets : List ( Category, List Bucket )
    , currentBucket : BucketType
    , bucketName : BucketType -> String
    }
    -> Maybe MoneyOp
    -> Html msg
moneyOpView config moneyOp =
    let
        singleBucketView : String -> String -> String -> Html msg
        singleBucketView valueString label placeholder =
            Html.div
                [ Attrs.class "flex gap-1" ]
                [ Html.span [] [ Html.text label ]
                , input
                    [ Events.onInput config.setMoneyOpInput
                    , Events.onEnter config.finishMoneyOp
                    , Attrs.placeholder placeholder
                    , domId config.inputDomId
                    ]
                    valueString
                , button
                    Sky
                    [ Events.onClick config.cancelMoneyOp ]
                    [ Icons.xmark ]
                , button
                    Sky
                    [ Events.onClick config.finishMoneyOp
                    , Attrs.disabled <| not <| isValidNumber valueString
                    ]
                    [ Icons.check ]
                ]
    in
    case moneyOp of
        Nothing ->
            Html.div
                [ Attrs.class "flex gap-1 align-stretch" ]
                [ button
                    Sky
                    [ Events.onClick <| config.startMoneyOp <| SubtractM "" ]
                    [ Icons.minus ]
                , button
                    Sky
                    [ Events.onClick <| config.startMoneyOp <| AddM "" ]
                    [ Icons.plus ]
                , button
                    Sky
                    [ Events.onClick <| config.startMoneyOp <| SetM "" ]
                    [ Icons.equals ]
                , button
                    Sky
                    [ Events.onClick <| config.startMoneyOp <| MoveToM Nothing "" ]
                    [ Icons.arrowRight ]
                ]

        Just (SubtractM valueString) ->
            singleBucketView valueString
                "Subtracting:"
                "Amount to subtract"

        Just (AddM valueString) ->
            singleBucketView valueString
                "Adding:"
                "Amount to add"

        Just (SetM valueString) ->
            singleBucketView valueString
                "Setting to:"
                "Amount to set"

        Just (MoveToM targetBucket valueString) ->
            moneyInputAndBucketSelectView
                config
                { label = "Moving:"
                , placeholder = "Amount to move"
                , selectPlaceholder = "Move where? ▼"
                }
                targetBucket
                valueString

        Just (TakeFromM sourceBucket valueString) ->
            moneyInputAndBucketSelectView
                config
                { label = "Taking:"
                , placeholder = "Amount to take"
                , selectPlaceholder = "Take from where? ▼"
                }
                sourceBucket
                valueString


moneyInputAndBucketSelectView :
    { startMoneyOp : MoneyOp -> msg
    , selectOtherBucket : BucketType -> msg
    , setMoneyOpInput : String -> msg
    , finishMoneyOp : msg
    , cancelMoneyOp : msg
    , inputDomId : DomId
    , categoriesAndBuckets : List ( Category, List Bucket )
    , currentBucket : BucketType
    , bucketName : BucketType -> String
    }
    ->
        { label : String
        , placeholder : String
        , selectPlaceholder : String
        }
    -> Maybe BucketType
    -> String
    -> Html msg
moneyInputAndBucketSelectView config labels otherBucket valueString =
    let
        otherBucketMsgDecoder : Decoder msg
        otherBucketMsgDecoder =
            Decode.at [ "target", "value" ] bucketTypeDecoder
                |> Decode.map config.selectOtherBucket
    in
    Html.div
        [ Attrs.class "flex gap-1" ]
        [ Html.span [] [ Html.text labels.label ]
        , input
            [ Events.onInput config.setMoneyOpInput
            , Events.onEnter config.finishMoneyOp
            , Attrs.placeholder labels.placeholder
            , domId config.inputDomId
            ]
            valueString
        , Html.select
            [ Attrs.class "appearance-none border px-2 bg-sky-100 rounded border-sky-300 border hover:bg-sky-200 hover:border-sky-400"
            , Events.on "change" otherBucketMsgDecoder
            ]
            (Html.option
                [ Attrs.disabled True
                , Attrs.selected <| otherBucket == Nothing
                ]
                [ Html.text labels.selectPlaceholder ]
                :: Html.option
                    [ Attrs.selected <| otherBucket == Just ToBeBudgeted
                    , Attrs.disabled <| otherBucket == Just ToBeBudgeted || config.currentBucket == ToBeBudgeted
                    , Attrs.value <| bucketTypeToString ToBeBudgeted
                    ]
                    [ Html.text toBeBudgetedName ]
                :: List.concatMap
                    (otherBucketOptionsView
                        { currentBucket = config.currentBucket
                        , selectedOtherBucket = otherBucket
                        , bucketName = config.bucketName
                        }
                    )
                    config.categoriesAndBuckets
            )
        , button
            Sky
            [ Events.onClick config.cancelMoneyOp ]
            [ Icons.xmark ]
        , button
            Sky
            [ Events.onClick config.finishMoneyOp
            , Attrs.disabled <| not <| isValidNumber valueString
            ]
            [ Icons.check ]
        ]


isValidNumber : String -> Bool
isValidNumber valueString =
    case Money.fromString valueString of
        Just money ->
            not (Money.isNegative money)

        Nothing ->
            False


type ButtonStyle
    = Sky
    | Orange
    | Inline


buttonStyle : ButtonStyle -> Attribute msg
buttonStyle style =
    case style of
        Sky ->
            Attrs.class "rounded border bg-sky-200 border-sky-400 text-sky-700 hover:bg-sky-300 hover:border-sky-500"

        Orange ->
            Attrs.class "rounded border bg-orange-200 border-orange-400 text-orange-700 hover:bg-orange-300 hover:border-orange-500"

        Inline ->
            Attrs.class "text-gray-400 hover:text-gray-600"


button : ButtonStyle -> List (Attribute msg) -> List (Html msg) -> Html msg
button style attrs contents =
    Html.button
        (Attrs.class "px-2 flex flex-row gap-1 items-center disabled:cursor-not-allowed disabled:bg-gray-200 disabled:border-gray-400 disabled:text-gray-400 disabled:hover:bg-gray-300"
            :: buttonStyle style
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


valuePill : Maybe msg -> Money -> Html msg
valuePill onClick money =
    let
        color =
            if Money.isNegative money then
                Attrs.class "bg-red-200 border-red-400 text-red-600 hover:bg-red-300 hover:border-red-500"

            else
                Attrs.class "bg-lime-200 border-lime-400 text-lime-600 hover:bg-lime-300 hover:border-lime-500"
    in
    Html.div
        [ Attrs.class "rounded px-2 border"
        , color
        , onClick
            |> Maybe.map Events.onClick
            |> Maybe.withDefault Attrs.empty
        , onClick
            |> Maybe.map (\_ -> "cursor-pointer")
            |> Maybe.withDefault ""
            |> Attrs.class
        , onClick
            |> Maybe.map (\_ -> Attrs.title "Click to select where to take money from.")
            |> Maybe.withDefault Attrs.empty
        ]
        [ Html.text <| Money.toString money ++ " Kč" ]
