module Main exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events as Events exposing (..)
import Css exposing (..)
import Mouse exposing (..)
import Json.Decode as Json
import Dom


-- MODEL


type PlanType
    = Available
    | NotAvailable


type alias PlannerPosition =
    { hour: Int
    , division: Int
    }


type alias PlannerRange =
    { start : PlannerPosition
    , end : PlannerPosition
    }


type alias Plan =
    { range : PlannerRange
    , planType : PlanType
    }


type PlannerSelection
    = BrushSelected PlannerRange
    | Brushing PlannerRange
    | NoSelection
    | PlanDragging Plan PlannerPosition PlannerPosition
    | PlanSelected Plan


type alias History a =
  { states : List a
  , cursor : Int
  }


type alias Model =
    { plannerHourStart : Int
    , plannerHourEnd : Int
    , hourDivisions : Int
    , plans : History (List Plan)
    , selection : PlannerSelection
    }


toPosition : Int -> Int -> PlannerPosition
toPosition hour division = { hour = hour, division = division }

(=/=) = toPosition

toRange : PlannerPosition -> PlannerPosition -> PlannerRange
toRange start end = { start = start, end = end }

(=>=) = toRange

model : Model
model =
    { plannerHourStart = 8
    , plannerHourEnd = 24
    , hourDivisions = 4
{-  , plans = { states = [ [ { planType = Available
                             , range = (9 =/= 2) =>= (12 =/= 4)
                             }
                           , { planType = NotAvailable
                             , range = (21 =/= 1) =>= (23 =/= 2)
                             } ]
                         , [ { planType = NotAvailable
                             , range = (9 =/= 2) =>= (12 =/= 4)
                             }
                           , { planType = NotAvailable
                             , range = (21 =/= 1) =>= (23 =/= 2)
                             } ]
                         , [ { planType = NotAvailable
                             , range = (9 =/= 2) =>= (12 =/= 4)
                             } ]
                         ]
              , cursor = 0
              } -}
    , plans = { states = [ [] ], cursor = 0 }
    , selection = NoSelection
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


updatePlanToHistory : History (List Plan) -> Plan -> Plan -> History (List Plan)
updatePlanToHistory history prevPlan newPlan =
  let
    -- Salvo in prevPlans l'ultima lista di Plans
    prevPlans = (getCurrentPlans history)
    states = history.states
    cursor = history.cursor
    -- Concateno il nuovo Plan col oldPlans:
    nextPlans = prevPlans
        |> List.map (\plan ->
            if plan == prevPlan
            then newPlan
            else plan
            )
    -- Drop toglie i primi cursor element da history.states
    nextStates = (List.drop cursor states)
  in
    { states = nextPlans :: nextStates
    , cursor = 0
    }

addPlanToHistory : History (List Plan) -> Plan -> History (List Plan)
addPlanToHistory history plan =
  let
    -- Salvo in prevPlans l'ultima lista di Plans
    prevPlans = (getCurrentPlans history)
    states = history.states
    cursor = history.cursor
    -- Concateno il nuovo Plan col oldPlans:
    nextPlans = plan :: prevPlans
    -- Drop toglie i primi cursor element da history.states
    nextStates = (List.drop cursor states)
  in
    { states = nextPlans :: nextStates
    , cursor = 0
    }


get : Int -> List a -> Maybe a
get n list = list
  |> List.drop n
  |> List.head

getCurrentPlans : History (List Plan) -> List Plan
getCurrentPlans history = history.states
  |> get history.cursor
  |> Maybe.withDefault []


-- MESSAGES

type Msg
    = Increment
    | Decrement
  --| SelectPlan Plan
    | BrushStart PlannerPosition
    | MouseAt PlannerPosition
    | BrushEnd PlannerPosition
    | BrushCancel
    | BrushSave PlanType
    | RemoveAll
    | HistoryUndo
    | HistoryRedo
    | DragStart Plan PlannerPosition
  --| DragAt PlannerPosition
    | DragEnd PlannerPosition


-- VIEW

planTypeToColor : PlanType -> String
planTypeToColor planType =
    case planType of
        Available -> "#09f"
        NotAvailable -> "#e30"

thStyle : Attribute msg
thStyle =
    style
        [
          ("background", "#f0f0f0")
        , ("border-radius", "4px")
        , ("padding", "20px")
        , ("text-align", "center")
        , ("margin", "2px")
        , ("width","24px")
        ]

trStyle : Attribute msg
trStyle =
    style
        [
          ("border", "2px solid black")
        ]

hourDivisionStyleRules = [ ( "border-radius", "4px" )
                         , ( "padding-top", "20px" )
                         , ( "padding-bottom", "20px" )
                         ]

hourDivisionStyle = style ([ ( "background", "#eee" ) ] ++ hourDivisionStyleRules)

dynamicHourDivisionStyle planType selected =
                    style [ ( "background", planTypeToColor planType )
                          , ( "opacity", (if selected then "1.0" else "0.6") )
                          , ( "border-radius", "4px" )
                          , ( "padding-top", "20px" )
                          , ( "padding-bottom", "20px" )
                          , ( "text-align", "center" )
                          , ( "color", "darkgrey" )
                          ]

hourDivisionSelectedStyle model =
  dynamicHourDivisionStyle Available True

hourDivisionAvailableStyle =
  dynamicHourDivisionStyle Available False

hourDivisionNotAvailableStyle =
  dynamicHourDivisionStyle NotAvailable False

hourDivisionAvailableSelectedStyle =
  dynamicHourDivisionStyle Available True

hourDivisionNotAvailableSelectedStyle =
  dynamicHourDivisionStyle NotAvailable True

onBrushOptions = { stopPropagation = True
                 , preventDefault = True }

onMouseAt    position = onWithOptions "mousemove" onBrushOptions ( Json.succeed ( MouseAt position    ) )

onBrushStart position = onWithOptions "mousedown" onBrushOptions ( Json.succeed ( BrushStart position ) )
onBrushEnd   position = onWithOptions "mouseup"   onBrushOptions ( Json.succeed ( BrushEnd position   ) )

onDragStart plan position = onWithOptions "mousedown" onBrushOptions ( Json.succeed ( DragStart plan position ) )
onDragEnd   position      = onWithOptions "mouseup"   onBrushOptions ( Json.succeed ( DragEnd position   ) )

comparePositions : PlannerPosition -> PlannerPosition -> Order
comparePositions left right =
  let hourOrder = compare left.hour right.hour
  in case hourOrder of
    EQ -> compare left.division right.division
    _ -> hourOrder

isPositionInRange : PlannerPosition -> PlannerRange -> Bool
isPositionInRange position range =
  let comp = comparePositions position
  in case ( (comp range.start), (comp range.end) ) of
    ( EQ , _  ) -> True
    ( _  , EQ ) -> True
    ( GT , LT ) -> True
    ( LT , GT ) -> True -- required to make it work in reverse!
    _ -> False

{-
sanitizeRange : PlannerRange -> PlannerRange --Compare start and end, if start is greater, inverted.
sanitizeRange range =
  case comparePositions range.start range.end of
    EQ -> range
    LT -> range
    GT -> range.end =>= range.start
-}

isPositionInRanges : PlannerPosition -> List PlannerRange -> Bool
isPositionInRanges position ranges =
  List.any (isPositionInRange position) ranges


getSelectedPlan : PlannerSelection -> Maybe Plan
getSelectedPlan selection =
  case selection of
    PlanSelected plan -> Just plan
    _ -> Nothing

getSelectedShiftedPlan : Int -> PlannerSelection -> Maybe Plan
getSelectedShiftedPlan hourDivisions selection =
  case selection of
    PlanDragging plan from to ->
        Just { plan | range = (getDraggingPlan hourDivisions plan.range from to) }
    _ -> Nothing


getBrushRange : PlannerSelection -> Maybe PlannerRange
getBrushRange selection =
  case selection of
    BrushSelected range -> Just range
    Brushing range -> Just range
    _ -> Nothing

toAbsolute : Int -> PlannerPosition -> Int
toAbsolute hourDivisions position =
    ((position.hour * hourDivisions) + position.division)

fromAbsolute : Int -> Int -> PlannerPosition --da minuti a ore contrario di toAbsolute
fromAbsolute hourDivisions abspos =
    (abspos // hourDivisions) =/= (abspos % hourDivisions)

getDelta : Int -> PlannerPosition -> PlannerPosition -> Int
getDelta hourDivisions from to =
    (to |> toAbsolute hourDivisions) - (from |> toAbsolute hourDivisions)

getDraggingPlan : Int -> PlannerRange -> PlannerPosition -> PlannerPosition -> PlannerRange
getDraggingPlan hourDivisions range from to =
  let
    delta = getDelta hourDivisions from to
    --deltaHours = delta // hourDivisions
    --deltaDivisions = delta % hourDivisions
    fromAbs = fromAbsolute hourDivisions
    toAbs = toAbsolute hourDivisions
  in
    (fromAbs ((toAbs range.start) + delta))
    =>=
    (fromAbs ((toAbs range.end) + delta))

  {--let
    deltaHour = to.hour - from.hour
    deltaDivision = to.division - from.division
    newStartHour = range.start.hour + deltaHour
    newStartDivision = range.start.division + deltaDivision
    newEndHour = range.end.hour + deltaHour
    newEndDivision = range.end.division + deltaDivision
  in
      if (newStartDivision >= hourDivisions) then
          ((newStartHour + 1) =/= (newStartDivision - hourDivisions)) =>= (newEndHour =/= newEndDivision)
      else if (newEndDivision >= hourDivisions) then
          (newStartHour =/= newStartDivision) =>= ((newEndHour + 1) =/= (newEndDivision - hourDivisions))
      else if (newStartDivision < 0) then
          ((newStartHour - 1) =/= (newStartDivision + hourDivisions)) =>= (newEndHour =/= newEndDivision)
      else if (newEndDivision < 0) then
          (newStartHour =/= newStartDivision) =>= ((newEndHour - 1) =/= (newEndDivision + hourDivisions))
      else
          (newStartHour =/= newStartDivision) =>= (newEndHour =/= newEndDivision)
  --}


planChecker : PlannerPosition -> Plan -> Maybe Plan -> Maybe Plan
planChecker position plan accumulator =
    case accumulator of
      Nothing -> if isPositionInRange position plan.range
                  then Just plan
                  else Nothing
      _ -> accumulator

keepIf : (a -> Bool) -> Maybe a -> Maybe a
keepIf tester wrapper = case wrapper of
    Just v -> if (tester v) then wrapper
                            else Nothing
    _ -> wrapper

deletePlan : List Plan -> Plan -> List Plan
deletePlan plans plan =
  --Tuple.second (List.partition (\a -> a == plan) plans)
  plans |> List.filter ((/=) plan)


renderDivisionCell : PlannerSelection -> List Plan -> Int -> PlannerPosition -> Html Msg
renderDivisionCell selection plans hourDivisions position =
  let
    brushStarterListeners = [ onBrushStart position
                            , onMouseAt position
                            , onBrushEnd position
                            ]

    brushListeners = [ onBrushEnd position ]

    dragListeners = [ onDragEnd position ]

    brushRange = selection |> getBrushRange --brushRange sarà type BrushSelected OR Brushing OR Nothing

    planTypeToStyle selected plan =
        brushListeners ++ dragListeners ++
        [ onMouseAt position
        , onDragStart plan position
        , if selected ||
              ( selection
                  |> getSelectedPlan
                  |> Maybe.map ((==) plan)
                  |> Maybe.withDefault False
              )
          then case plan.planType of
            Available -> hourDivisionAvailableSelectedStyle
            NotAvailable -> hourDivisionNotAvailableSelectedStyle
          else case plan.planType of
            Available -> hourDivisionAvailableStyle
            NotAvailable -> hourDivisionNotAvailableStyle
        ]

    attrs =
            --if Maybe.withDefault False ( (Maybe.map (isPositionInRange position) brushRange) )
            --then brushStarterListeners ++ [ hourDivisionSelectedStyle model ]
            if brushRange --Se questo è type BrushSelected OR Brushing allora applico la funzione sotto
                |> Maybe.map (isPositionInRange position) --Tale funzione prende il range ritornato da Just e lo passa a isPositionInRange come 2 parametro
                |> Maybe.withDefault False --Se brushRange è Nothing ritorno False
            then brushStarterListeners ++
                [ onMouseAt position
                , hourDivisionSelectedStyle model ] --Se le position sono giuste ritorno True e applico questo style

            else selection
                |> getSelectedShiftedPlan hourDivisions
                |> keepIf (\plan -> isPositionInRange position plan.range)
                |> Maybe.map (planTypeToStyle True)
                |> Maybe.withDefault
                    (plans
                        -- Restituisce tutti i plan che non sono in lista
                        |> List.foldl (planChecker position) Nothing
                        -- ad ognuno dei quali applico lo style
                        |> Maybe.map (planTypeToStyle False)
                        -- Caso Nothing viene applicato lo style di default
                        |> Maybe.withDefault (
                                brushStarterListeners ++
                                dragListeners ++
                                [ onMouseAt position
                                , hourDivisionStyle
                                ]
                            )
                    )
              --Maybe.withDefault (brushStarterListeners ++ [ hourDivisionStyle ]) (Maybe.map planType (List.foldl (planChecker position) Nothing plans))


  in td attrs []

renderDivisionCells : PlannerSelection -> List Plan -> Int -> Int ->  List (Html Msg)
renderDivisionCells selection plans hour hourDivisions =
  List.range 0 (hourDivisions - 1)
    |> List.map (\division -> hour =/= division)
    |> List.map (renderDivisionCell selection plans hourDivisions)


debugSelection hourDivisions selection = case selection of
  NoSelection -> []
  BrushSelected range -> debugSelectionRange range
  Brushing range -> debugSelectionRange range
  PlanDragging plan from to ->
      [ text "DRAGGING!" ] ++
      [ text " movement:" ] ++
      (debugSelectionRange (from =>= to)) ++
      [ text " source:" ] ++
      (debugSelectionRange plan.range) ++
      [ text " result:" ] ++
      (debugSelectionRange (getDraggingPlan hourDivisions plan.range from to))
  PlanSelected plan -> [ text "Plan selected" ]


debugSelectionRange range = [ text "("
                           , text ( toString (range.start.hour) )
                           , text "=/="
                           , text ( toString (range.start.division) )
                           , text ")=>=("
                           , text ( toString (range.end.hour) )
                           , text "=/="
                           , text ( toString (range.end.division) )
                           , text ")"
                           ]


view : Model -> Html Msg
view model =
    div []
        [
              div []
                  ([ button [ onClick Decrement, Html.Styled.Attributes.disabled (model.hourDivisions == 1) ] [ text "-" ]
                  , text (toString model.hourDivisions)
                  , button [ onClick Increment ] [ text "+" ]
                  ]++ (case model.selection of
                      NoSelection -> [  text " No brush selected!" ]
                      _ -> debugSelection model.hourDivisions model.selection))

              , Html.Styled.table []
                [ thead []
                      [
                       tr []
                          (List.map
                            (\hour -> td [ colspan model.hourDivisions, thStyle] [ text (toString hour) ])
                            (List.range model.plannerHourStart model.plannerHourEnd))
                      ]--thead

                  , tbody []
                          [
                          tr []
                              [
                                td [ colspan (model.hourDivisions * (model.plannerHourEnd - model.plannerHourStart + 1)) ]
                                   [ button [ onClick (BrushSave Available) ] [ text "Disponibile" ]
                                   , button [ onClick (BrushSave NotAvailable) ] [ text "Non disponibile" ]
                                   , button [ onClick BrushCancel ] [ text "Annulla" ]
                                   , button [ style [("background-color", "red")], onClick RemoveAll ] [ text "Cancella" ]
                                   , button [ onClick HistoryUndo
                                            , Html.Styled.Attributes.disabled
                                                -- We need to keep in mind the last element in the states
                                                -- is actually the "first commit"
                                                (model.plans.cursor == (List.length model.plans.states) - 1)
                                            ]
                                            [ text "UNDO" ]
                                   , button [ onClick HistoryRedo
                                            , Html.Styled.Attributes.disabled (model.plans.cursor == 0)
                                            ]
                                            [ text "REDO" ]
                                   ]
                              ]
                         , tr []
                           (List.concatMap
                             (\hour -> renderDivisionCells model.selection (getCurrentPlans model.plans) hour model.hourDivisions)
                             (List.range model.plannerHourStart model.plannerHourEnd))
                          ]--/tbody
            ] --/table
        ] --/div



-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

      Increment ->
          ({ model | hourDivisions = model.hourDivisions + 1 }
          , Cmd.none
          )

      Decrement ->
          ({ model | hourDivisions = model.hourDivisions - 1 }
          , Cmd.none
          )

      {--SelectPlan plan ->
          ({ model | selection = PlanSelected plan }
          , Cmd.none
          )--}

      BrushStart position ->
          ({ model | selection = Brushing (position =>= position) }
          , Cmd.none
          )


      MouseAt position ->
          case model.selection of
            Brushing selection ->
              ({ model | selection = Brushing { selection | end = position } }
              , Cmd.none
              )
            PlanDragging plan from to ->
                ({ model | selection = PlanDragging plan from position }
                , Cmd.none
                )
            _ -> ( model, Cmd.none )


      BrushEnd position ->
          case model.selection of
            Brushing selection ->
              ( { model | selection = BrushSelected { selection | end = position } }
              , Cmd.none
              )
            _ -> ( model, Cmd.none )


      BrushCancel ->
          ( { model | selection = NoSelection }, Cmd.none )


      RemoveAll ->
          ( { model | plans = { states = [ [] ]
                              , cursor = 0
                              }
                    , selection = NoSelection
            }, Cmd.none )


      HistoryUndo ->
          let plans = model.plans
          in ( { model | plans = { plans | cursor = ( Basics.min ( ( List.length model.plans.states ) - 1 ) ( model.plans.cursor + 1 ) ) } }
             , Cmd.none
             )


      HistoryRedo ->
          let plans = model.plans
          in ( { model | plans = { plans | cursor = ( Basics.max 0 ( model.plans.cursor - 1 ) ) } }
               , Cmd.none
               )


      BrushSave planType ->
          case model.selection of
              BrushSelected range -> ( { model | selection = NoSelection
                                       , plans = addPlanToHistory model.plans { planType = planType, range = range }
                                       }
                                       , Cmd.none )
              _ -> ( model, Cmd.none )


      DragStart plan position ->
          ({ model | selection = PlanDragging plan position position }
          , Cmd.none
          )


      DragEnd position ->
          case model.selection of
              PlanDragging plan from to ->
                  let
                    newPlan = { planType = plan.planType
                              , range = (getDraggingPlan model.hourDivisions plan.range from to)
                              }
                  in
                      ( if 0 == (getDelta model.hourDivisions from to)
                        then { model | selection = PlanSelected plan }
                        else { model | selection = PlanSelected newPlan
                                     , plans = updatePlanToHistory model.plans plan newPlan
                                     , selection = NoSelection --?
                                     }
                      , Cmd.none
                      )
              _ -> ( model, Cmd.none )



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
      Sub.batch
        [
        ]


-- MAIN

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
