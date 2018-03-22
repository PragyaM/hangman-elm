module Hangman exposing (init, newGame, view, update, subscriptions, Model, Msg)

import Html exposing (text, input, button, div, h1, form, span)
import Html.Events exposing (onInput, onSubmit, onClick)
import Html.Attributes exposing (type_, placeholder)
import Array
import Dict exposing (Dict)


-- MODEL


type alias Model =
    { secretWord : String
    , secretWordLetters : Dict String Bool
    , livesRemaining : Int
    , guessedLetters : List String
    , currentGuessLetter : Maybe String
    , currentGuessOutcome : Maybe GuessOutcome
    }


init : ( Model, Cmd Msg )
init =
    newGame "word" ! [ Cmd.none ]


newGame : String -> Model
newGame word =
    let
        wordDict =
            Dict.fromList (List.map2 (,) (String.split "" word) (List.repeat (String.length word) False))
    in
        { secretWord = word
        , secretWordLetters = wordDict
        , livesRemaining = 5
        , guessedLetters = []
        , currentGuessLetter = Nothing
        , currentGuessOutcome = Nothing
        }



-- currentGame : Model


randomWord : String
randomWord =
    "Hello"


type Msg
    = SubmitGuess
    | Restart
    | InputGuess String


type GameState
    = InProgress
    | Won
    | Lost



-- VIEW


view : Model -> Html.Html Msg
view model =
    div []
        [ h1 [] [ text "Welcome to Hangman" ]
        , div [ hiddenWordStyle ] [ text ("Word to guess: " ++ (obfuscatedWord model)) ]
        , div [] [ text ("Lives remaining: " ++ (toString model.livesRemaining)) ]
        , guessInputField model
        , button [ onClick SubmitGuess ] [ text "Submit" ]
        , div [] [ text ("Guessed letters: " ++ (toString model.guessedLetters)) ]
        , guessOutcomeOutput model
        , gameStateOutput model
        , button [ onClick Restart ] [ text "New Game" ]
        ]


hiddenWordStyle =
    Html.Attributes.style
        [ ( "font-size", "2em" )
        , ( "letter-spacing", "5px" )
        ]


guessOutcomeOutput : Model -> Html.Html Msg
guessOutcomeOutput model =
    case model.currentGuessOutcome of
        Nothing ->
            div [] []

        Just Success ->
            div [] [ text "Successful guess! HIGH FIVE." ]

        Just Invalid ->
            div [] [ text "That was not a valid guess! Try entering a letter from the alphabet this time." ]

        Just Duplicate ->
            div [] [ text "You have already guessed that!" ]

        Just Fail ->
            div [] [ text "WRONG! Try again." ]


gameStateOutput : Model -> Html.Html Msg
gameStateOutput model =
    case currentGameState model of
        InProgress ->
            div [] []

        Won ->
            div [] [ text "YOU WON!!!!!!!!! Nice." ]

        Lost ->
            div [] [ text "He dead. You lose." ]


obfuscatedWord : Model -> String
obfuscatedWord { secretWord, secretWordLetters, guessedLetters } =
    String.map
        (\letter ->
            if List.member (String.fromChar letter) guessedLetters then
                letter
            else
                '_'
        )
        secretWord


guessInputField : Model -> Html.Html Msg
guessInputField model =
    form [] [ input [ type_ "text", placeholder "Enter guess", onInput InputGuess ] [] ]



-- UPDATE


updateLivesRemaining : Int -> GuessOutcome -> Int
updateLivesRemaining livesRemaining guessOutcome =
    if guessOutcome == Fail then
        livesRemaining - 1
    else
        livesRemaining


inputGuess : String -> Model -> Model
inputGuess guessString model =
    let
        guessLetter =
            Array.get 0 ((Array.fromList (String.split "" guessString)))
    in
        { model | currentGuessLetter = guessLetter }


submitGuess : Model -> Model
submitGuess model =
    case model.currentGuessLetter of
        Nothing ->
            model

        Just letter ->
            let
                guessOutcome =
                    outcomeForGuess letter model
            in
                { model
                    | secretWordLetters = Dict.update letter (secretWordLettersDictUpdate True) model.secretWordLetters
                    , currentGuessOutcome = Just guessOutcome
                    , livesRemaining = updateLivesRemaining model.livesRemaining (outcomeForGuess letter model)
                    , guessedLetters = model.guessedLetters ++ [ letter ]
                }


type GuessOutcome
    = Success
    | Fail
    | Invalid
    | Duplicate


outcomeForGuess : String -> Model -> GuessOutcome
outcomeForGuess letter model =
    if List.member letter model.guessedLetters then
        Duplicate
        -- they have already guessed this letter
    else if Dict.member letter model.secretWordLetters then
        Success
        -- successful guess!
    else
        Fail



-- unsuccessful guess


secretWordLettersDictUpdate : Bool -> Maybe Bool -> Maybe Bool
secretWordLettersDictUpdate newVal maybeVal =
    case maybeVal of
        Nothing ->
            Nothing

        Just val ->
            Just newVal


currentGameState : Model -> GameState
currentGameState model =
    if List.foldr (&&) True (Dict.values model.secretWordLetters) then
        Won
    else if model.livesRemaining == 0 then
        Lost
    else
        InProgress



-- letterInWordHasBeenGuessed : String -> Model -> Bool
-- letterInWordHasBeenGuessed letter model =
--   contains letter model.secretWordLetters


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputGuess string ->
            (( inputGuess string model, Cmd.none ))

        SubmitGuess ->
            (( submitGuess model, Cmd.none ))

        Restart ->
            (( newGame "waterworld", Cmd.none ))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
