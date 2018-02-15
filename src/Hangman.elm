module Hangman exposing ( init, newGame, view, update, subscriptions, Model, Msg )

import Html exposing ( text, input, button, div, h1, form, span )
import Html.Events exposing ( onInput, onSubmit, onClick )
import Html.Attributes exposing ( type_, placeholder )
import Array

-- MODEL

type alias Model =
  { secretWord : Dict Char Bool
  , livesRemaining : Int
  , guessedLetters : List Char
  , currentGuess : String
  , obfuscatedWord : String
  , gameState : GameState
  }

init : (Model, Cmd Msg)
init =
  newGame "word" ! [Cmd.none]

newGame : String -> Model
newGame word =
  let wordDict = List.map2 (,) (String.toList word) (List.repeat (String.length word) False)
  in
    Model wordDict 5 [] "" "obfuscated word" InProgress

-- currentGame : Model

randomWord : String
randomWord = "Hello"

type Msg = SubmitGuess String
         | Restart
         | InputGuess String

type GameState = InProgress
               | Won
               | Lost

-- VIEW
view : Model -> Html.Html Msg
view model =
  div []
    [ h1 [] [text "Welcome to Hangman"]
    , div [] [text ("Word to guess: " ++ (toString model.obfuscatedWord))]
    , div [] [text ("Lives remaining: " ++ (toString model.livesRemaining))]
    , span [] [guessInputField model
              , button [onClick (SubmitGuess model.currentGuess)] [text "Submit"]
              ]
    , div [] [text ("Guessed letters: " ++ (toString model.guessedLetters))]
    , button [onClick Restart] [text "New Game"]
    ]

-- obfuscatedWord : (String, List Char) -> String
-- obfuscatedWord word lettersToShow =
--   "obfuscated word"

guessInputField : Model -> Html.Html Msg
guessInputField model =
  form [] [input [type_ "text", placeholder "Enter guess", onInput InputGuess] []]


-- UPDATE


deductOneLife : Model -> Model
deductOneLife model =
  { model | livesRemaining = model.livesRemaining - 1 }

submitGuess : (String, Model) -> Model
submitGuess (letter, model) =
  let charLetter = Array.get 0 ((Array.fromList (String.toList letter)))
  in
    case charLetter of
      Nothing ->
        model
      Just characterLetter ->
        { model | guessedLetters = model.guessedLetters ++ [characterLetter] }

type GuessOutcome = Success
                  | Fail
                  | Invalid
                  | Duplicate

isGuessSuccessful : String -> Model -> GuessOutcome
isGuessSuccessful letter model =
  let charLetter = Array.get 0 ((Array.fromList (String.toList letter)))
  in
    if List.member charLetter (List.map Maybe.Just model.guessedLetters) then
      Duplicate -- they have already guessed this letter
    else if Dict.member letter model.secretWord then
      Success -- successful guess!
    else
      Fail -- unsuccessful guess

updateGameState : Model -> Model
updateGameState model =
  if List.foldr (&&) True model.sercretWord.values then
    { model | gameState = GameState.Won }

-- letterInWordHasBeenGuessed : String -> Model -> Bool
-- letterInWordHasBeenGuessed letter model =
--   contains letter model.secretWord


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InputGuess string -> (
      ({ model | currentGuess = string }, Cmd.none)
    )
    SubmitGuess guess -> (
      (submitGuess(guess, model), Cmd.none)
    )
    UpdateGameState -> (
      (updateGameState(model), Cmd.none)
    )
    Restart ->
      (newGame "another word", Cmd.none)



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
