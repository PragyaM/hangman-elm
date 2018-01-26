module Main exposing ( .. )

import Html
import Hangman


main : Program Never Hangman.Model Hangman.Msg
main =
  Html.program
    { init = Hangman.init
    , view = Hangman.view
    , update = Hangman.update
    , subscriptions = Hangman.subscriptions
    }
