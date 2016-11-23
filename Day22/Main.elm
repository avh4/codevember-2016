module Main exposing (..)

import Html exposing (Html)
import Combine exposing ((<$>), (<*>), (<*), (*>))
import TreeDiagram
import DrawTree


testSentence1 : String
testSentence1 =
    "The whin was frankincense and flame."


testSentence2 : String
testSentence2 =
    "Now I have seen the furze wren at his very best!"


type alias Tree =
    TreeDiagram.Tree String


node : String -> List Tree -> Tree
node =
    TreeDiagram.node


nounPhrase : Combine.Parser () Tree
nounPhrase =
    (\result -> node result [])
        <$> Combine.choice
                [ Combine.string "The whin"
                , Combine.string "frankincense and flame"
                , Combine.string "I"
                , Combine.string "the furze wren"
                , Combine.string "his very best"
                ]


verb : Combine.Parser () String
verb =
    Combine.choice
        [ Combine.string "was"
        , Combine.string "have seen"
        ]


whitespace : Combine.Parser () String
whitespace =
    Combine.string " "


timePhrase : Combine.Parser () String
timePhrase =
    Combine.string "Now"


prepositionalPhrase : Combine.Parser () Tree
prepositionalPhrase =
    (\prep noun -> node prep [ noun ])
        <$> (Combine.string "at" <* whitespace)
        <*> nounPhrase


sentenceParser : Combine.Parser () Tree
sentenceParser =
    Combine.choice
        [ (\subject verb object preps ->
            node verb
                ([ subject, object ] ++ preps)
          )
            <$> nounPhrase
            <*> (whitespace *> verb <* whitespace)
            <*> nounPhrase
            <*> Combine.many (whitespace *> prepositionalPhrase)
        , (\time rest -> node time [ rest ])
            <$> (timePhrase <* whitespace)
            <*> Combine.lazy (\() -> sentenceParser)
        ]


parse : String -> Result ( String, List String ) Tree
parse input =
    case Combine.parse sentenceParser input of
        Err ( _, inputStream, messages ) ->
            Err ( inputStream.input, messages )

        Ok ( _, _, result ) ->
            Ok result


main : Html msg
main =
    case parse testSentence2 of
        Ok tree ->
            tree
                |> DrawTree.draw

        Err ( remaining, messages ) ->
            Html.div []
                [ Html.b [] [ Html.text <| "Couldn't parse: " ++ remaining ]
                , messages
                    |> List.map (\m -> Html.li [] [ Html.text m ])
                    |> Html.ul []
                ]
