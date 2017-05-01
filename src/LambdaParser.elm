module LambdaParser exposing (..)

import Lambda exposing (..)
import Combo exposing (..)


name : Parser Expression
name =
    manyS1 alpha <$> Name


function : Parser Expression
function =
    return Function
        |> andMap (char 'λ' *> manyS1 alpha)
        |> andMap (char '.' *> expression)


application : Parser Expression
application =
    return Application
        |> andMap (char '(' *> expression <* char ' ')
        |> andMap (expression <* char ')')


{-| Avoid bad recursion by using a lazy parser.
-}
expression : Parser Expression
expression =
    lazy <| \() -> name <|> function <|> application
