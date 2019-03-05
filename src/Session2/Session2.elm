module Session2.Session2 exposing(..)

import String

-- Map one structure to another
-- > convert [{name="John", email="john@gmail.com", phone_number="+3801234567"}]
-- [{name="John", email="john@gmail.com"}]
convert 
  : List { name : String, email : String, phone_number : String}
  -> List { name : String, email : String}
convert xs =
  let
    convertOne x =
      { name = x.name, email = x.email}
  in
    List.map convertOne xs

-- Filter elements with non-empty name and email
-- > convert02 [{name=Just "John", email=Just "john@gmail.com"}, {name=Just "John", email=Nothing}, {name=Just "Ivan", email=Just "ivan@gmail.com"}]
-- [{name="John", email="john@gmail.com"}, {name=Just "Ivan", email="ivan@gmail.com}]
convert02 
  : List { name : Maybe String, email : Maybe String} 
  -> List { name : String, email : String}
convert02 xs =
  let
    appendIfNonEmpty i a =
      case i of
        Just x ->
          a ++ [x]
        Nothing ->
          a
    toMaybe {name, email} =
      case (name, email) of
        (Just a, Just b) ->
          Just { name = a, email = b }
        (_, _) ->
          Nothing
  in
    xs
      |> List.map toMaybe
      |> List.foldl appendIfNonEmpty []

-- Fill in missing emails with <unspecified>, while removing elements with no name
-- > convert03 [{name=Just "John", email=Just "john@gmail.com"}, {name=Just "Jack", email=Nothing}, {name=Nothing, email=Just "hello@world.com"}]
-- [{name="John", email="john@gmail.com"}, {name="Jack", email="<unspecified>"}]
convert03 
  : List { name : Maybe String, email : Maybe String} 
  -> List { name : String, email : String} 
convert03 xs =
  let
    appendIfNonEmpty i a =
      case i of
        Just x ->
          a ++ [x]
        Nothing ->
          a

    toMaybe {name, email} =
      case (name, email) of
        (Just a, Just b) ->
          Just { name = a, email = b }
        (_, _) ->
          Nothing

    withMarkedEmail a =
      if a.email == Nothing then
        { a | email = Just "<unspecified>" }
      else
        a
  in
    xs
      |> List.map withMarkedEmail
      |> List.map toMaybe
      |> List.foldl appendIfNonEmpty []


-- Rewrite bird using <|, then using |> instead of parens (where applicable)
bird : Int
bird =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    List.sum (List.filter notThree (List.map incr [ 1, 2, 3 ]))

-- using <|
bird2 : Int
bird2 =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
        List.sum
            <| List.filter notThree
            <| List.map incr
            <| [ 1, 2, 3 ]

-- using |>
bird3 =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
        [1, 2, 3]
            |> List.map incr
            |> List.filter notThree
            |> List.sum
