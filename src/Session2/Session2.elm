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
