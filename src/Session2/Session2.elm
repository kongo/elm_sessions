module Session2.Session2 exposing(..)

import String
import Url.Builder exposing(..)

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
    toMaybe {name, email} =
      case (name, email) of
        (Just a, Just b) ->
          Just { name = a, email = b }
        (_, _) ->
          Nothing
  in
    xs
      |> List.map toMaybe
      |> catMaybes

-- Fill in missing emails with <unspecified>, while removing elements with no name
-- > convert03 [{name=Just "John", email=Just "john@gmail.com"}, {name=Just "Jack", email=Nothing}, {name=Nothing, email=Just "hello@world.com"}]
-- [{name="John", email="john@gmail.com"}, {name="Jack", email="<unspecified>"}]
convert03 
  : List { name : Maybe String, email : Maybe String} 
  -> List { name : String, email : String} 
convert03 xs =
  let
    withMarkedEmail a =
      if a.email == Nothing then
        { a | email = Just "<unspecified>" }
      else
        a
  in
    xs
      |> List.map withMarkedEmail
      |> convert02

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


-- Implement setPhone
-- > setPhone "+123456" { profile = { address = { phone = "+654321" } } }
-- { profile = { address = { phone = "+123456" } } }
type alias User = { profile : Profile }
type alias Profile = { address : Address }
type alias Address = { phone : String }

setPhone : String -> User -> User
setPhone phone user =
  let
      profile = user.profile
      address = profile.address
  in
      { user | profile = { profile | address = { address | phone = phone } } }

-- > catMaybes [Just 1, Nothing, Just 3]
-- [1,3] : List number
catMaybes : List (Maybe a) -> List a
catMaybes xs =
  let appendIfNonEmpty i a =
        case i of
          Just x ->
            a ++ [x]
          Nothing ->
            a
  in
        List.foldl appendIfNonEmpty [] xs


-- > mapMaybes (\x -> if x == Just 3 then x else Just 4) [Just 1, Nothing, Just 3]
-- [4,4,3] : List number
-- mapMaybes : (a -> Maybe b) -> List a -> List b
mapMaybes f xs = List.map f xs |> catMaybes


-- Use package elm/url and its Url.Builder.absolute to build URL from parameters

-- > buildStatsUrl 12 {startDate=Nothing, numElems=Nothing}
-- https://myapi.com/api/item/12/stats.json

-- > buildStatsUrl 12 {startDate=Just "2019-01-01", numElems=Nothing}
-- https://myapi.com/api/item/12/stats.json?start_date=2019-01-01

-- > buildStatsUrl 12 {startDate=Just "2019-01-01", numElems=Just 10}
-- https://myapi.com/api/item/12/stats.json?start_date=2019-01-01&num_items=10
buildStatsUrl : Int -> { startDate : Maybe String, numElems : Maybe Int } -> String
buildStatsUrl itemId ps =
  let
    params = [] |> listWithStartDate ps |> listWithNumElems ps
  in
    Url.Builder.absolute ["api", "item", String.fromInt itemId] params

listWithStartDate {startDate, numElems} list =
  case startDate of
    Nothing ->
      list
    Just a ->
      (Url.Builder.string "startDate" a) :: list

listWithNumElems {startDate, numElems} list =
  case numElems of
    Nothing ->
      list
    Just a ->
      (Url.Builder.int "numElems" a) :: list
