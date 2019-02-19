-- https://github.com/KyivHaskell/elm-study-group/blob/master/resources/homework-01.md

module Session1.Session1 exposing(
    myLast, myButLast, myLength, myReverse, isPalindrome,
    compress, dropEvery, clap
  )
import String

-- Find last element in a list
myLast xs =
  case xs of
    [] ->
      Nothing
    [a] ->
      Just a
    first :: rest ->
      myLast rest

-- Find the last but one element of a list
myButLast xs =
  case xs of
    [] ->
      Nothing
    [a] ->
      Nothing
    [a, b] ->
      Just a
    first :: rest ->
      myButLast rest

-- Find the K'th element of a list
elementAt : List a -> Int -> Maybe a
elementAt xs n =
  if n == 1 then
    List.head xs
  else
    case xs of
      [] ->
        Nothing
      [a] ->
        Nothing
      first :: rest ->
        elementAt rest (n - 1)

-- Find the number of elements of a list
myLength : List a -> Int
myLength xs =
  case xs of
    [] ->
      0
    [a] ->
      1
    first::rest ->
      1 + myLength rest

-- Reverse a list
myReverse : List a -> List a
myReverse xs =
  case xs of
    [] ->
      []
    [a] ->
      [a]
    [a, b] ->
      [b, a]
    first :: rest ->
      myReverse rest ++ [first]

-- Find out whether a list is a palindrome
isPalindrome : List a -> Bool
isPalindrome xs = xs == myReverse xs

-- Eliminate consecutive duplicates of string elements
compress : String -> String
compress s =
  s
    |> String.toList
    |> compressList
    |> String.fromList

-- Eliminate consecutive duplicates in a list
compressList : List a -> List a
compressList xs =
  case xs of
    [] ->
      []
    [a] ->
      [a]
    first :: rest ->
      if Just first == List.head rest then
        compressList rest
      else
        [first] ++ compressList rest

-- Drop every N'th element from a string
dropEvery : String -> Int -> String
dropEvery s n =
  let dropEveryOnListFlipped a b = dropEveryOnList b a
  in
  s
    |> String.toList
    |> dropEveryOnListFlipped n
    |> String.fromList

-- Drop every N'th element from a list
dropEveryOnList : List a -> Int -> List a
dropEveryOnList xs n =
  let
    dropEveryWithCounterOnList ys m j =
      case ys of
        [] ->
          []
        first :: rest ->
          if myLength ys < j then
            ys
          else if j == 1 then
            dropEveryWithCounterOnList rest m m
          else
            [first] ++ dropEveryWithCounterOnList rest m (j-1)
  in
  dropEveryWithCounterOnList xs n n

-- Insert the clap emoji between words
clap : String -> String
clap s =
  s
    |> String.words
    |> String.join "👏"
