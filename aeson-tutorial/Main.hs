{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module AesonTutorial where
import Data.Aeson.Types
import Data.Text (Text)
import Data.Scientific
import Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Control.Applicative ((<|>))

-- Notes from the Aeson tutorial at https://artyom.me/aeson

-- The Basics
-- Aeson has the following data type for representing JSON (without the '):
data Value'
    = Object' Object
    | Array' Array
    | String' Text
    | Number' Scientific
    | Bool' Bool
    | Null

-- It includes the encode, decode, and eitherDecode functions for basic handling of ToJSON and FromJSON instances.
-- eitherDecode will give a string with failure information instead of throwing an exception.

-- Aeson defines parsers for most popular types using the FromJson typeclass
class FromJSON' a where
    parseJSON' :: Value -> Parser a

-- The "withX" style functions can be used to encapsulate the pattern of expecting an X and
-- failing with an appropriate error message if one is not encountered.
-- Example for bool:
instance FromJSON' Bool where
    parseJSON' = withBool "Bool" return

-- And for lists:
instance FromJSON' a => FromJSON' [a] where
    parseJSON' = withArray "[a]" $ mapM parseJSON' . V.toList


-- Parsing a tuple
parseTuple :: Value -> Parser (Bool, String)
parseTuple = withObject "tuple" $ \o -> do
    a <- case HM.lookup "a" o of
        Just x -> parseJSON x
        Nothing -> fail "no field 'a'"
    b <- case HM.lookup "b" o of
        Just x -> parseJSON x
        Nothing -> fail "no field 'b'"
    return (a,b)

-- We can avoid this repetitive style with some handy functions: (.:)
-- This will lookup the value by the key, call parseJSON on it if it exists,
-- and call fail if it doesn't exist
parseTuple' :: Value -> Parser (Bool, String)
parseTuple' = withObject "tuple" $ \o ->
    (,) <$> o .: "a"
        <*> o .: "b"

-- Using RecordWildCards
data Person = Person { name :: String, age :: Int }
instance FromJSON Person where
    parseJSON = withObject "person" $ \o -> do
        name <- o .: "name"
        age <- o .: "age"
        return Person{..}

-- (.:?) is as above, but it returns a Maybe instead of failing when the field doesn't exist.
-- (.!=) can be used to provide a default value
instance FromJSON' Person where
    parseJSON' = withObject "person" $ \o -> do
        name <- o .: "name"
        age <- o .:? "age" .!= 18
        return Person{..}

-- We can also use <|> to alternate parsers, and asum to try a list of parsers.  The following looks up the keys
-- a and b, where b is case insensitive with a preference to the lower-case version
parseTuple'' :: Value -> Parser (Bool, String)
parseTuple'' = withObject "tuple" $ \o ->
    (,) <$> o .: "a"
        <*> o .: "b" <|> o .: "B"

ex1 :: Value
ex1 = Object (fromList [("a", Bool True), ("b", String "foobar")])

t :: Parser (Bool, String)
t = parseTuple ex1

-- If the data does not have known field names to access, we can get around this by using the fact
-- that under the hood we're dealing with Object :: HashMap Text Value,
-- so we can look at the actual keys that are there in the HashMap with some normal toList function logic.

-- We can use generics to derive everything automatically, if we aren't doing any pre- or post- processing.
-- Even if we are, we can use `genericParseJSON`, which takes some options, to customize the generated functions.

-- Moreover, there are also TemplateHaskell derivations in `Data.Aeson.TH`


main :: IO ()
main = putStrLn "Notes from following https://artyom.me/aeson"

-- Note: Bryan O'Sullivan just says to read the Haddock on Aeson instead of this tutorial :)
