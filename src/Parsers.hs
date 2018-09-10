{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
module Parsers where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import qualified Data.Text        as T
import           Data.Time.Clock
import           Data.Time.Format
import           GHC.Generics

newtype ISO8601Time = ISO8601Time UTCTime deriving (Show, Generic)

instance FromJSON ISO8601Time where
  parseJSON = withText "ISO8601Time" $ \v -> ISO8601Time <$> parseISO8601 v
    where parseISO8601 = parseTimeM True defaultTimeLocale
                          (iso8601DateFormat $ Just "%T") . T.unpack

parseJSONIgnoreUnderscore :: (Generic a, GFromJSON (Rep a)) => Value -> Parser a
parseJSONIgnoreUnderscore =
  genericParseJSON defaultOptions {fieldLabelModifier = filter (/= '_')}

parseJSONCamelToSnakeCase :: (Generic a, GFromJSON (Rep a)) => Value -> Parser a
parseJSONCamelToSnakeCase = parseJSONCamelReplaceUpper (\s -> ['_', toLower s])

parseJSONCamelToWords :: (Generic a, GFromJSON (Rep a)) => Value -> Parser a
parseJSONCamelToWords = parseJSONCamelReplaceUpper (\s -> [' ', toLower s])

parseJSONCamelReplaceUpper :: (Generic a, GFromJSON (Rep a)) => (Char -> String) -> Value -> Parser a
parseJSONCamelReplaceUpper replaceUpper =
  genericParseJSON defaultOptions {fieldLabelModifier = modifyUpper}
  where modifyUpper = foldr (\s ss -> if isLower s then s : ss else replaceUpper s ++ ss) []
