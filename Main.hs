{-# LANGUAGE OverloadedStrings,DataKinds #-}
module Main where

import Network.StackExchange hiding (Error)
import Data.Monoid ((<>),mconcat)
import Data.PSQueue (PSQ,Binding((:->)))
import qualified Data.PSQueue as PSQ (singleton,minView,insert)
import Data.Aeson (withObject,(.:),FromJSON(parseJSON),fromJSON,Result(Success,Error))
import Data.Text.Lazy (pack)
import Pipes
import qualified Pipes.Prelude as Pipes (print)
import Network.HTTP.Base (urlEncode)

type TagName = String
type Count = Int
data NameAndCount = NameAndCount TagName Count deriving (Show,Eq)

fromResult :: Result a -> a
fromResult (Success a) = a
fromResult (Error e) = error e

instance FromJSON NameAndCount where
    parseJSON = withObject "Tag Object" (\o -> do
        name <- o .: "name"
        count <- o .: "count"
        return (NameAndCount name count))

step :: PSQ [TagName] Int -> Producer [TagName] IO r
step psq = do
    let Just (tagnames :-> p,rest) = PSQ.minView psq
    next_tags <- liftIO (case tagnames of
        [] -> askSE (mconcat [site "stackoverflow",tags])
        _ -> askSE (mconcat [site "stackoverflow",relatedTags (map (pack . urlEncode) tagnames)]))
    let key_priority_pairs = map (fromResult . fromJSON  . unSE) next_tags
    yield tagnames
    step (foldr (\(NameAndCount k p) q -> PSQ.insert (k:tagnames) (negate p) q) rest key_priority_pairs)


main :: IO ()
main = do
    let initial_tagset = PSQ.singleton [] 0
    runEffect (step initial_tagset >-> Pipes.print)






