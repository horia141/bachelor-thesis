module Utils
    (toLefts,
     maybeToEither,
     gatherEithers) where

import Data.Either (partitionEithers)
import Data.List (intercalate)
    
toLefts :: (a -> c) -> Either a b -> Either c b
toLefts f (Left x) = Left $ f x
toLefts f (Right x) = Right x

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just x) = Right x
maybeToEither a Nothing = Left a

gatherEithers :: [Either String a] -> Either String [a]
gatherEithers eithers =
    case partitionEithers eithers of
      ([],results) -> Right results
      (errorMessages,_) -> Left (intercalate "\n" errorMessages)

