module ResErr
    (ResErr(..),
     (|->),
     (>->),
     reLift,
     reReduce) where

data ResErr a
    = Res a
    | Err [String]
    deriving (Show)

(|->) :: a -> (a -> ResErr b) -> ResErr b
(|->) value f = f value

(>->) :: ResErr a -> (a -> ResErr b) -> ResErr b
(>->) (Res value) f = f value
(>->) (Err errStrings) f = Err errStrings

reLift :: (a -> b) -> ResErr a -> ResErr b
reLift f (Res value) = Res (f value)
reLift f (Err errString) = Err errString

reReduce :: [ResErr a] -> ResErr [a]
reReduce reserrs =
    case foldr partitionResErr ([],[]) reserrs of
      (values,[]) -> Res values
      (_,errStrings) -> Err $ concat errStrings
    where partitionResErr :: ResErr a -> ([a],[[String]]) -> ([a],[[String]])
          partitionResErr (Res value) (values,errStrings) = (value:values,errStrings)
          partitionResErr (Err errString) (values,errStrings) = (values,errString:errStrings)
