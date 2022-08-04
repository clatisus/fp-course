{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
-- on a Mac - run this with:
-- > fastAnagrams "Tony" "/usr/share/dict/words"
fastAnagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams str fp = filter' (permutations str) . S.fromList . hlist . (NoCaseString <$>) . lines <$> readFile fp
  where member' :: S.Set NoCaseString -> Chars -> Bool
        member' = flip $ S.member . NoCaseString
        filter' :: List Chars -> S.Set NoCaseString -> List Chars
        filter' = flip (filter . member')

-- >> fastAnagrams "abc" "share/dictionary.txt"
-- ["abc","cba","cab"]

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Ord NoCaseString where
  (<=) = (<=) `on` map toLower . ncString

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
