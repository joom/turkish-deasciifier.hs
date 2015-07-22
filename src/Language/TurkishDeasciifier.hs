module Language.TurkishDeasciifier (deasciify) where

import Data.List (foldl')
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Lazy as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as DVM
import Language.TurkishDeasciifier.Table (turkishPatternTable)

turkishContextSize :: Int
turkishContextSize = 10

uppercaseLetters :: String
uppercaseLetters = ['A'..'Z']

turkishAsciifyTable :: M.Map Char Char
turkishAsciifyTable =
  M.fromList [ ('ç', 'c') , ('Ç', 'C') , ('ğ', 'g') , ('Ğ', 'G') , ('ö', 'o')
             , ('Ö', 'O') , ('ı', 'i') , ('İ', 'I') , ('ş', 's') , ('Ş', 'S') ]

turkishDowncaseAsciifyTable :: M.Map Char Char
turkishDowncaseAsciifyTable =
  M.fromList $ concatMap (\c -> let x = toLower c in [(c, x), (x, x)])
                         uppercaseLetters
             ++ [ ('ç', 'c') , ('Ç', 'c') , ('ğ', 'g') , ('Ğ', 'g')
                , ('ö', 'o') , ('Ö', 'o') , ('ı', 'i') , ('İ', 'i')
                , ('ş', 's') , ('Ş', 's') , ('ü', 'u') , ('Ü', 'u') ]

turkishUpcaseAccentsTable :: M.Map Char Char
turkishUpcaseAccentsTable =
  M.fromList $ concatMap (\c -> let x = toLower c in [(c, x), (x, x)])
                         uppercaseLetters
             ++ [ ('ç', 'C') , ('Ç', 'C') , ('ğ', 'G') , ('Ğ', 'G')
                , ('ö', 'O') , ('Ö', 'O') , ('ı', 'I') , ('İ', 'i')
                , ('ş', 'S') , ('Ş', 'S') , ('ü', 'U') , ('Ü', 'U') ]

turkishToggleAccentTable :: M.Map Char Char
turkishToggleAccentTable =
  M.fromList [ ('c', 'ç') , ('C', 'Ç') , ('g', 'ğ') , ('G', 'Ğ') , ('o', 'ö')
             , ('O', 'Ö') , ('u', 'ü') , ('U', 'Ü') , ('i', 'ı') , ('I', 'İ')
             , ('s', 'ş') , ('S', 'Ş')   -- end of initial direction
             , ('ç', 'c') , ('Ç', 'C') , ('ğ', 'g') , ('Ğ', 'G') , ('ö', 'o')
             , ('Ö', 'O') , ('ü', 'u') , ('Ü', 'U') , ('ı', 'i') , ('İ', 'I')
             , ('ş', 's') , ('Ş', 'S') ] -- end of other direction

-- | Adds or removes Turkish accent.
turkishToggleAccent :: Char -> Char
turkishToggleAccent c = fromMaybe c (M.lookup c turkishToggleAccentTable)

-- | Replace a single character in given index with a given character.
setCharAt :: V.Vector Char -> Int -> Char -> V.Vector Char
setCharAt v i c = V.modify (\v' -> DVM.unsafeWrite v' i c) v

-- | Java's substring for lists.
substring :: Int -> Int -> [a] -> [a]
substring x y = drop x . take y

-- | Java's substring for vectors.
vsubstring :: Int -> Int -> V.Vector a -> V.Vector a
vsubstring x y = V.drop x . V.take y

-- | Returns the deasciified text.
deasciify :: String -> String
deasciify asciiString = V.toList $ V.ifoldl' f original original
  where
    original = V.fromList asciiString

    f :: V.Vector Char -> Int -> Char -> V.Vector Char
    f v i c
      | turkishNeedCorrection v c i = setCharAt v i (turkishToggleAccent c)
      | otherwise = v

-- | Determines if the char at the given index needs correction.
turkishNeedCorrection :: V.Vector Char -> Char -> Int -> Bool
turkishNeedCorrection v c point
    | tr == 'I' = if c == tr then not m else m
    | otherwise = if c == tr then m else not m
  where
    tr = fromMaybe c (M.lookup c turkishAsciifyTable)
    pl = M.lookup (toLower tr) turkishPatternTable
    m = maybe False (turkishMatchPattern v point) pl

turkishMatchPattern :: V.Vector Char -> Int -> M.Map String Int -> Bool
turkishMatchPattern v point dlist = rank > 0
  where
    str = turkishGetContext v turkishContextSize point
    rank = foldl'
            (\acc start ->
                foldl'
                  (\acc' end -> maybe acc'
                                (\r -> if abs r < abs acc' then r else acc')
                                (M.lookup (substring start end str) dlist))
                  acc [(turkishContextSize + 1)..(length str)])
            (M.size dlist * 2) [0..turkishContextSize]

turkishGetContext :: V.Vector Char -> Int -> Int -> String
turkishGetContext v size point =
    V.toList $ fst (loopUp s'' (size - 1) False (point - 1))
  where
    loopDown :: V.Vector Char -> Int -> Bool -> Int -> (V.Vector Char, Int)
    loopDown s' i space index =
      if i < V.length s && not space && index < V.length v then
        (case M.lookup ((V.!) v index) turkishDowncaseAsciifyTable of
          Just x  -> loopDown (setCharAt s' i x) (i + 1) False (index + 1)
          Nothing -> loopDown s' (i + 1) True (index + 1))
      else  (s', i)

    loopUp :: V.Vector Char -> Int -> Bool -> Int -> (V.Vector Char, Int)
    loopUp s' i space index =
      if i >= 0 && index >= 0 then
        (case M.lookup ((V.!) v index) turkishUpcaseAccentsTable of
          Just x  -> loopUp (setCharAt s' i x)
                            (i - 1) False (index - 1)
          Nothing -> if   space
                     then loopUp s' (i - 2) space (index - 1)
                     else loopUp s' (i - 1) True  (index - 1) )
      else (s', i)

    s       = setCharAt (V.replicate (2 * size + 1) ' ') size 'X'
    (s', i) = loopDown s (size + 1) False (point + 1)
    s''     = vsubstring 0 i s'
