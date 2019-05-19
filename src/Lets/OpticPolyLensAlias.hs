{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Lets.OpticPolyLensAlias
  ( Lens
  , getsetLaw
  , setgetLaw
  , setsetLaw
  , get
  , set
  , modify
  , (%~)
  , (.~)
  , fmodify
  , (|=)
  , fstL
  , sndL
  , mapL
  , setL
  , identity
  , product
  , (***)
  , choice
  , (|||)
  , cityL
  , countryL
  , streetL
  , suburbL
  , localityL
  , ageL
  , nameL
  , addressL
  , intAndIntL
  , intAndL
  , getSuburb
  , setStreet
  , getAgeAndCountry
  , setCityAndLocality
  , getSuburbOrCity
  , setStreetOrState
  , modifyCityUppercase
  , modifyIntandLengthEven
  )
where

import           Data.Char                      ( toUpper )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
                                                ( insert
                                                , delete
                                                , lookup
                                                )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
                                                ( insert
                                                , delete
                                                , member
                                                )
import           Lets.Data                      ( AlongsideLeft
                                                  ( AlongsideLeft
                                                  , getAlongsideLeft
                                                  )
                                                , AlongsideRight
                                                  ( AlongsideRight
                                                  , getAlongsideRight
                                                  )
                                                , Identity
                                                  ( Identity
                                                  , getIdentity
                                                  )
                                                , Const(Const, getConst)
                                                , IntAnd(IntAnd)
                                                , Person(Person)
                                                , Locality(Locality)
                                                , Address(Address)
                                                )
import           Prelude                 hiding ( product )

-- $setup
-- >>> import qualified Data.Map as Map(fromList)
-- >>> import qualified Data.Set as Set(fromList)
-- >>> import Data.Bool(bool)
-- >>> import Data.Char(ord)
-- >>> import Lets.Data

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

get :: Lens s t a b -> s -> a
get l = getConst . l Const

set :: Lens s t a b -> s -> b -> t
set l s b = getIdentity (l a2fb s) where a2fb = const (Identity b)

-- | The get/set law of lenses. This function should always return @True@.
getsetLaw :: Eq s => Lens s s a a -> s -> Bool
getsetLaw l a = set l a (get l a) == a

-- | The set/get law of lenses. This function should always return @True@.
setgetLaw :: Eq a => Lens s s a a -> s -> a -> Bool
setgetLaw l a b = get l (set l a b) == b

-- | The set/set law of lenses. This function should always return @True@.
setsetLaw :: Eq s => Lens s s a b -> s -> b -> b -> Bool
setsetLaw l a b1 b2 = set l (set l a b1) b2 == set l a b2

----

-- |
--
-- >>> modify fstL (+1) (0 :: Int, "abc")
-- (1,"abc")
--
-- >>> modify sndL (+1) ("abc", 0 :: Int)
-- ("abc",1)
--
-- prop> let types = (x :: Int, y :: String) in modify fstL id (x, y) == (x, y)
--
-- prop> let types = (x :: Int, y :: String) in modify sndL id (x, y) == (x, y)
modify :: Lens s t a b -> (a -> b) -> s -> t
modify l a2b = getIdentity . l (Identity . a2b)

-- | An alias for @modify@.
(%~) :: Lens s t a b -> (a -> b) -> s -> t
(%~) = modify

infixr 4 %~

-- |
--
-- >>> fstL .~ 1 $ (0 :: Int, "abc")
-- (1,"abc")
--
-- >>> sndL .~ 1 $ ("abc", 0 :: Int)
-- ("abc",1)
--
-- prop> let types = (x :: Int, y :: String) in set fstL (x, y) z == (fstL .~ z $ (x, y))
--
-- prop> let types = (x :: Int, y :: String) in set sndL (x, y) z == (sndL .~ z $ (x, y))
(.~) :: Lens s t a b -> b -> s -> t
(.~) l b s = set l s b

infixl 5 .~

-- |
--
-- >>> fmodify fstL (+) (5 :: Int, "abc") 8
-- (13,"abc")
--
-- >>> fmodify fstL (\n -> bool Nothing (Just (n * 2)) (even n)) (10, "abc")
-- Just (20,"abc")
--
-- >>> fmodify fstL (\n -> bool Nothing (Just (n * 2)) (even n)) (11, "abc")
-- Nothing
fmodify :: Functor f => Lens s t a b -> (a -> f b) -> s -> f t
fmodify l = l

-- |
--
-- >>> fstL |= Just 3 $ (7, "abc")
-- Just (3,"abc")
--
-- >>> (fstL |= (+1) $ (3, "abc")) 17
-- (18,"abc")
(|=) :: Functor f => Lens s t a b -> f b -> s -> f t
(|=) l fb = l (const fb)

infixl 5 |=

-- |
--
-- >>> modify fstL (*10) (3, "abc")
-- (30,"abc")
--
-- prop> let types = (x :: Int, y :: String) in getsetLaw fstL (x, y)
--
-- prop> let types = (x :: Int, y :: String) in setgetLaw fstL (x, y) z
--
-- prop> let types = (x :: Int, y :: String) in setsetLaw fstL (x, y) z
fstL :: Lens (a, x) (b, x) a b
fstL = \a2fb (a, x) -> (, x) <$> a2fb a

-- |
--
-- >>> modify sndL (++ "def") (13, "abc")
-- (13,"abcdef")
--
-- prop> let types = (x :: Int, y :: String) in getsetLaw sndL (x, y)
--
-- prop> let types = (x :: Int, y :: String) in setgetLaw sndL (x, y) z
--
-- prop> let types = (x :: Int, y :: String) in setsetLaw sndL (x, y) z
sndL :: Lens (x, a) (x, b) a b
sndL = \a2fb (x, a) -> (x, ) <$> a2fb a

-- |
--
-- >>> get (mapL 3) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d']))
-- Just 'c'
--
-- >>> get (mapL 33) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d']))
-- Nothing
--
-- >>> set (mapL 3) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d'])) (Just 'X')
-- fromList [(1,'a'),(2,'b'),(3,'X'),(4,'d')]
--
-- >>> set (mapL 33) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d'])) (Just 'X')
-- fromList [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(33,'X')]
--
-- >>> set (mapL 3) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d'])) Nothing
-- fromList [(1,'a'),(2,'b'),(4,'d')]
--
-- >>> set (mapL 33) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d'])) Nothing
-- fromList [(1,'a'),(2,'b'),(3,'c'),(4,'d')]
mapL :: Ord k => k -> Lens (Map k v) (Map k v) (Maybe v) (Maybe v)
mapL k = r
 where
  r mv2fmv kvMap = insertOrDelete kvMap <$> mv2fmv (Map.lookup k kvMap)
  insertOrDelete kvMap =
    maybe (Map.delete k kvMap) (\v -> Map.insert k v kvMap)

-- |
--
-- >>> get (setL 3) (Set.fromList [1..5])
-- True
--
-- >>> get (setL 33) (Set.fromList [1..5])
-- False
--
-- >>> set (setL 3) (Set.fromList [1..5]) True
-- fromList [1,2,3,4,5]
--
-- >>> set (setL 3) (Set.fromList [1..5]) False
-- fromList [1,2,4,5]
--
-- >>> set (setL 33) (Set.fromList [1..5]) True
-- fromList [1,2,3,4,5,33]
--
-- >>> set (setL 33) (Set.fromList [1..5]) False
-- fromList [1,2,3,4,5]
setL :: Ord k => k -> Lens (Set k) (Set k) Bool Bool
setL k = r
 where
  r bool2fbool kSet = insertOrDelete kSet <$> bool2fbool (Set.member k kSet)
  insertOrDelete kSet b = if b then Set.insert k kSet else Set.delete k kSet

-- |
--
-- >>> get (compose sndL fstL ) ("abc", (7, "def"))
-- 7
--
-- >>> set (compose sndL fstL) ("abc", (7, "def")) 8
-- ("abc",(8,"def"))
compose :: Lens q r s t -> Lens s t a b -> Lens q r a b
compose = (.)
-- (a -> f b) -> s -> f t
-- -> (s -> f t) -> q -> f r
-- -> (a -> f b) -> q -> f r

-- |
--
-- >>> get identity 3
-- 3
--
-- >>> set identity 3 4
-- 4
identity :: Lens a b a b
identity = id
-- (a -> f b) -> a -> f b

-- |
-- AlongsideLeft
-- AlongsideRight
-- >>> get (product fstL sndL) (("abc", 3), (4, "def"))
-- ("abc","def")
--
-- >>> set (product fstL sndL) (("abc", 3), (4, "def")) ("ghi", "jkl")
-- (("ghi",3),(4,"jkl"))
product :: Lens s t a b -> Lens q r c d -> Lens (s, q) (t, r) (a, c) (b, d)
product l l' = \ac2fbd (s, q) -> getAlongsideLeft (runLeft ac2fbd s q)
 where
  runLeft ac2fbd s q =
    l (\a -> AlongsideLeft (getAlongsideRight (runRight ac2fbd a q))) s
  runRight ac2fbd a = l' (\c -> AlongsideRight (ac2fbd (a, c)))

product' :: Lens s t a b -> Lens q r c d -> Lens (s, q) (t, r) (a, c) (b, d)
product' l l' = \ac2fbd (s, q) ->
  (\(b, d) -> (set l s b, set l' q d)) <$> ac2fbd (get l s, get l' q)
-- (a -> f b) -> s -> f t
-- (c -> f d) -> q -> f r
-- ((a, c) -> f (b, d)) -> (s, q) -> f (t, r)

-- | An alias for @product@.
(***) :: Lens s t a b -> Lens q r c d -> Lens (s, q) (t, r) (a, c) (b, d)
(***) = product

infixr 3 ***

-- |
--
-- >>> get (choice fstL sndL) (Left ("abc", 7))
-- "abc"
--
-- >>> get (choice fstL sndL) (Right ("abc", 7))
-- 7
--
-- >>> set (choice fstL sndL) (Left ("abc", 7)) "def"
-- Left ("def",7)
--
-- >>> set (choice fstL sndL) (Right ("abc", 7)) 8
-- Right ("abc",8)
choice :: Lens s t a b -> Lens q r a b -> Lens (Either s q) (Either t r) a b
choice l l' = choose
 where
  choose a2fb (Left  s) = Left <$> l a2fb s
  choose a2fb (Right q) = Right <$> l' a2fb q

choice' :: Lens s t a b -> Lens q r a b -> Lens (Either s q) (Either t r) a b
choice' l l' = choose
 where
  choose a2fb (Left  s) = Left . set l s <$> a2fb (get l s)
  choose a2fb (Right q) = Right . set l' q <$> a2fb (get l' q)
-- (a -> f b) -> Either s q -> f (Either t r)

-- | An alias for @choice@.
(|||) :: Lens s t a b -> Lens q r a b -> Lens (Either s q) (Either t r) a b
(|||) = choice

infixr 2 |||

----

type Lens' a b =
  Lens a a b b

cityL :: Lens' Locality String
cityL = (\p (Locality c t y) -> fmap (\c' -> Locality c' t y) (p c))

stateL :: Lens' Locality String
stateL = (\p (Locality c t y) -> fmap (\t' -> Locality c t' y) (p t))

countryL :: Lens' Locality String
countryL = (\p (Locality c t y) -> fmap (Locality c t) (p y))

streetL :: Lens' Address String
streetL = (\p (Address t s l) -> fmap (\t' -> Address t' s l) (p t))

suburbL :: Lens' Address String
suburbL = (\p (Address t s l) -> fmap (\s' -> Address t s' l) (p s))

localityL :: Lens' Address Locality
localityL = (\p (Address t s l) -> fmap (Address t s) (p l))

ageL :: Lens' Person Int
ageL = (\p (Person a n d) -> fmap (\a' -> Person a' n d) (p a))

nameL :: Lens' Person String
nameL = (\p (Person a n d) -> fmap (\n' -> Person a n' d) (p n))

addressL :: Lens' Person Address
addressL = (\p (Person a n d) -> fmap (Person a n) (p d))

intAndIntL :: Lens' (IntAnd a) Int
intAndIntL = (\p (IntAnd n a) -> fmap (`IntAnd` a) (p n))

-- lens for polymorphic update
intAndL :: Lens (IntAnd a) (IntAnd b) a b
intAndL = (\p (IntAnd n a) -> fmap (IntAnd n) (p a))

-- |
--
-- >>> getSuburb fred
-- "Fredville"
--
-- >>> getSuburb mary
-- "Maryland"
getSuburb :: Person -> String
getSuburb = get $ addressL `compose` suburbL


-- |
--
-- >>> setStreet fred "Some Other St"
-- Person 24 "Fred" (Address "Some Other St" "Fredville" (Locality "Fredmania" "New South Fred" "Fredalia"))
--
-- >>> setStreet mary "Some Other St"
-- Person 28 "Mary" (Address "Some Other St" "Maryland" (Locality "Mary Mary" "Western Mary" "Maristan"))
setStreet :: Person -> String -> Person
setStreet = set $ addressL . streetL

-- |
--
-- >>> getAgeAndCountry (fred, maryLocality)
-- (24,"Maristan")
--
-- >>> getAgeAndCountry (mary, fredLocality)
-- (28,"Fredalia")
getAgeAndCountry :: (Person, Locality) -> (Int, String)
getAgeAndCountry = get $ ageL *** countryL

-- |
--
-- >>> setCityAndLocality (fred, maryAddress) ("Some Other City", fredLocality)
-- (Person 24 "Fred" (Address "15 Fred St" "Fredville" (Locality "Some Other City" "New South Fred" "Fredalia")),Address "83 Mary Ln" "Maryland" (Locality "Fredmania" "New South Fred" "Fredalia"))
--
-- >>> setCityAndLocality (mary, fredAddress) ("Some Other City", maryLocality)
-- (Person 28 "Mary" (Address "83 Mary Ln" "Maryland" (Locality "Some Other City" "Western Mary" "Maristan")),Address "15 Fred St" "Fredville" (Locality "Mary Mary" "Western Mary" "Maristan"))
setCityAndLocality
  :: (Person, Address) -> (String, Locality) -> (Person, Address)
setCityAndLocality = set $ addressL . localityL . cityL *** localityL
--
-- |
--
-- >>> getSuburbOrCity (Left maryAddress)
-- "Maryland"
--
-- >>> getSuburbOrCity (Right fredLocality)
-- "Fredmania"
getSuburbOrCity :: Either Address Locality -> String
getSuburbOrCity = get (suburbL ||| cityL)

-- |
--
-- >>> setStreetOrState (Right maryLocality) "Some Other State"
-- Right (Locality "Mary Mary" "Some Other State" "Maristan")
--
-- >>> setStreetOrState (Left fred) "Some Other St"
-- Left (Person 24 "Fred" (Address "Some Other St" "Fredville" (Locality "Fredmania" "New South Fred" "Fredalia")))
setStreetOrState :: Either Person Locality -> String -> Either Person Locality
setStreetOrState = set (addressL . streetL ||| stateL)

-- |
--
-- >>> modifyCityUppercase fred
-- Person 24 "Fred" (Address "15 Fred St" "Fredville" (Locality "FREDMANIA" "New South Fred" "Fredalia"))
--
-- >>> modifyCityUppercase mary
-- Person 28 "Mary" (Address "83 Mary Ln" "Maryland" (Locality "MARY MARY" "Western Mary" "Maristan"))
modifyCityUppercase :: Person -> Person
modifyCityUppercase = addressL . localityL . cityL %~ map toUpper

-- |
--
-- >>> modify intAndL (even . length) (IntAnd 10 "abc")
-- IntAnd 10 False
--
-- >>> modify intAndL (even . length) (IntAnd 10 "abcd")
-- IntAnd 10 True
modifyIntandLengthEven :: IntAnd [a] -> IntAnd Bool
modifyIntandLengthEven = intAndL %~ even . length
