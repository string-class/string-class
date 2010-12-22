{-# LANGUAGE TypeFamilies, FlexibleContexts, TypeSynonymInstances #-}

-- | This module is mostly self-explanatory

module Data.String.Class
    ( StringConstruct(..)
    , StringChar(..)
    , StringLength(..)
    , StringEmpty(..)
    , StringPack(..)
    , StringString(..)
    , StringStrictByteString(..)
    , StringLazyByteString(..)
    , StringText(..)

    , fromJust
    , isJust
    ) where

import Prelude hiding (head, tail, last, init)
import Control.Applicative
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.List as List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Word

type Unused a = a

-- | Minimal complete definition: StringConstructChar; StringConstructAltChar; toMainChar; toAltChar; cons; snoc; and either all of head, tail, last, and init, or all of uncons and unsnoc
class (StringChar (StringConstructChar s), StringChar (StringConstructAltChar s)) => StringConstruct s where
    type StringConstructChar s
    type StringConstructAltChar s

    infixr 9 `cons`
    infixr 9 `uncons`
    infixr 9 `altCons`
    infixr 9 `altUncons`
    cons :: (StringConstructChar s) -> s -> s
    uncons :: s -> Maybe ((StringConstructChar s), s)
    snoc :: s -> (StringConstructChar s) -> s
    unsnoc :: s -> Maybe (s, (StringConstructChar s))
    altCons :: (StringConstructAltChar s) -> s -> s
    altUncons :: s -> Maybe ((StringConstructAltChar s), s)
    altSnoc :: s -> (StringConstructAltChar s) -> s
    altUnsnoc :: s -> Maybe (s, (StringConstructAltChar s))

    -- | The unused parameter is only needed for its type and should be ignored in the function definition
    toMainChar :: (StringChar c) => Unused s -> c -> (StringConstructChar s)
    -- | The unused parameter is only needed for its type and should be ignored in the function definition
    toAltChar :: (StringChar c) => Unused s -> c -> (StringConstructAltChar s)

    head :: s -> Maybe (StringConstructChar s)
    tail :: s -> Maybe s
    last :: s -> Maybe (StringConstructChar s)
    init :: s -> Maybe s
    altHead :: s -> Maybe (StringConstructAltChar s)
    altLast :: s -> Maybe (StringConstructAltChar s)

    infixr 9 `cons2`
    infixr 9 `uncons2`
    infixr 9 `cons3`
    infixr 9 `uncons3`
    infixr 9 `cons4`
    infixr 9 `uncons4`
    cons2   :: (StringConstructChar s) -> (StringConstructChar s) -> s -> s
    uncons2 :: s -> Maybe ((StringConstructChar s), (StringConstructChar s), s)
    cons3   :: (StringConstructChar s) -> (StringConstructChar s) -> (StringConstructChar s) -> s -> s
    uncons3 :: s -> Maybe ((StringConstructChar s), (StringConstructChar s), (StringConstructChar s), s)
    cons4   :: (StringConstructChar s) -> (StringConstructChar s) -> (StringConstructChar s) -> (StringConstructChar s) -> s -> s
    uncons4 :: s -> Maybe ((StringConstructChar s), (StringConstructChar s), (StringConstructChar s), (StringConstructChar s), s)

    -- | An undefined value meant to disambiguate which instance of 'toMainChar' or 'toAltChar' to use
    --
    -- It is therefore normally unnecessary to define in instances.
    keyStringConstruct :: s
    keyStringConstruct = undefined

    altCons c s = cons (toMainChar s c) s
    altSnoc s c = snoc s (toMainChar s c)
    altUncons s = (\ ~(a, s') -> (toAltChar s a, s')) `fmap` uncons s
    altUnsnoc s = (\ ~(s', a) -> (s', toAltChar s a)) `fmap` unsnoc s

    head = (fst `fmap`) . uncons
    tail = (snd `fmap`) . uncons
    last = (snd `fmap`) . unsnoc
    init = (fst `fmap`) . unsnoc
    altHead s = (toAltChar s <$>) . head $ s
    altLast s = (toAltChar s <$>) . last $ s

    cons4 a b c d s = a `cons` b `cons` c `cons` d `cons` s
    uncons3 s       = do
        (a, s')   <- uncons s
        (b, s'')  <- uncons s'
        (c, s''') <- uncons s''
        return (a, b, c, s''')
    cons3 a b c s = a `cons` b `cons` c `cons` s
    uncons4 s       = do
        (a, s')    <- uncons s
        (b, s'')   <- uncons s'
        (c, s''')  <- uncons s''
        (d, s'''') <- uncons s'''
        return (a, b, c, d, s'''')
    cons2 a b s = a `cons` b `cons` s
    uncons2 s       = do
        (a, s')   <- uncons s
        (b, s'')  <- uncons s'
        return (a, b, s'')

    uncons s = pure (,) <*> head s <*> tail s
    unsnoc s = pure (,) <*> init s <*> last s

class StringChar c where
    toChar    :: c -> Char
    toWord8   :: c -> Word8
    fromChar  :: Char -> c
    fromWord8 :: Word8 -> c

class StringLength s where
    length :: (Integral i) => s -> i

class StringEmpty s where
    empty :: s
    null :: s -> Bool

class StringPack s where
    pack :: String -> s
    unpack :: s -> String

class StringString s where
    toString :: s -> String
    fromString :: String -> s

class StringStrictByteString s where
    toStrictByteString :: s -> S.ByteString
    fromStrictByteString :: S.ByteString -> s

class StringLazyByteString s where
    toLazyByteString :: s -> L.ByteString
    fromLazyByteString :: L.ByteString -> s

class StringText s where
    toText :: s -> T.Text
    fromText :: T.Text -> s

instance StringConstruct String where
    type StringConstructChar String = Char
    type StringConstructAltChar String = Char
    cons          = (:)
    snoc s c      = s ++ [c]
    uncons (x:xs) = Just (x, xs)
    uncons _      = Nothing
    toMainChar _  = toChar
    toAltChar  _  = toChar
    head (x:_)    = Just x
    head _        = Nothing
    tail (_:xs)   = Just xs
    tail _        = Nothing
    init s
        | List.null s = Nothing
        | otherwise   = Just . List.init $ s
    last s
        | List.null s = Nothing
        | otherwise   = Just . List.last $ s

instance StringConstruct S.ByteString where
    type StringConstructChar S.ByteString = Word8
    type StringConstructAltChar S.ByteString = Char
    cons            = S.cons
    snoc            = S.snoc
    uncons          = S.uncons
    toMainChar _    = toWord8
    toAltChar  _    = toChar
    head s
        | S.null s  = Nothing
        | otherwise = Just . S.head $ s
    tail s
        | S.null s  = Nothing
        | otherwise = Just . S.tail $ s
    init s
        | S.null s  = Nothing
        | otherwise = Just . S.init $ s
    last s
        | S.null s  = Nothing
        | otherwise = Just . S.last $ s

instance StringConstruct L.ByteString where
    type StringConstructChar L.ByteString = Word8
    type StringConstructAltChar L.ByteString = Char
    cons            = L.cons
    snoc            = L.snoc
    uncons          = L.uncons
    toMainChar _    = toWord8
    toAltChar  _    = toChar
    head s
        | L.null s  = Nothing
        | otherwise = Just . L.head $ s
    tail s
        | L.null s  = Nothing
        | otherwise = Just . L.tail $ s
    init s
        | L.null s  = Nothing
        | otherwise = Just . L.init $ s
    last s
        | L.null s  = Nothing
        | otherwise = Just . L.last $ s

instance StringConstruct T.Text where
    type StringConstructChar T.Text = Char
    type StringConstructAltChar T.Text = Char
    cons            = T.cons
    uncons          = T.uncons
    snoc            = T.snoc
    altSnoc         = T.snoc
    toMainChar _    = toChar
    toAltChar  _    = toChar
    head s
        | T.null s  = Nothing
        | otherwise = Just . T.head $ s
    tail s
        | T.null s  = Nothing
        | otherwise = Just . T.tail $ s
    init s
        | T.null s  = Nothing
        | otherwise = Just . T.init $ s
    last s
        | T.null s  = Nothing
        | otherwise = Just . T.last $ s

instance StringChar Char where
    toChar = id
    toWord8 = BI.c2w
    fromChar = id
    fromWord8 = BI.w2c

instance StringChar Word8 where
    toChar = BI.w2c
    toWord8 = id
    fromChar = BI.c2w
    fromWord8 = id

instance StringLength String where
    length = List.genericLength

instance StringLength S.ByteString where
    length = fromIntegral . S.length

instance StringLength L.ByteString where
    length = fromIntegral . L.length

instance StringLength T.Text where
    length = fromIntegral . T.length

instance StringEmpty String where
    empty = []
    null  = List.null

instance StringEmpty S.ByteString where
    empty = S.empty
    null  = S.null

instance StringEmpty L.ByteString where
    empty = L.empty
    null  = L.null

instance StringEmpty T.Text where
    empty = T.empty
    null  = T.null

instance StringPack String where
    pack   = id
    unpack = id

instance StringPack SC.ByteString where
    pack   = SC.pack
    unpack = SC.unpack

instance StringPack LC.ByteString where
    pack   = LC.pack
    unpack = LC.unpack

instance StringPack T.Text where
    pack   = T.pack
    unpack = T.unpack

instance StringString String where
    toString   = pack
    fromString = unpack

instance StringString SC.ByteString where
    toString   = unpack
    fromString = pack

instance StringString LC.ByteString where
    toString   = unpack
    fromString = pack

instance StringString T.Text where
    toString   = unpack
    fromString = pack

instance StringStrictByteString String where
    toStrictByteString   = pack
    fromStrictByteString = unpack

instance StringStrictByteString S.ByteString where
    toStrictByteString   = id
    fromStrictByteString = id

instance StringStrictByteString L.ByteString where
    toStrictByteString   = S.concat . L.toChunks
    fromStrictByteString = toLazyByteString

instance StringStrictByteString T.Text where
    toStrictByteString   = TE.encodeUtf8
    fromStrictByteString = toText

instance StringLazyByteString String where
    toLazyByteString   = pack
    fromLazyByteString = unpack

instance StringLazyByteString S.ByteString where
    toLazyByteString   = L.fromChunks . (:[])
    fromLazyByteString = toStrictByteString

instance StringLazyByteString L.ByteString where
    toLazyByteString   = id
    fromLazyByteString = id

instance StringLazyByteString T.Text where
    toLazyByteString   = toLazyByteString . toStrictByteString
    fromLazyByteString = toText

instance StringText String where
    toText   = pack
    fromText = unpack

instance StringText S.ByteString where
    toText   = TE.decodeUtf8With TEE.lenientDecode
    fromText = toStrictByteString

instance StringText L.ByteString where
    toText   = toText . toStrictByteString
    fromText = toLazyByteString

instance StringText T.Text where
    toText   = id
    fromText = id
