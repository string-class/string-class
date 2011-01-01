{-# LANGUAGE TypeFamilies, FlexibleContexts, TypeSynonymInstances #-}

-- | This module is mostly self-explanatory

module Data.String.Class
    ( StringCells(..)
    , StringCell(..)
    , ConvString(..)
    , ConvStrictByteString(..)
    , ConvLazyByteString(..)
    , ConvText(..)
    ) where

import Prelude hiding (head, tail, last, init, take, drop, length, null)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Int
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Typeable
import Data.Word

type Unused a = a

-- | Minimal complete definition: StringCellChar; StringCellAltChar; toStringCells; fromStringCells; toMainChar; toAltChar; cons; snoc; either all of head, tail, last, and init, or all of uncons and unsnoc; take, take64 or genericTake; drop, drop64, or genericDrop; length, length64, or genericLength; empty; null; and concat
class (StringCell (StringCellChar s), StringCell (StringCellAltChar s), ConvString s, ConvStrictByteString s, ConvLazyByteString s, ConvText s, Eq s, Typeable s) => StringCells s where
    type StringCellChar s
    type StringCellAltChar s

    toStringCells   :: (StringCells s2) => s  -> s2
    fromStringCells :: (StringCells s2) => s2 -> s

    infixr 9 `cons`
    infixr 9 `uncons`
    infixr 9 `altCons`
    infixr 9 `altUncons`
    cons      :: StringCellChar s -> s -> s
    uncons    :: s -> (StringCellChar s, s)
    snoc      :: s -> StringCellChar s -> s
    unsnoc    :: s -> (s, StringCellChar s)
    altCons   :: StringCellAltChar s -> s -> s
    altUncons :: s -> (StringCellAltChar s, s)
    altSnoc   :: s -> StringCellAltChar s -> s
    altUnsnoc :: s -> (s, StringCellAltChar s)

    -- | The unused parameter is only needed for its type and should be ignored in the function definition
    toMainChar :: (StringCell c) => Unused s -> c -> StringCellChar s
    -- | The unused parameter is only needed for its type and should be ignored in the function definition
    toAltChar  :: (StringCell c) => Unused s -> c -> StringCellAltChar s

    -- | Append two strings
    infixr 9 `append`
    append :: s -> s -> s
    concat :: [s] -> s

    empty :: s
    null :: s -> Bool

    head :: s -> StringCellChar s
    tail :: s -> s
    last :: s -> StringCellChar s
    init :: s -> s
    altHead :: s -> StringCellAltChar s
    altLast :: s -> StringCellAltChar s

    -- | Get the character at the given position
    --
    -- The default definitions are independent of each other,
    -- and work in terms of 'head' and 'tail', which can be
    -- inefficient.
    index   :: s -> Int   -> StringCellChar s
    index64 :: s -> Int64 -> StringCellChar s
    -- | Index a string at any location
    --
    -- This function can be significantly slower than 'index', since
    -- the function must be able to support arbitrarily large
    -- indices.  Consider using 'index' or 'index64', even if you need to
    -- coerce the index to an 'Int'.
    genericIndex :: (Integral i) => s -> i -> StringCellChar s

    take        :: Int -> s -> s
    take64      :: Int64 -> s -> s
    genericTake :: (Integral i) => i -> s -> s
    drop        :: Int -> s -> s
    drop64      :: Int64 -> s -> s
    genericDrop :: (Integral i) => i -> s -> s

    length        :: s -> Int
    length64      :: s -> Int64
    genericLength :: (Integral i) => s -> i

    safeUncons        :: s -> Maybe ((StringCellChar s), s)
    safeUnsnoc        :: s -> Maybe (s, (StringCellChar s))
    safeAltUncons     :: s -> Maybe ((StringCellAltChar s), s)
    safeAltUnsnoc     :: s -> Maybe (s, (StringCellAltChar s))
    safeHead          :: s -> Maybe (StringCellChar s)
    safeTail          :: s -> Maybe s
    safeLast          :: s -> Maybe (StringCellChar s)
    safeInit          :: s -> Maybe s
    safeAltHead       :: s -> Maybe (StringCellAltChar s)
    safeAltLast       :: s -> Maybe (StringCellAltChar s)
    safeIndex         :: s -> Int   -> Maybe (StringCellChar s)
    safeIndex64       :: s -> Int64 -> Maybe (StringCellChar s)
    safeGenericIndex  :: (Integral i) => s -> i -> Maybe (StringCellChar s)
    safeTake          :: Int -> s -> Maybe s
    safeTake64        :: Int64 -> s -> Maybe s
    safeGenericTake   :: (Integral i) => i -> s -> Maybe s
    safeDrop          :: Int -> s -> Maybe s
    safeDrop64        :: Int64 -> s -> Maybe s
    safeGenericDrop   :: (Integral i) => i -> s -> Maybe s
    safeUncons2       :: s -> Maybe ((StringCellChar s), (StringCellChar s), s)
    safeUncons3       :: s -> Maybe ((StringCellChar s), (StringCellChar s), (StringCellChar s), s)
    safeUncons4       :: s -> Maybe ((StringCellChar s), (StringCellChar s), (StringCellChar s), (StringCellChar s), s)

    infixr 9 `cons2`
    infixr 9 `cons3`
    infixr 9 `cons4`
    infixr 9 `uncons2`
    infixr 9 `uncons3`
    infixr 9 `uncons4`
    cons2   :: StringCellChar s -> StringCellChar s -> s -> s
    cons3   :: StringCellChar s -> StringCellChar s -> StringCellChar s -> s -> s
    cons4   :: StringCellChar s -> StringCellChar s -> StringCellChar s -> StringCellChar s -> s -> s
    uncons2 :: s -> (StringCellChar s, StringCellChar s, s)
    uncons3 :: s -> (StringCellChar s, StringCellChar s, StringCellChar s, s)
    uncons4 :: s -> (StringCellChar s, StringCellChar s, StringCellChar s, StringCellChar s, s)

    -- | An undefined value meant to disambiguate which instance of 'toMainChar' or 'toAltChar' to use
    --
    -- It is thus normally unnecessary to define in instances.
    keyStringCells :: s
    keyStringCells = undefined

    altCons c s = cons (toMainChar s c) s
    altSnoc s c = snoc s (toMainChar s c)
    altUncons s = (\ ~(a, s') -> (toAltChar s a, s')) $ uncons s
    altUnsnoc s = (\ ~(s', a) -> (s', toAltChar s a)) $ unsnoc s

    head = fst . uncons
    tail = snd . uncons
    last = snd . unsnoc
    init = fst . unsnoc
    altHead s = toAltChar s . head $ s
    altLast s = toAltChar s . last $ s

    index        s 0 = head s
    index        s n = (flip index $ pred n) . tail $ s
    index64      s 0 = head s
    index64      s n = (flip index64 $ pred n) . tail $ s
    genericIndex s 0 = head s
    genericIndex s n = (flip genericIndex $ pred n) . tail $ s

    take        n s = take64      (fromIntegral n) s
    take64      n s = genericTake (fromIntegral n  :: Integer) s
    genericTake n s = take        (fromIntegral n) s
    drop        n s = drop64      (fromIntegral n) s
    drop64      n s = genericDrop (fromIntegral n  :: Integer) s
    genericDrop n s = drop        (fromIntegral n) s

    length        = fromIntegral . length64
    length64      = (fromIntegral :: Integer -> Int64) . genericLength
    genericLength = fromIntegral . length

    append a b = case safeUncons a of
        (Just (c, cs)) -> c `cons` append cs b
        (Nothing)      -> a

    concat = foldr append empty

    uncons s = (head s, tail s)
    unsnoc s = (init s, last s)

    cons2 a b s = a `cons` b `cons` s
    cons3 a b c s = a `cons` b `cons` c `cons` s
    cons4 a b c d s = a `cons` b `cons` c `cons` d `cons` s
    uncons2 s       =
        let (a, s')   = uncons s
            (b, s'')  = uncons s'
        in  (a, b, s'')
    uncons3 s       =
        let (a, s')   = uncons s
            (b, s'')  = uncons s'
            (c, s''') = uncons s''
        in  (a, b, c, s''')
    uncons4 s       =
        let (a, s')    = uncons s
            (b, s'')   = uncons s'
            (c, s''')  = uncons s''
            (d, s'''') = uncons s'''
        in  (a, b, c, d, s'''')

    safeUncons s
        | null s    = Nothing
        | otherwise = Just $ uncons s
    safeUnsnoc s
        | null s    = Nothing
        | otherwise = Just $ unsnoc s
    safeAltUncons s
        | null s    = Nothing
        | otherwise = Just $ altUncons s
    safeAltUnsnoc s
        | null s    = Nothing
        | otherwise = Just $ altUnsnoc s
    safeHead s
        | null s    = Nothing
        | otherwise = Just $ head s
    safeTail s
        | null s    = Nothing
        | otherwise = Just $ tail s
    safeLast s
        | null s    = Nothing
        | otherwise = Just $ last s
    safeInit s
        | null s    = Nothing
        | otherwise = Just $ init s
    safeAltHead s
        | null s    = Nothing
        | otherwise = Just $ altHead s
    safeAltLast s
        | null s    = Nothing
        | otherwise = Just $ altLast s
    safeIndex s n
        | length s <= n = Nothing
        | otherwise     = Just $ s `index` n
    safeIndex64 s n
        | length64 s <= n = Nothing
        | otherwise     = Just $ s `index64` n
    safeGenericIndex s n
        | genericLength s <= n = Nothing
        | otherwise            = Just $ s `genericIndex` n
    safeTake n s
        | n > length s = Nothing
        | otherwise    = Just $ take n s
    safeTake64 n s
        | n > length64 s = Nothing
        | otherwise      = Just $ take64 n s
    safeGenericTake n s
        | n > genericLength s = Nothing
        | otherwise           = Just $ genericTake n s
    safeDrop n s
        | n > length s = Nothing
        | otherwise    = Just $ drop n s
    safeDrop64 n s
        | n > length64 s = Nothing
        | otherwise      = Just $ drop64 n s
    safeGenericDrop n s
        | n > genericLength s = Nothing
        | otherwise           = Just $ genericDrop n s
    safeUncons2 s = do
        (a, s')    <- safeUncons s
        (b, s'')   <- safeUncons s'
        return (a, b, s'')
    safeUncons3 s = do
        (a, s')    <- safeUncons s
        (b, s'')   <- safeUncons s'
        (c, s''')  <- safeUncons s''
        return (a, b, c, s''')
    safeUncons4 s = do
        (a, s')    <- safeUncons s
        (b, s'')   <- safeUncons s'
        (c, s''')  <- safeUncons s''
        (d, s'''') <- safeUncons s'''
        return (a, b, c, d, s'''')

class StringCell c where
    toChar    :: c -> Char
    toWord8   :: c -> Word8
    fromChar  :: Char -> c
    fromWord8 :: Word8 -> c

class ConvString s where
    toString :: s -> String
    fromString :: String -> s

class ConvStrictByteString s where
    toStrictByteString :: s -> S.ByteString
    fromStrictByteString :: S.ByteString -> s

class ConvLazyByteString s where
    toLazyByteString :: s -> L.ByteString
    fromLazyByteString :: L.ByteString -> s

class ConvText s where
    toText :: s -> T.Text
    fromText :: T.Text -> s

instance StringCells String where
    type StringCellChar    String = Char
    type StringCellAltChar String = Char

    toStringCells   = fromString
    fromStringCells = toString

    length = List.genericLength
    empty  = []
    null   = List.null
    cons          = (:)
    snoc s c      = s ++ [c]
    safeUncons (x:xs) = Just (x, xs)
    safeUncons _      = Nothing
    uncons (x:xs) = (x, xs)
    uncons _      = error "String.uncons: null string"
    toMainChar _  = toChar
    toAltChar  _  = toChar
    head          = List.head
    tail          = List.tail
    init          = List.init
    last          = List.last
    index         = (List.!!)
    index64 s     = index s . fromIntegral
    genericIndex  = List.genericIndex
    take          = List.take
    genericTake   = List.genericTake
    drop          = List.drop
    genericDrop   = List.genericDrop
    append        = (List.++)
    concat        = List.concat

instance StringCells S.ByteString where
    type StringCellChar    S.ByteString = Word8
    type StringCellAltChar S.ByteString = Char

    toStringCells   = fromStrictByteString
    fromStringCells = toStrictByteString

    length          = S.length
    empty           = S.empty
    null            = S.null
    cons            = S.cons
    snoc            = S.snoc
    safeUncons      = S.uncons
    uncons          = maybe (error "StringCells.Data.ByteString.ByteString.uncons: string is null") id . safeUncons
    toMainChar _    = toWord8
    toAltChar  _    = toChar
    head            = S.head
    tail            = S.tail
    init            = S.init
    last            = S.last
    index           = S.index
    index64 s       = index s . fromIntegral
    take            = S.take
    drop            = S.drop
    append          = S.append
    concat          = S.concat

instance StringCells L.ByteString where
    type StringCellChar    L.ByteString = Word8
    type StringCellAltChar L.ByteString = Char

    toStringCells   = fromLazyByteString
    fromStringCells = toLazyByteString

    length64        = L.length
    length          = fromIntegral . length64
    empty           = L.empty
    null            = L.null
    cons            = L.cons
    snoc            = L.snoc
    safeUncons      = L.uncons
    uncons          = maybe (error "StringCells.Data.ByteString.Lazy.ByteString.uncons: string is null") id . safeUncons
    toMainChar _    = toWord8
    toAltChar  _    = toChar
    head            = L.head
    tail            = L.tail
    init            = L.init
    last            = L.last
    index s         = index64 s . fromIntegral
    index64         = L.index
    take64          = L.take
    drop64          = L.drop
    append          = L.append
    concat          = L.concat

instance StringCells T.Text where
    type StringCellChar    T.Text = Char
    type StringCellAltChar T.Text = Char

    toStringCells   = fromText
    fromStringCells = toText

    length          = T.length
    empty           = T.empty
    null            = T.null
    cons            = T.cons
    safeUncons      = T.uncons
    uncons          = maybe (error "StringCells.Data.Text.Text.uncons: string is null") id . safeUncons
    snoc            = T.snoc
    altSnoc         = T.snoc
    toMainChar _    = toChar
    toAltChar  _    = toChar
    head            = T.head
    tail            = T.tail
    init            = T.init
    last            = T.last
    index           = T.index
    index64 s       = index s . fromIntegral
    append          = T.append
    concat          = T.concat

instance StringCell Char where
    toChar    = id
    toWord8   = BI.c2w
    fromChar  = id
    fromWord8 = BI.w2c

instance StringCell Word8 where
    toChar    = BI.w2c
    toWord8   = id
    fromChar  = BI.c2w
    fromWord8 = id

instance ConvString String where
    toString   = id
    fromString = id

instance ConvString SC.ByteString where
    toString   = SC.unpack
    fromString = SC.pack

instance ConvString LC.ByteString where
    toString   = LC.unpack
    fromString = LC.pack

instance ConvString T.Text where
    toString   = T.unpack
    fromString = T.pack

instance ConvStrictByteString String where
    toStrictByteString   = SC.pack
    fromStrictByteString = SC.unpack

instance ConvStrictByteString S.ByteString where
    toStrictByteString   = id
    fromStrictByteString = id

instance ConvStrictByteString L.ByteString where
    toStrictByteString   = S.concat . L.toChunks
    fromStrictByteString = toLazyByteString

instance ConvStrictByteString T.Text where
    toStrictByteString   = TE.encodeUtf8
    fromStrictByteString = toText

instance ConvLazyByteString String where
    toLazyByteString   = LC.pack
    fromLazyByteString = LC.unpack

instance ConvLazyByteString S.ByteString where
    toLazyByteString   = L.fromChunks . (:[])
    fromLazyByteString = toStrictByteString

instance ConvLazyByteString L.ByteString where
    toLazyByteString   = id
    fromLazyByteString = id

instance ConvLazyByteString T.Text where
    toLazyByteString   = toLazyByteString . toStrictByteString
    fromLazyByteString = toText

instance ConvText String where
    toText   = T.pack
    fromText = T.unpack

instance ConvText S.ByteString where
    toText   = TE.decodeUtf8With TEE.lenientDecode
    fromText = toStrictByteString

instance ConvText L.ByteString where
    toText   = toText . toStrictByteString
    fromText = toLazyByteString

instance ConvText T.Text where
    toText   = id
    fromText = id
