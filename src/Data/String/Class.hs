{-# LANGUAGE TypeFamilies, FlexibleContexts, TypeSynonymInstances, ExistentialQuantification, DeriveDataTypeable #-}

-- | This module is mostly self-explanatory

module Data.String.Class
    ( StringCells(..)
    , StringCell(..)
    , ConvGenString(..)
    , ConvString(..)
    , ConvStrictByteString(..)
    , ConvLazyByteString(..)
    , ConvText(..)
    , GenString(..)
    , GenStringDefault
    ) where

import Prelude hiding (head, tail, last, init, take, drop, length, null, concat)
import Control.Applicative hiding (empty)
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
class (StringCell (StringCellChar s), StringCell (StringCellAltChar s), ConvGenString s, ConvString s, ConvStrictByteString s, ConvLazyByteString s, ConvText s, Eq s, Typeable s) => StringCells s where
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

    -- | Construction of a string; implementations should behave safely with invalid lengths
    --
    -- The default implementation of 'undfoldr' is independent from that of 'altUnfoldr',
    -- as well as 'unfoldrN' as and 'altUnfoldrN'.
    unfoldr     ::        (a -> Maybe (StringCellChar    s, a)) -> a -> s
    altUnfoldr  ::        (a -> Maybe (StringCellAltChar s, a)) -> a -> s
    unfoldrN    :: Int -> (a -> Maybe (StringCellChar    s, a)) -> a -> s
    altUnfoldrN :: Int -> (a -> Maybe (StringCellAltChar s, a)) -> a -> s

    unfoldr f b =
        case f b of
            (Just (a, new_b)) -> a `cons` unfoldr f new_b
            (Nothing)         -> empty

    altUnfoldr f b =
        case f b of
            (Just (a, new_b)) -> a `altCons` altUnfoldr f new_b
            (Nothing)         -> empty
    unfoldrN    = const unfoldr
    altUnfoldrN = const altUnfoldr

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
    toChar     :: c      -> Char
    toWord8    :: c      -> Word8
    toWord16   :: c      -> Word16
    toWord32   :: c      -> Word32
    toWord64   :: c      -> Word64
    fromChar   :: Char   -> c
    fromWord8  :: Word8  -> c
    fromWord16 :: Word16 -> c
    fromWord32 :: Word32 -> c
    fromWord64 :: Word64 -> c

class ConvGenString s where
    toGenString   :: s -> GenString
    fromGenString :: GenString -> s

class ConvString s where
    toString   :: s -> String
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
    unfoldr       = List.unfoldr
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
    unfoldr         = S.unfoldr
    altUnfoldr      = SC.unfoldr
    unfoldrN        = ((fst .) .) . S.unfoldrN
    altUnfoldrN     = ((fst .) .) . SC.unfoldrN
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
    unfoldr         = L.unfoldr
    altUnfoldr      = LC.unfoldr
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
    unfoldr         = T.unfoldr
    altUnfoldr      = T.unfoldr
    unfoldrN        = T.unfoldrN
    altUnfoldrN     = T.unfoldrN
    index           = T.index
    index64 s       = index s . fromIntegral
    append          = T.append
    concat          = T.concat

instance StringCell Char where
    toChar     = id
    toWord8    = BI.c2w
    toWord16   = fromIntegral . toWord8
    toWord32   = fromIntegral . toWord8
    toWord64   = fromIntegral . toWord8
    fromChar   = id
    fromWord8  = BI.w2c
    fromWord16 = BI.w2c . fromIntegral
    fromWord32 = BI.w2c . fromIntegral
    fromWord64 = BI.w2c . fromIntegral

instance StringCell Word8 where
    toChar     = BI.w2c
    toWord8    = id
    toWord16   = fromIntegral
    toWord32   = fromIntegral
    toWord64   = fromIntegral
    fromChar   = BI.c2w
    fromWord8  = id
    fromWord16 = fromIntegral
    fromWord32 = fromIntegral
    fromWord64 = fromIntegral

instance StringCell Word16 where
    toChar     = BI.w2c . fromIntegral
    toWord8    = fromIntegral
    toWord16   = id
    toWord32   = fromIntegral
    toWord64   = fromIntegral
    fromChar   = fromIntegral . BI.c2w
    fromWord8  = fromIntegral
    fromWord16 = id
    fromWord32 = fromIntegral
    fromWord64 = fromIntegral

instance StringCell Word32 where
    toChar     = BI.w2c . fromIntegral
    toWord8    = fromIntegral
    toWord16   = fromIntegral
    toWord32   = id
    toWord64   = fromIntegral
    fromChar   = fromIntegral . BI.c2w
    fromWord8  = fromIntegral
    fromWord16 = fromIntegral
    fromWord32 = id
    fromWord64 = fromIntegral

instance StringCell Word64 where
    toChar     = BI.w2c . fromIntegral
    toWord8    = fromIntegral
    toWord16   = fromIntegral
    toWord32   = fromIntegral
    toWord64   = id
    fromChar   = fromIntegral . BI.c2w
    fromWord8  = fromIntegral
    fromWord16 = fromIntegral
    fromWord32 = fromIntegral
    fromWord64 = id

instance ConvGenString GenString where
    toGenString   = id
    fromGenString = id

instance ConvGenString String where
    toGenString      = GenString
    fromGenString _s = case _s of
        (GenString _s) -> toStringCells _s

instance ConvGenString SC.ByteString where
    toGenString      = GenString
    fromGenString _s = case _s of
        (GenString _s) -> toStringCells _s

instance ConvGenString LC.ByteString where
    toGenString      = GenString
    fromGenString _s = case _s of
        (GenString _s) -> toStringCells _s

instance ConvGenString T.Text where
    toGenString      = GenString
    fromGenString _s = case _s of
        (GenString _s) -> toStringCells _s

instance ConvString GenString where
    toString   = fromGenString
    fromString = toGenString

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

instance ConvStrictByteString GenString where
    toStrictByteString   = fromGenString
    fromStrictByteString = toGenString

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

instance ConvLazyByteString GenString where
    toLazyByteString   = fromGenString
    fromLazyByteString = toGenString

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

instance ConvText GenString where
    toText   = fromGenString
    fromText = toGenString

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

-- | Polymorphic container of a string
--
-- When operations take place on multiple 'GenString's, they are first
-- converted to the type 'GenStringDefault', which are lazy bytestrings,
-- whenever absolutely necessary (which includes testing for equality,
-- appending strings, concatenating lists of strings, empty strings with
-- 'empty', and unfolding), making them the most efficient type for this
-- type.
data GenString = forall s. (StringCells s) => GenString {gen_string :: s}
    deriving (Typeable)

toGenDefaultString :: (StringCells s) => s -> GenStringDefault
toGenDefaultString = toStringCells

genStringKey :: GenString
genStringKey = keyStringCells

instance Eq GenString where
    _a == _b = case (_a, _b) of
        ((GenString _a), (GenString _b)) -> toGenDefaultString _a == toGenDefaultString _b
    _a /= _b = case (_a, _b) of
        ((GenString _a), (GenString _b)) -> toGenDefaultString _a /= toGenDefaultString _b

instance StringCells GenString where
    -- These associated types were rather arbitrarily chosen
    type StringCellChar GenString = Char
    type StringCellAltChar GenString = Word8

    toStringCells   = fromGenString
    fromStringCells = toGenString

    cons c _s = case _s of
        (GenString _s) -> GenString $ cons (toMainChar _s c) _s
    uncons _s = case _s of
        (GenString _s) -> let (c, s') = uncons _s
                          in  (toMainChar genStringKey c, GenString s')
    snoc _s c = case _s of
        (GenString _s) -> GenString $ snoc _s (toMainChar _s c)
    unsnoc _s = case _s of
        (GenString _s) -> let (s', c) = unsnoc _s
                          in  (GenString s', toMainChar genStringKey c)

    altCons c _s = case _s of
        (GenString _s) -> GenString $ cons (fromWord8 c) _s
    altUncons _s = case _s of
        (GenString _s) -> let (c, s') = uncons _s
                          in  (toAltChar genStringKey c, GenString s')
    altSnoc _s c = case _s of
        (GenString _s) -> GenString $ snoc _s (fromWord8 c)
    altUnsnoc _s = case _s of
        (GenString _s) -> let (s', c) = unsnoc _s
                          in  (GenString s', toAltChar genStringKey c)

    toMainChar = const toChar
    toAltChar  = const toWord8

    append a b = case (a, b) of
        (GenString _a, GenString _b) -> GenString $ append (toGenDefaultString _a) (toGenDefaultString _b)
    concat ss = GenString $ concat . map toGenDefaultString $ ss

    empty = GenString $ (empty :: GenStringDefault)
    null _s = case _s of
        (GenString _s) -> null _s

    head _s = case _s of
        (GenString _s) -> toMainChar genStringKey $ head _s
    tail _s = case _s of
        (GenString _s) -> GenString $ tail _s
    last _s = case _s of
        (GenString _s) -> toMainChar genStringKey $ last _s
    init _s = case _s of
        (GenString _s) -> GenString $ init _s
    altHead _s = case _s of
        (GenString _s) -> toAltChar genStringKey $ head _s
    altLast _s = case _s of
        (GenString _s) -> toAltChar genStringKey $ last _s

    unfoldr       f z = GenString $ (altUnfoldr    f z  :: GenStringDefault)
    altUnfoldr    f z = GenString $ (unfoldr       f z  :: GenStringDefault)
    unfoldrN    n f z = GenString $ (altUnfoldrN n f z  :: GenStringDefault)
    altUnfoldrN n f z = GenString $ (unfoldrN    n f z  :: GenStringDefault)

    index _s i = case _s of
        (GenString _s) -> toMainChar genStringKey $ index _s i
    index64 _s i = case _s of
        (GenString _s) -> toMainChar genStringKey $ index64 _s i
    genericIndex _s i = case _s of
        (GenString _s) -> toMainChar genStringKey $ genericIndex _s i

    take n _s = case _s of
        (GenString _s) -> GenString $ take n _s
    take64 n _s = case _s of
        (GenString _s) -> GenString $ take64 n _s
    genericTake n _s = case _s of
        (GenString _s) -> GenString $ genericTake n _s
    drop n _s = case _s of
        (GenString _s) -> GenString $ drop n _s
    drop64 n _s = case _s of
        (GenString _s) -> GenString $ drop64 n _s
    genericDrop n _s = case _s of
        (GenString _s) -> GenString $ genericDrop n _s

    length _s = case _s of
        (GenString _s) -> length _s
    length64 _s = case _s of
        (GenString _s) -> length64 _s
    genericLength _s = case _s of
        (GenString _s) -> genericLength _s

    safeUncons _s = case _s of
        (GenString _s) -> (\(c, s') -> (toMainChar genStringKey c, GenString s')) <$> safeUncons _s
    safeUnsnoc _s = case _s of
        (GenString _s) -> (\(s', c) -> (GenString s', toMainChar genStringKey c)) <$> safeUnsnoc _s
    safeAltUncons _s = case _s of
        (GenString _s) -> (\(c, s') -> (toAltChar genStringKey c, GenString s')) <$> safeAltUncons _s
    safeAltUnsnoc _s = case _s of
        (GenString _s) -> (\(s', c) -> (GenString s', toAltChar genStringKey c)) <$> safeAltUnsnoc _s
    safeHead _s = case _s of
        (GenString _s) -> toMainChar genStringKey <$> safeHead _s
    safeTail _s = case _s of
        (GenString _s) -> GenString <$> safeTail _s
    safeLast _s = case _s of
        (GenString _s) -> toMainChar genStringKey <$> safeLast _s
    safeInit _s = case _s of
        (GenString _s) -> GenString <$> safeInit _s
    safeAltHead _s = case _s of
        (GenString _s) -> toAltChar  genStringKey <$> safeAltHead _s
    safeAltLast _s = case _s of
        (GenString _s) -> toAltChar  genStringKey <$> safeAltLast _s
    safeIndex _s i = case _s of
        (GenString _s) -> toMainChar genStringKey <$> safeIndex _s i
    safeIndex64 _s i = case _s of
        (GenString _s) -> toMainChar genStringKey <$> safeIndex64 _s i
    safeGenericIndex _s i = case _s of
        (GenString _s) -> toMainChar genStringKey <$> safeGenericIndex _s i
    safeTake n _s = case _s of
        (GenString _s) -> GenString <$> safeTake n _s
    safeTake64 n _s = case _s of
        (GenString _s) -> GenString <$> safeTake64 n _s
    safeGenericTake n _s = case _s of
        (GenString _s) -> GenString <$> safeGenericTake n _s
    safeDrop n _s = case _s of
        (GenString _s) -> GenString <$> safeDrop n _s
    safeDrop64 n _s = case _s of
        (GenString _s) -> GenString <$> safeDrop64 n _s
    safeGenericDrop n _s = case _s of
        (GenString _s) -> GenString <$> safeGenericDrop n _s
    safeUncons2 _s = case _s of
        (GenString _s) -> (\(a, b, s') -> (toMainChar genStringKey a, toMainChar genStringKey b, GenString s')) <$> safeUncons2 _s
    safeUncons3 _s = case _s of
        (GenString _s) -> (\(a, b, c, s') -> (toMainChar genStringKey a, toMainChar genStringKey b, toMainChar genStringKey c, GenString s')) <$> safeUncons3 _s
    safeUncons4 _s = case _s of
        (GenString _s) -> (\(a, b, c, d, s') -> (toMainChar genStringKey a, toMainChar genStringKey b, toMainChar genStringKey c, toMainChar genStringKey d, GenString s')) <$> safeUncons4 _s

    cons2 a b _s = case _s of
        (GenString _s) -> GenString $ cons2 (toMainChar _s a) (toMainChar _s b) _s
    cons3 a b c _s = case _s of
        (GenString _s) -> GenString $ cons3 (toMainChar _s a) (toMainChar _s b) (toMainChar _s c) _s
    cons4 a b c d _s = case _s of
        (GenString _s) -> GenString $ cons4 (toMainChar _s a) (toMainChar _s b) (toMainChar _s c) (toMainChar _s d) _s
    uncons2 _s = case _s of
        (GenString _s) -> let (a, b, s') = uncons2 _s
                          in  (toMainChar genStringKey a, toMainChar genStringKey b, GenString s')
    uncons3 _s = case _s of
        (GenString _s) -> let (a, b, c, s') = uncons3 _s
                          in  (toMainChar genStringKey a, toMainChar genStringKey b, toMainChar genStringKey c, GenString s')
    uncons4 _s = case _s of
        (GenString _s) -> let (a, b, c, d, s') = uncons4 _s
                          in  (toMainChar genStringKey a, toMainChar genStringKey b, toMainChar genStringKey c, toMainChar genStringKey d, GenString s')

-- | This type is used by 'GenString' when a concrete string type is needed
type GenStringDefault = L.ByteString
