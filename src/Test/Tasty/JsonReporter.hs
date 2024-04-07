{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Test.Tasty.JsonReporter
-- Copyright: Copyright © 2020 Lars Kuhtz <lakuhtz@gmail.com>
-- License: MIT
-- Maintainer: Lars Kuhtz <lakuhtz@gmail.com>
-- Stability: experimental
--
-- JSON reporter for Tasty
--
module Test.Tasty.JsonReporter
( jsonReporter
, consoleAndJsonReporter
) where

import Control.Concurrent.STM
import Control.Monad

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.IntMap as M
import Data.Proxy
import Data.Tagged
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word

import System.IO

import Test.Tasty.Ingredients
import Test.Tasty.Options
import Test.Tasty.Runners

-- -------------------------------------------------------------------------- --
-- Primitive JSON encodings

-- | According to [ECMA-404](https://www.json.org) only @"@ and @\\@ must be
-- escaped. For a more readable result we include some optional escape
-- sequences.
--
escaped :: BP.BoundedPrim Word8
escaped
    = BP.condB (== w '\b') (e 'b')
    $ BP.condB (== w '\f') (e 'f')
    $ BP.condB (== w '\n') (e 'n')
    $ BP.condB (== w '\r') (e 'r')
    $ BP.condB (== w '\t') (e 't')
    $ BP.condB (== w '"') (e '"')
    $ BP.condB (== w '\\') (e '\\')
    $ BP.liftFixedToBounded BP.word8
  where
    w :: Char -> Word8
    w c = toEnum (fromEnum c)
    {-# INLINE w #-}

    e :: Char -> BP.BoundedPrim Word8
    e c = BP.liftFixedToBounded (const (w '\\', w c) BP.>$< (BP.word8 BP.>*< BP.word8))
    {-# INLINE e #-}
{-# INLINE escaped #-}

quoted :: BB.Builder -> BB.Builder
quoted a = BB.char7 '"' <> a <> BB.char7 '"'
{-# INLINE quoted #-}

text :: T.Text -> BB.Builder
text = quoted . T.encodeUtf8BuilderEscaped escaped
{-# INLINE text #-}

string :: String -> BB.Builder
string = text . T.pack
{-# INLINE string #-}

int :: Int -> BB.Builder
int = BB.intDec
{-# INLINE int #-}

double :: Double -> BB.Builder
double = BB.doubleDec
{-# INLINE double #-}

bool :: Bool -> BB.Builder
bool True = "true"
bool False = "false"
{-# INLINE bool #-}

nul :: BB.Builder
nul = "null"
{-# INLINE nul #-}

-- -------------------------------------------------------------------------- --
-- Encode JSON Objects

data Object = EmptyObject | Object BB.Builder

instance Semigroup Object where
    EmptyObject <> a = a
    a <> EmptyObject = a
    (Object a) <> (Object b) = Object (a <> "," <> b)
    {-# INLINE (<>) #-}

instance Monoid Object where
    mempty = EmptyObject
    {-# INLINE mempty #-}

assoc :: T.Text -> BB.Builder -> Object
assoc key value = Object $ text key <> ":" <> value
{-# INLINE assoc #-}

(.=) :: T.Text -> BB.Builder -> Object
(.=) = assoc
{-# INLINE (.=) #-}

object :: Object -> BB.Builder
object EmptyObject = "{" <> "}"
object (Object e) = "{" <> e <> "}"
{-# INLINE object #-}

-- -------------------------------------------------------------------------- --
-- Encode JSON Arrays

data Array = EmptyArray | Array BB.Builder

instance Semigroup Array where
    EmptyArray <> a = a
    a <> EmptyArray = a
    (Array a) <> (Array b) = Array (a <> "," <> b)
    {-# INLINE (<>) #-}

instance Monoid Array where
    mempty = EmptyArray
    {-# INLINE mempty #-}

item :: BB.Builder -> Array
item = Array
{-# INLINE item #-}

array :: Array -> BB.Builder
array EmptyArray = "[" <> "]"
array (Array e) = "[" <> e <> "]"
{-# INLINE array #-}

-- -------------------------------------------------------------------------- --
-- JSON Encoding for results

resultEncoding :: String -> Result -> BB.Builder
resultEncoding n r = object
    $ "name" .= string n
    <> "success" .= bool (resultSuccessful r)
    <> "failure" .= case resultOutcome r of
        Success -> nul
        Failure reason -> string (show reason)
    <> "description" .= string (resultDescription r)
    <> "summary" .= string (resultShortDescription r)
    <> "time" .= double (resultTime r)
    <> "startTime" .= double (resultStartTime r)

-- -------------------------------------------------------------------------- --
-- JSON Result file Option

newtype ResultsFile = ResultsFile { _getResultFile :: FilePath }

instance IsOption (Maybe ResultsFile) where
    defaultValue = Nothing
    parseValue = Just . Just . ResultsFile
    optionName = Tagged "results-json"
    optionHelp = Tagged "Filepath where results are stored in JSON format"

resultOption :: [ OptionDescription ]
resultOption = [ Option $ Proxy @(Maybe ResultsFile) ]

-- -------------------------------------------------------------------------- --
-- Reporters

awaitTest :: StatusMap -> Int -> IO Result
awaitTest smap i = atomically $ readTVar (smap M.! i) >>= \case
    Done x -> pure x
    _      -> retry

jsonReporter :: Ingredient
jsonReporter = TestReporter resultOption $ \opts tree -> do
    filePath <- _getResultFile <$> lookupOption opts
    Just $ \smap -> do
        let nthreads = getNumThreads $ lookupOption opts

            -- testNames traverses the test tree (taking into account options
            -- that affect the shape of the tree) and returns the test names in
            -- the same order as the appear in smap.
            tests = zip [0..] $ testsNames opts tree
            go (x, s) (i, n) = do
                r <- awaitTest smap i
                let !success = x && resultSuccessful r
                let !results = s <> item (resultEncoding n r)
                return (success, results)
        (x, results) <- foldM go (True, mempty) tests

        return $ \(startTime, timeTaken) -> x <$ do
            withBinaryFile filePath WriteMode $ \h ->
                BB.hPutBuilder h $ object
                    $ "results" .= array results
                    <> "startTime" .= double startTime
                    <> "time" .= double timeTaken
                    <> "success" .= bool x
                    <> "threads" .= int nthreads
                    <> "testCount" .= int (length tests)

consoleAndJsonReporter :: Ingredient
consoleAndJsonReporter = composeReporters consoleTestReporter jsonReporter
