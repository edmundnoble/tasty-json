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

import Data.Aeson hiding (Result, Success)
import Data.Aeson.Encoding
import Data.Aeson.Encoding.Internal
import qualified Data.ByteString.Builder as BB
import qualified Data.IntMap as M
import Data.Proxy
import Data.Tagged

import System.IO

import Test.Tasty.Ingredients
import Test.Tasty.Options
import Test.Tasty.Runners

-- -------------------------------------------------------------------------- --
-- JSON Encoding Utils

data ArrayEnc = EmptyArray | ArrayEnc (Encoding' InArray)

instance Semigroup ArrayEnc where
    EmptyArray <> a = a
    a <> EmptyArray = a
    (ArrayEnc a) <> (ArrayEnc b) = ArrayEnc (a >*< b)
    {-# INLINE (<>) #-}

instance Monoid ArrayEnc where
    mempty = EmptyArray
    {-# INLINE mempty #-}

item :: Encoding -> ArrayEnc
item = ArrayEnc . retagEncoding
{-# INLINE item #-}

arrayEnc :: ArrayEnc -> Encoding
arrayEnc EmptyArray = emptyArray_
arrayEnc (ArrayEnc e) = tuple e
{-# INLINE arrayEnc #-}

-- -------------------------------------------------------------------------- --
-- JSON Encoding for results

resultEncoding :: String -> Result -> Encoding
resultEncoding n r = pairs
    $ "name" .= n
    <> "success" .= resultSuccessful r
    <> "failure" .= case resultOutcome r of
        Success -> Nothing
        Failure reason -> Just $ show reason
    <> "description" .= resultDescription r
    <> "summary" .= resultShortDescription r
    <> "time" .= resultTime r

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

        return $ \t -> x <$ do
            withBinaryFile filePath WriteMode $ \h ->
                BB.hPutBuilder h $ fromEncoding $ pairs
                    $ pair "results" (arrayEnc results)
                    <> "time" .= t
                    <> "success" .= x
                    <> "threads" .= nthreads
                    <> "testCount" .= length tests

consoleAndJsonReporter :: Ingredient
consoleAndJsonReporter = composeReporters consoleTestReporter jsonReporter

