{-# OPTIONS -fglasgow-exts #-}

module LoadData (
    loadCSVTable
)
where

import Control.Monad.Error
import Data.Either
import Text.ParserCombinators.Parsec (parseFromFile)

import CSV.Parser
import Parser
import Value

loadCSVTable :: (MonadIO m) => String -> m Value
loadCSVTable path =
    loadTable loadCSV path

loadTable loader path =
    runErrorT (loader path >>= gimlParseTable) >>=
    return . either VError VTable

loadCSV :: (MonadIO m, MonadError String m) => String -> m [[String]]
loadCSV path =
    liftIO (parseFromFile csvFile path) >>=
    either (throwError . show) return
