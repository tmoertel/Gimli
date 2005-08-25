{-# OPTIONS -fglasgow-exts #-}

module LoadData (
    loadCsvTable, loadWsvTable
)
where

import Control.Monad.Error
import Data.Either
import Text.ParserCombinators.Parsec (parseFromFile)

import CSV.Parser
import WSV.Parser
import Parser
import Value

loadCsvTable :: (MonadIO m) => String -> m Value
loadCsvTable path =
    loadTable csvFile path

loadWsvTable :: (MonadIO m) => String -> m Value
loadWsvTable path =
    loadTable wsvFile path

loadTable parser path =
    runErrorT (loadFile parser path >>= gimlParseTable) >>=
    return . either VError VTable

loadFile parser path =
    liftIO (parseFromFile parser path) >>=
    either (throwError . show) return
