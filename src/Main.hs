{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Unbound.Generics.LocallyNameless
import           Text.Megaparsec
import           Text.PrettyPrint.HughesPJ
import           System.Environment
import           System.Exit
import           Data.Text
import           Data.Either                    ( partitionEithers )
import qualified Data.Text.IO                  as TIO
import qualified Data.Map                      as M

import           Syntax
import           Env
import           Eval
import           Parser
import           Pretty

mainFunc :: TmName
mainFunc = string2Name "main"

go :: Text -> IO ()
go t = case parseProgram t of
    Left  err -> putStrLn $ errorBundlePretty err
    Right r   -> do
        let (bad, good) = partitionEithers r
        let env         = toEnv good initEnv
        case M.lookup mainFunc env of
            Nothing ->
                putStrLn "no main function found. Nothing to do." >> exitFailure
            Just m -> do
                res <- runEval env (eval m)
                case res of
                    Left  err       -> TIO.putStrLn err
                    Right (t, stps) -> do
                        TIO.putStrLn . pack . render . disp $ t
                        mapM_ (TIO.putStrLn . pack . render . disp) stps

main :: IO ()
main = do
    (f : _) <- getArgs
    program <- TIO.readFile f
    go program
