module Main (
    main,
) where

import Data.Text.IO qualified as Text
import Hedger.Config (loadCatalog)
import Hedger.Runtime.Server (runServer)
import System.Exit (exitFailure)
import System.IO (stderr)

main :: IO ()
main = do
    result <- loadCatalog
    case result of
        Left problem -> Text.hPutStrLn stderr problem >> exitFailure
        Right configuration -> runServer configuration
