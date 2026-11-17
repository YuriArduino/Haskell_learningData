module FileReceiver (main) where

import FileTransfer (receiveFile)

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr, stdin)


main :: IO ()
main = do
    args <- getArgs

    outputDir <- case args of
        [] ->
            pure "received"

        [directory] ->
            pure directory

        _ ->
            ioError (userError "Usage: hsTopy-file-receiver [output-directory]")

    outputPath <- receiveFile stdin outputDir

    hPutStrLn stderr $
        "File successfully received: " ++ outputPath
