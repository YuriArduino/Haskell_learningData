module FileSender (main) where

import FileTransfer (sendFile)

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr, stdout)


main :: IO ()
main = do
    args <- getArgs

    inputPath <- case args of
        [] ->
            pure "test/files/file_1.txt"

        [path] ->
            pure path

        _ ->
            ioError (userError "Usage: hsTopy-file-sender [input-file]")

    (total, count) <- sendFile stdout inputPath

    hPutStrLn stderr $
        "Transfer completed: "
        ++ inputPath
        ++ " (" ++ show total
        ++ " bytes, " ++ show count
        ++ " chunks)"
