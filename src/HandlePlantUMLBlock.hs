import Text.Pandoc.JSON
import Data.ByteString.Lazy (hGetContents, hPut)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.IO
       (hClose, hPutStr, IOMode(..), openBinaryFile, Handle)
import System.Process

processBlocks :: Maybe Format -> Block -> IO Block
processBlocks format cb@(CodeBlock (_, classes, _) contents) =
  if elem "plantuml" classes || elem "{plantuml}" classes -- stupid - only because of atom markdown-preview-enhanced!!
    then plantUMLToImg format contents
    else return cb
processBlocks _ cb = return cb

plantUMLToImg :: Maybe Format -> String -> IO Block
plantUMLToImg format content = do
  path <- renderImage format content
  return $ Para [Image nullAttr [] (path, "")]

renderImage :: Maybe Format -> String -> IO String
renderImage format content = do
  let path = uniqueName content ++ "." ++ imgFormat format
  (Just hIn, Just hOut, _, _) <-
    createProcess
      (proc "/bin/sh" ["plantuml", "-pipe", "-t" ++ imgFormat format]) -- shell won't work; I don't know why
      { std_in = CreatePipe
      , std_out = CreatePipe
      }
  hPutStr hIn content
  hClose hIn
  writeImageFile hOut path
  hClose hOut
  return path
  where
    imgFormat :: Maybe Format -> String
    imgFormat (Just (Format s))
      | s `elem` ["pdf", "latex"] = "latex"
      | s `elem` ["html", "markdown"] = "svg"
      | otherwise = "png"
    imgFormat _ = "png"
    uniqueName :: String -> String
    uniqueName = showDigest . sha1 . fromString
    writeImageFile :: Handle -> String -> IO ()
    writeImageFile hOut path = do
      hFile <- openBinaryFile path WriteMode
      img <- hGetContents hOut
      hPut hFile img
      hClose hFile

main :: IO ()
main = toJSONFilter processBlocks
