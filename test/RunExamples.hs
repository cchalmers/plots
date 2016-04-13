import           Data.List            (isInfixOf)
import           System.FilePath      ((<.>), (</>), (-<.>), takeDirectory, takeBaseName)
import           Development.Shake

main :: IO ()
main = shake shakeOptions $ do
  "examples_output/*.png" %> \out -> do
    let src    = "examples" </> takeBaseName out -<.> "hs"
        outDir = takeDirectory out
    need [src]
    command_ [] "stack" ["runghc", src, "--", "-o", out, "-w", "600"]

  action $ do
    sourceFiles <- getDirectoryFiles "" ["examples/*.hs"]
    let examplesToSkip = ["table"]
        isValid file = not $ any (`isInfixOf` file) examplesToSkip
    need ["examples_output" </> takeBaseName file -<.> "png" | file <- sourceFiles, isValid file]
