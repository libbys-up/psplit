module Main where
import Analysis (f,splitName)
import FlatCurry.Files  (writeFlatCurryFile, readFlatCurry, readFlatCurryFile)
import FlatCurry.Goodies (progFuncs, funcName)

main :: IO ()
main = do prelude <- readFlatCurry "Prelude" 
          --let comps = orderFuns (progFuncs prelude)
          --mapM_ printComp comps
          --mapM_ (print . snd . funcName) $ progFuncs prelude
          --mapM_ (print . splitName . snd . funcName) $ progFuncs prelude
          mapM_ print (f $ progFuncs prelude)
          --mapM_ (\(x,y) -> putStrLn (x ++ " -- " ++ y)) (cg $ progFuncs prelude)


printComp :: [[(String,String)]] -> IO ()
printComp comp = mapM_ (putStrLn . unwords . map snd) comp >> putStrLn ""
