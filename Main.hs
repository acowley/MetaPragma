{-# LANGUAGE LambdaCase #-}
import Data.List (intercalate, isInfixOf, isPrefixOf, nub)
import System.Environment (getArgs)
import Data.Maybe (mapMaybe)

-- | This should be a list of extensions that a plurality of people
-- can agree upon. It should change in 2016.
haskell2015 :: [String]
haskell2015 = [ "ConstraintKinds"
              , "DataKinds"
              , "FlexibleContexts"
              , "FlexibleInstances"
              , "GADTs"
              , "LambdaCase"
              , "MultiParamTypeClasses"
              , "NoImplicitPrelude"
              , "TypeFamilies"
              , "ScopedTypeVariables" ]

defns :: [(String, [String])]
defns = [("Haskell2015", haskell2015)]

closeBlockComment :: [String] -> ([String] -> [String] -> r) -> r
closeBlockComment lns k = uncurry k $ go lns ([],[])
  where go [] acc = acc
        go (x:xs) (pre,post)
          | "-}" `isInfixOf` x = ([x], xs)
          | otherwise = (x:pre, post)

takeUntilSuffix :: String -> String -> String
takeUntilSuffix suffix = go
  where go [] = []
        go str@(s:ss) | suffix `isPrefixOf` str = []
                      | otherwise = s : go ss

substituteMetaPragmas :: String -> String
substituteMetaPragmas = unlines . go . lines
  where go [] = []
        go (ln : lns)
          | "-- " `isPrefixOf` ln = ln : go lns
          | "{-# META" `isInfixOf` ln = closeBlockComment (ln:lns) $ \pre post ->
                                        aux pre : go post
          | "{-#" `isInfixOf` ln = ln : 
                                   (closeBlockComment (ln:lns) $ \pre post ->
                                   pre ++ go post)
          | otherwise = ln:lns
        aux (r:rest) = 
          subst . metas $ 
          drop 8 r ++ takeUntilSuffix "#-}" (unlines rest)
        aux [] = error "Something went wrong parsing a META comment block"
        metas :: String -> [String]
        metas = words . map (\c -> if c == ',' then ' ' else c)
        subst :: [String] -> String
        subst = mkLang . intercalate ", " . concat
              . mapMaybe (flip lookup defns) . nub
        mkLang s = "{-# LANGUAGE " ++ s ++ " #-}"

usage :: IO ()
usage = error msg
  where msg = unlines 
              [ "Usage: ghc -F -pgmF metapragma myFile"
              , "Or: {-# OPTIONS_GHC -F -pgmF metapragma #-} in your source file"
              , ""
              , "Replaces known metapragmas with their constituent "
              , "GHC LANGUAGE pragmas." ]

main :: IO ()
main = getArgs >>= \case
         [_orig, input, output] -> readFile input
                                   >>= writeFile output . substituteMetaPragmas
         _ -> usage
         
