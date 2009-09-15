#! /usr/bin/env runhaskell

{-
configure.hs for bindings-levmar

This file was completely copied from hmatrix-0.5.2.2 and then adjusted
for bindings-levmar.
-}

import System
import Data.List (isPrefixOf)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Configure
import Distribution.PackageDescription

-- possible additional dependencies for the desired libs (by default lapack)
opts :: [String]
opts = [ ""                          -- Ubuntu/Debian
       , "blas"
       , "blas cblas"
       , "cblas"
       , "f77blas cblas atlas gcc_s" -- Arch Linux (older version of atlas-lapack)
       , "blas gfortran"             -- Arch Linux with normal blas and lapack
       ]

-- Compile a simple program with symbols from LAPACK with the given libs.
testProg :: String -> String -> String
testProg libs fmks = "gcc levmar_lapack_test.c -o /tmp/dummy "
                   ++ f1 libs ++ " " ++ f2 fmks
                   ++ " > /dev/null 2> /dev/null"

f1, f2 :: String -> String
f1 = unwords . map ("-l" ++) . words
f2 = unwords . map ("-framework "++) . words

check :: String -> String -> IO Bool
check libs fmks = checkCommand (testProg libs fmks)

checkCommand :: String -> IO Bool
checkCommand c = (ExitSuccess ==) `fmap` system c

-- test different configurations until the first one works
try :: String -> String -> [String] -> IO (Maybe String)
try _ _ [] = return Nothing
try b f (opt:rest) = do
    ok <- check (b ++ " " ++ opt) f
    if ok then return (Just opt)
          else try b f rest

-- read --configure-option=link:lib1,lib2,lib3,etc
linkOp :: String
linkOp = "link:"

getUserLink :: [String] -> String
getUserLink = concatMap (g . drop (length linkOp)) . filter (isPrefixOf linkOp)
    where g = map cs
          cs ',' = ' '
          cs x   = x

main :: IO ()
main = do
    putStr "Checking foreign libraries..."

    args <- getArgs
    Just bInfo <- maybeGetPersistBuildConfig "dist"

    let Just lib = library . localPkgDescr $ bInfo
        base = unwords . extraLibs . libBuildInfo $ lib
        fwks = unwords . frameworks . libBuildInfo $ lib
        auxpref = getUserLink args

    -- We extract the desired libs from bindings-levmar.cabal (using cabal flags)
    -- and from a possible --configure-option=link:lib1,lib2,lib3
    -- by default the desired lib is lapack.

    let pref = if null (words (base ++ " " ++ auxpref)) then "lapack" else auxpref
        fullOpts = map ((pref ++ " ") ++ ) opts

    r <- try base fwks fullOpts
    case r of
        Nothing -> do
            putStrLn " FAIL"
            putStrLn " *** Sorry, I can't link LAPACK."
            putStrLn " *** Please make sure that the appropriate -dev packages are installed."
            putStrLn " *** You can also specify the required libraries using"
            putStrLn " *** cabal install bindings-levmar --configure-option=link:lib1,lib2,lib3,etc."
            writeFile "bindings-levmar.buildinfo" "buildable: False\n"
        Just ops -> do
            putStrLn " OK"
            writeFile "bindings-levmar.buildinfo" $ "extra-libraries: " ++ ops ++ "\n"
