module Rules.Rts (rtsRules, needRtsSymLinks) where

import Packages (rts)
import Hadrian.Utilities
import Settings.Builders.Common

-- | Dynamic RTS library files need symlinks without the dummy version number.
-- This is for backwards compatibility (the old make build system omitted the
-- dummy version number).
-- This rule has priority 3 to override the general rule for generating shared
-- library files (see Rules.Library.libraryRules).
rtsRules :: Rules ()
rtsRules = priority 3 $ do
    root <- buildRootRules
    [ root -/- "//libHSrts_*-ghc*.so",
      root -/- "//libHSrts_*-ghc*.dylib",
      root -/- "//libHSrts-ghc*.so",
      root -/- "//libHSrts-ghc*.dylib"]
      |%> \ rtsLibFilePath' -> createFileLinkUntracked
            (addRtsDummyVersion $ takeFileName rtsLibFilePath')
            rtsLibFilePath'

-- Need symlinks generated by rtsRules.
needRtsSymLinks :: Stage -> [Way] -> Action ()
needRtsSymLinks stage rtsWays
    = forM_ (filter (wayUnit Dynamic) rtsWays) $ \ way -> do
        let ctx = Context stage rts way
        libPath     <- libPath ctx
        distDir     <- distDir stage
        rtsLibFile  <- takeFileName <$> pkgLibraryFile ctx
        need [removeRtsDummyVersion (libPath </> distDir </> rtsLibFile)]

prefix, versionlessPrefix :: String
versionlessPrefix = "libHSrts"
prefix = versionlessPrefix ++ "-1.0"

-- removeRtsDummyVersion "a/libHSrts-1.0-ghc1.2.3.4.so"
--                    == "a/libHSrts-ghc1.2.3.4.so"
removeRtsDummyVersion :: FilePath -> FilePath
removeRtsDummyVersion = replaceLibFilePrefix prefix versionlessPrefix

-- addRtsDummyVersion "a/libHSrts-ghc1.2.3.4.so"
--                 == "a/libHSrts-1.0-ghc1.2.3.4.so"
addRtsDummyVersion :: FilePath -> FilePath
addRtsDummyVersion = replaceLibFilePrefix versionlessPrefix prefix

replaceLibFilePrefix :: String -> String -> FilePath -> FilePath
replaceLibFilePrefix oldPrefix newPrefix oldFilePath = let
    oldFileName = takeFileName oldFilePath
    newFileName = maybe
        (error $ "Expected RTS library file to start with " ++ oldPrefix)
        (newPrefix ++)
        (stripPrefix oldPrefix oldFileName)
    in replaceFileName oldFilePath newFileName