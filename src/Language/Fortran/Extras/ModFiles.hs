-- | This module provides functions for handling FORTRAN source modules.
module Language.Fortran.Extras.ModFiles
  ( isModFile
  , decodeModFiles
  )
where

import           Control.Monad                  ( foldM
                                                , forM
                                                )
import           Data.Binary                    ( decodeFileOrFail )
import           Language.Fortran.Util.ModFile  ( emptyModFile
                                                , emptyModFiles
                                                , ModFiles
                                                , modFileSuffix
                                                )
import           Language.Fortran.Util.Files    ( rGetDirContents )
import           System.FilePath                ( takeExtension
                                                , (</>)
                                                )

-- | Return TRUE iff the file extension indicates a module file.
isModFile :: String -> Bool
isModFile = (== modFileSuffix) . takeExtension

-- | Read suspected module files from a list of files and obtain the
-- 'Language.Fortran.Util.ModFiles' object populated by their contents.
--
-- TODO: almost equal to Language.Fortran.Analysis.ModGraph.decodeModFiles
decodeModFiles :: [FilePath] -> IO ModFiles
decodeModFiles = foldM
  (\modFiles d -> do
    modFileNames  <- filter isModFile `fmap` rGetDirContents d
    addedModFiles <- forM modFileNames $ \modFileName -> do
      eResult <- decodeFileOrFail (d </> modFileName)
      case eResult of
        Left (offset, msg) -> do
          putStrLn
            $  modFileName
            ++ ": Error at offset "
            ++ show offset
            ++ ": "
            ++ msg
          return emptyModFile
        Right modFile -> do
          putStrLn $ modFileName ++ ": successfully parsed precompiled file."
          return modFile
    return $ addedModFiles ++ modFiles
  )
  emptyModFiles
