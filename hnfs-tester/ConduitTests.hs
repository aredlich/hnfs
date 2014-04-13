{-|
Module      : ConduitTests
Description : hnfs-tester - Nfs (client library) test tool
Copyright   : (c) 2014 Arne Redlich <arne.redlich@googlemail.com>
License     : LGPL v2.1
Maintainer  : Arne Redlich <arne.redlich@googlemail.com>
Stability   : experimental
Portability : POSIX

hnfs conduit tests.
-}

module ConduitTests ( tests ) where

import Base

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource (runResourceT)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import Data.Conduit
import qualified Data.Conduit.List as ListC
import qualified Data.Conduit.Nfs as NfsC

import System.FilePath.Posix ((</>))
import System.Posix.IO (OpenMode (..))

import Test.Tasty.HUnit as HU

test_directory_source :: ReaderT TestContext IO ()
test_directory_source =
  withDirectory "/" $ \parent ->
    withDirectory' parent $ \child_dir ->
      withFile' parent $ \child_file -> do
        tctx <- ask
        let ctx = ctxContext tctx
        l <- runResourceT $ NfsC.sourceDirectory ctx parent $$ ListC.consume
        liftIO $ HU.assertEqual "unexpected directory contents" 2 $ length l
        liftIO $ HU.assertBool "child file must be present in output" $
          any (\e -> e == parent </> child_file) l
        liftIO $ HU.assertBool "child dir must be present in output" $
          any (\e -> e == parent </> child_dir) l

test_fh_source_and_sink :: ReaderT TestContext IO ()
test_fh_source_and_sink =
  let pattern = BSC8.pack "some arbitrary, utterly unremarkable text"
  in withFile $ \dir source ->
    withFh source ReadWrite $ \fh -> do
      checkPWrite fh 0 pattern
      withFile' dir $ \sink ->
        withFh sink ReadWrite $ \fh2 -> do
          runResourceT $ NfsC.sourceFh fh $$ NfsC.sinkFh fh2
          checkPRead fh2 0 pattern

test_file_path_source_and_sink :: ReaderT TestContext IO ()
test_file_path_source_and_sink =
  let pattern = BSC8.pack "some dull string"
  in withFile $ \dir source ->
    withFh source ReadWrite $ \fh -> do
      checkPWrite fh 0 pattern
      withFile' dir $ \sink -> do
        tctx <- ask
        let ctx = ctxContext tctx
        runResourceT $ NfsC.sourceFile ctx source $$ NfsC.sinkFile ctx sink
        withFh sink ReadWrite $ \fh2 ->
          checkPRead fh2 0 pattern

test_file_path_range_source_and_sink :: ReaderT TestContext IO ()
test_file_path_range_source_and_sink =
  let pattern1 = BSC8.pack "some"
      size1 = BS.length pattern1
      pattern2 = BSC8.pack "pointless"
      size2 = BS.length pattern2
      pattern3 = BSC8.pack "text"
  in withFile $ \dir source ->
    withFh source ReadWrite $ \fh -> do
      checkWrite fh pattern1
      checkWrite fh pattern2
      checkWrite fh pattern3
      withFile' dir $ \sink1 ->
        withFile' dir $ \sink2 ->
          withFile' dir $ \sink3 -> do
            tctx <- ask
            let ctx = ctxContext tctx
            runResourceT $
              NfsC.sourceFileRange ctx source Nothing (Just $ fromIntegral size1) $$
              NfsC.sinkFile ctx sink1
            runResourceT $
              NfsC.sourceFileRange ctx source (Just $ fromIntegral size1)
              (Just $ fromIntegral size2) $$ NfsC.sinkFile ctx sink2
            runResourceT $
              NfsC.sourceFileRange ctx source (Just $ fromIntegral (size1 + size2))
              Nothing $$ NfsC.sinkFile ctx sink3
            withFh sink1 ReadOnly $ \fh' -> checkRead fh' pattern1
            withFh sink2 ReadOnly $ \fh' -> checkRead fh' pattern2
            withFh sink3 ReadOnly $ \fh' -> checkRead fh' pattern3

tests :: [ (ReaderT TestContext IO (), String) ]
tests = [ (test_directory_source, "conduit directory source")
        , (test_fh_source_and_sink, "conduit, fh source and sink")
        , (test_file_path_source_and_sink, "conduit file source and sink")
        , (test_file_path_range_source_and_sink,
           "conduit file range source and sink") ]
