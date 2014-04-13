{-|
Module      : Base
Description : hnfs-tester - Nfs (client library) test tool
Copyright   : (c) 2014 Arne Redlich <arne.redlich@googlemail.com>
License     : LGPL v2.1
Maintainer  : Arne Redlich <arne.redlich@googlemail.com>
Stability   : experimental
Portability : POSIX

Utilities for all hnfs-tester modules.
-}

module Base ( SyncNfs (..)
            , TestContext (..)
            , checkPos
            , checkPRead
            , checkPWrite
            , checkRead
            , checkWrite
            , mount
            , withContext
            , withDirectory
            , withDirectory'
            , withFh
            , withFile
            , withFile'
            , withMount) where

import Control.Exception (bracket)
import qualified Control.Exception.Lifted as LE
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader

import qualified Data.ByteString as BS
import Data.UUID hiding (null)
import Data.UUID.V4

import Foreign.C.Types (CSize)

import qualified System.Nfs as Nfs

import System.FilePath.Posix (joinPath)
import System.IO (SeekMode (..))
import System.Posix.IO (OpenMode (..))
import System.Posix.Types (FileOffset)

import Test.Tasty.HUnit as HU

-- push to Base?
data TestContext = TestContext { ctxSyncNfs :: SyncNfs
                               , ctxContext :: Nfs.Context
                               , ctxServer :: Nfs.ServerAddress
                               , ctxExport :: Nfs.ExportName }

mount :: ReaderT TestContext IO (Either String ())
mount = do
  tctx <- ask
  let ctx = ctxContext tctx
  liftIO $ Nfs.setUid ctx 0
  liftIO $ Nfs.setUid ctx 0
  liftIO $ syncMount (ctxSyncNfs tctx) ctx (ctxServer tctx) (ctxExport tctx)

withMount :: ReaderT TestContext IO () -> ReaderT TestContext IO ()
withMount act = do
  tctx <- ask
  ret <- mount
  case ret of
    Left s -> liftIO $ HU.assertFailure $
              "mounting " ++ ctxServer tctx ++ ":" ++ ctxExport tctx ++
              " failed: " ++ s
    Right () -> act

mk_random_path :: FilePath -> IO FilePath
mk_random_path parent = do
  uuid <- return.toString =<< nextRandom
  return $ joinPath [ parent, uuid ]

withDirectory' :: FilePath ->
                  (FilePath -> ReaderT TestContext IO ()) ->
                  ReaderT TestContext IO ()
withDirectory' parent = LE.bracket mkdir rmdir
  where
    mkdir = do
      tctx <- ask
      let ctx = ctxContext tctx
          nfs = ctxSyncNfs tctx
      path <- liftIO $ mk_random_path parent
      ret <- liftIO $ syncMkDir nfs ctx path
      case ret of
        Left s -> fail $ "failed to create directory: " ++ s
        Right () -> return path
    rmdir path = do
      tctx <- ask
      let ctx = ctxContext tctx
          nfs = ctxSyncNfs tctx
      ret <- liftIO $ syncRmDir nfs ctx path
      case ret of
        -- XXX: does this override the original assertion?
        Left s -> liftIO $ HU.assertFailure $ "failed to remove path: " ++ s
        Right () -> return ()

withDirectory :: FilePath ->
                  (FilePath -> ReaderT TestContext IO ()) ->
                  ReaderT TestContext IO ()
withDirectory parent action = withMount $ withDirectory' parent action

withFile' :: FilePath ->
             (FilePath -> ReaderT TestContext IO ()) ->
             ReaderT TestContext IO ()
withFile' parent = LE.bracket mkfile rmfile
  where
    mkfile = do
      tctx <- ask
      let ctx = ctxContext tctx
          nfs = ctxSyncNfs tctx
      path <- liftIO $ mk_random_path parent
      ret <- liftIO $ syncCreat nfs ctx path WriteOnly
      case ret of
        Left s -> fail $ "failed to create file: " ++ s
        Right fh -> liftIO $ Nfs.closeFh fh >> return path
    rmfile path = do
      tctx <- ask
      let ctx = ctxContext tctx
          nfs = ctxSyncNfs tctx
      ret <- liftIO $ syncUnlink nfs ctx path
      case ret of
        Left s -> fail $ "failed to unlink file " ++ path ++ ": " ++ s
        Right () -> return ()

withFile :: (FilePath -> FilePath -> ReaderT TestContext IO ()) ->
              ReaderT TestContext IO ()
withFile act =
  withDirectory "/" $ \dpath -> withFile' dpath $ \fpath -> act dpath fpath

withContext :: (Nfs.Context -> IO ()) -> IO ()
withContext = bracket Nfs.initContext Nfs.destroyContext

data SyncNfs = SyncNfs { syncMount :: Nfs.Context ->
                                      Nfs.ServerAddress ->
                                      Nfs.ExportName ->
                                      IO (Either String ())
                       , syncOpenDir :: Nfs.Context ->
                                        FilePath ->
                                        IO (Either String Nfs.Dir)
                       , syncMkDir :: Nfs.Context ->
                                      FilePath ->
                                      IO (Either String ())
                       , syncRmDir :: Nfs.Context ->
                                      FilePath ->
                                      IO (Either String ())
                       , syncStat :: Nfs.Context ->
                                     FilePath ->
                                     IO (Either String Nfs.Stat)
                       , syncFStat :: Nfs.Context ->
                                     Nfs.Fh ->
                                     IO (Either String Nfs.Stat)
                       , syncOpen :: Nfs.Context ->
                                     FilePath ->
                                     OpenMode ->
                                     IO (Either String Nfs.Fh)
                       , syncWrite :: Nfs.Context ->
                                      Nfs.Fh ->
                                      BS.ByteString ->
                                      IO (Either String CSize)
                       , syncPWrite :: Nfs.Context ->
                                       Nfs.Fh ->
                                       BS.ByteString ->
                                       FileOffset ->
                                       IO (Either String CSize)
                       , syncRead :: Nfs.Context ->
                                     Nfs.Fh ->
                                     CSize ->
                                     IO (Either String BS.ByteString)
                       , syncPRead :: Nfs.Context ->
                                      Nfs.Fh ->
                                      CSize ->
                                      FileOffset ->
                                      IO (Either String BS.ByteString)
                       , syncCreat :: Nfs.Context ->
                                      FilePath ->
                                      OpenMode ->
                                      IO (Either String Nfs.Fh)
                       , syncUnlink :: Nfs.Context ->
                                       FilePath ->
                                       IO (Either String ())
                       , syncTruncate :: Nfs.Context ->
                                         FilePath ->
                                         FileOffset ->
                                         IO (Either String ())
                       , syncFTruncate :: Nfs.Context ->
                                          Nfs.Fh ->
                                          FileOffset ->
                                          IO (Either String ())
                       , syncLSeek :: Nfs.Context ->
                                      Nfs.Fh ->
                                      FileOffset ->
                                      SeekMode ->
                                      IO (Either String FileOffset)
                       }

withFh :: FilePath ->
           OpenMode ->
           (Nfs.Fh -> ReaderT TestContext IO ()) ->
           ReaderT TestContext IO ()
withFh path mode = LE.bracket open close
  where
    open = do
      tctx <- ask
      let nfs = ctxSyncNfs tctx
          ctx = ctxContext tctx
      ret <- liftIO $ syncOpen nfs ctx path mode
      case ret of
        Left s -> fail $ "failed to open " ++ path ++ ": " ++ s
        Right fh -> return fh
    close = liftIO.Nfs.closeFh

checkPRead :: Nfs.Fh ->
               FileOffset ->
               BS.ByteString ->
               ReaderT TestContext IO ()
checkPRead fh off pattern = do
  tctx <- ask
  let nfs = ctxSyncNfs tctx
      ctx = ctxContext tctx
  pos <- liftIO $ Nfs.getCurrentOffset fh
  rret <- liftIO $ syncPRead nfs ctx fh (fromIntegral $ BS.length pattern) (fromIntegral off)
  case rret of
    Left s -> liftIO $ HU.assertFailure $ "pread failed: " ++ s
    Right bs -> liftIO $ HU.assertEqual "read data should match expectation" pattern bs
  pos' <- liftIO $ Nfs.getCurrentOffset fh
  liftIO $ HU.assertEqual "file position must not have changed" pos pos'

checkRead :: Nfs.Fh ->
              BS.ByteString ->
              ReaderT TestContext IO ()
checkRead fh pattern = do
  tctx <- ask
  let nfs = ctxSyncNfs tctx
      ctx = ctxContext tctx
      size = (fromIntegral $ BS.length pattern)
  pos <- liftIO $ Nfs.getCurrentOffset fh
  rret <- liftIO $ syncRead nfs ctx fh size
  case rret of
    Left s -> liftIO $ HU.assertFailure $ "read failed: " ++ s
    Right bs -> liftIO $
                HU.assertEqual "read data should match expectation" pattern bs
  pos' <- liftIO $ Nfs.getCurrentOffset fh
  liftIO $ HU.assertEqual
    "file position must have changed"
    (pos + fromIntegral size) pos'

checkPWrite :: Nfs.Fh ->
                FileOffset ->
                BS.ByteString ->
                ReaderT TestContext IO ()
checkPWrite fh off pattern = do
  tctx <- ask
  let nfs = ctxSyncNfs tctx
      ctx = ctxContext tctx
  pos <- liftIO $ Nfs.getCurrentOffset fh
  wret <- liftIO $ syncPWrite nfs ctx fh pattern (fromIntegral off)
  case wret of
    Left s -> liftIO $ HU.assertFailure $ "pwrite failed: " ++ s
    Right size -> liftIO $ HU.assertEqual
                  "write size should match expectation"
                  (BS.length pattern) (fromIntegral size)
  pos' <- liftIO $ Nfs.getCurrentOffset fh
  liftIO $ HU.assertEqual "file position must not have changed" pos pos'

checkWrite :: Nfs.Fh ->
               BS.ByteString ->
               ReaderT TestContext IO ()
checkWrite fh pattern = do
  tctx <- ask
  let nfs = ctxSyncNfs tctx
      ctx = ctxContext tctx
      size = (fromIntegral $ BS.length pattern)
  pos <- liftIO $ Nfs.getCurrentOffset fh
  wret <- liftIO $ syncWrite nfs ctx fh pattern
  case wret of
    Left s -> liftIO $ HU.assertFailure $ "write failed: " ++ s
    Right size' -> liftIO $ HU.assertEqual
                  "write size should match expectation"
                  size size'
  pos' <- liftIO $ Nfs.getCurrentOffset fh
  liftIO $ HU.assertEqual
    "file position must have changed"
    (pos + fromIntegral size) pos'

checkPos :: Nfs.Fh -> FileOffset -> String -> ReaderT TestContext IO ()
checkPos fh expected desc = do
  tctx <- ask
  let nfs = ctxSyncNfs tctx
      ctx = ctxContext tctx
  sret <- liftIO $ syncLSeek nfs ctx fh 0 RelativeSeek
  case sret of
    Left s -> liftIO $ HU.assertFailure $
              desc ++ ": failed to lseek to relative pos 0: " ++ s
    Right pos -> liftIO $ HU.assertEqual
                 (desc ++ ": expected file position " ++ show expected)
                 expected pos
