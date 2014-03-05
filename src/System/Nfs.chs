-- System.Nfs
--
-- Copyright (C) 2014 Arne Redlich <arne.redlich@googlemail.com>
--
-- Licensed under the LGPL v2.1 - see the LICENSE file.
--
-- Haskell bindings to libnfs - https://github.com/sahlberg/libnfs

{-# LANGUAGE ForeignFunctionInterface #-}

module System.Nfs ( AccessCallback
                  , AccessMode
                  , BlockCount
                  , BlockSize
                  , ChdirCallback
                  , ChmodCallback
                  , ChownCallback
                  , Context
                  , Dir
                  , Dirent
                  , Event
                  , FType3
                  , FSyncCallback
                  , LinkCallback
                  , LSeekCallback
                  , MkDirCallback
                  , MountCallback
                  , NoDataCallback
                  , OpenDirCallback
                  , OpenFileCallback
                  , ReadCallback
                  , ReadLinkCallback
                  , RenameCallback
                  , RmDirCallback
                  , Stat
                  , StatCallback
                  , StatVFSCallback
                  , SymlinkCallback
                  , TruncateCallback
                  , WriteCallback
                  , accessAsync
                  , accessModeExec
                  , accessModeExists
                  , accessModeRead
                  , accessModeWrite
                  , chdirAsync
                  , chmodAsync
                  , chownAsync
                  , closeDir
                  , closeFh
                  , creatAsync
                  , destroyContext
                  , direntFType3
                  , direntGid
                  , direntInode
                  , direntMode
                  , direntName
                  , direntSize
                  , direntUid
                  , eventErr
                  , eventHup
                  , eventNone
                  , eventNval
                  , eventPri
                  , eventRead
                  , eventWrite
                  , fchmodAsync
                  , fchownAsync
                  , fstatAsync
                  , fsyncAsync
                  , ftruncateAsync
                  , initContext
                  , getCurrentOffset
                  , getCwd
                  , getError
                  , getFd
                  , getReadMax
                  , getWriteMax
                  , linkAsync
                  , lseekAsync
                  , mkDirAsync
                  , mountAsync
                  , openAsync
                  , openDirAsync
                  , preadAsync
                  , pwriteAsync
                  , queueLength
                  , readAsync
                  , readDir
                  , readlinkAsync
                  , renameAsync
                  , rmDirAsync
                  , service
                  , setGid
                  , setUid
                  , setTcpSynCount
                  , statATime
                  , statAsync
                  , statBlkSize
                  , statBlocks
                  , statCTime
                  , statDev
                  , statGid
                  , statIno
                  , statMTime
                  , statNLink
                  , statRDev
                  , statSize
                  , statUid
                  , statvfsAsync
                  , symlinkAsync
                  , truncateAsync
                  , whichEvents
                  , writeAsync
                  ) where

import Control.Monad

import Data.Bits
import qualified Data.ByteString as BS
--import Data.Either
--import Data.Maybe
import Data.Monoid
import Data.Word (Word64)

import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr
import Foreign.Storable

import System.IO (SeekMode)
import System.Posix.IO (OpenMode (..))
import System.Posix.StatVFS
import System.Posix.Types

#include <poll.h>

-- access mode definitions: ${R,W,F,X}_OK
#include <unistd.h>

-- stat and statvfs
#include <sys/stat.h>
#include <sys/statvfs.h>
#include <sys/types.h>
#include <unistd.h>

-- I hope this is the fixed version that includes <sys/time.h>
-- (a deficiency in libnfs.h: it makes use of struct timeval but lacks this
-- include).
-- For some reason including it explicitly here before the libnfs.h is not enough.
#include <nfsc/libnfs.h>

-- No "prefix" for now to avoid confusion.
{# context lib="nfs" #}

-- struct nfs_context - an opaque handle passed around with pointers
{# pointer* nfs_context as Context foreign #}

foreign import ccall "nfsc/libnfs.h &nfs_destroy_context"
  destroy_context :: FunPtr (Ptr () -> IO ())

wrap_context :: Ptr () -> IO Context
wrap_context ctx = newForeignPtr destroy_context ctx

-- init_context :: IO Context
-- init_context = do
--   cptr <- {# call nfs_init_context #}
--   ctx <- newForeignPtr destroy_context cptr
--   return $ Context ctx

{# fun nfs_init_context as initContext {} -> `Context' wrap_context* #}

reset_foreign_ptr :: ForeignPtr a -> IO ()
reset_foreign_ptr ptr = do
  finalizeForeignPtr ptr
  withForeignPtr ptr $ \cptr -> poke (castPtr cptr) nullPtr

destroyContext :: Context -> IO ()
destroyContext = reset_foreign_ptr

maybe_error :: CString -> IO (Maybe String)
maybe_error ptr
  | ptr == nullPtr = return Nothing
  | otherwise = liftM Just $ peekCString ptr

{# fun nfs_get_error as getError { withForeignPtr* `Context' } -> `Maybe String' maybe_error* #}

-- struct nfs_url - an opaque handle (for now, as we could actually introspect
-- it but there's not much point in doing so) passed around with pointers.
-- XXX: struct nfs_url looks pretty unused so we might drop it altogether.
{# pointer *nfs_url as Url foreign #}

foreign import ccall "nfsc/libnfs.h &nfs_destroy_url"
  destroy_url :: FunPtr (Ptr () -> IO ())

wrap_url :: Ptr () -> IO Url
wrap_url curl = newForeignPtr destroy_url curl

{# fun nfs_parse_url_full as parse_url_full { withForeignPtr* `Context'
                                            , withCString* `String'
                                            } -> `Url' wrap_url* #}

{# fun nfs_parse_url_dir as parse_url_dir { withForeignPtr* `Context'
                                          , withCString* `String'
                                          } -> `Url' wrap_url* #}

{# fun nfs_parse_url_incomplete as parse_url_incomplete { withForeignPtr* `Context'
                                                        , withCString* `String'
                                                        } -> `Url' wrap_url* #}

{# fun nfs_get_readmax as getReadMax { withForeignPtr* `Context' } -> `Integer' fromIntegral #}

{# fun nfs_get_writemax as getWriteMax { withForeignPtr* `Context' } -> `Integer' fromIntegral #}

{# fun nfs_set_tcp_syncnt as setTcpSynCount { withForeignPtr* `Context',
                                              fromIntegral `Integer'} -> `()' #}

{# fun nfs_set_uid as setUid { withForeignPtr* `Context',
                               fromIntegral `UserID' } -> `()' #}

{# fun nfs_set_gid as setGid { withForeignPtr* `Context',
                               fromIntegral `GroupID' } -> `()' #}

-- Not provided by <poll.h> but will be handy for our purposes.
#c
#define POLLNONE 0
#endc

{# enum define PollEvent { POLLNONE as PollNone,
                           POLLIN as PollIn,
                           POLLPRI as PollPri,
                           POLLOUT as PollOut,
                           POLLERR as PollErr,
                           POLLHUP as PollHup,
                           POLLNVAL as PollNval } deriving (Eq, Show) #}

-- TODO:
-- * a nicer "show"
-- * push to a higher level API?
newtype Event = Event Int deriving (Eq, Show)

eventNone :: Event
eventNone = Event $ fromEnum PollNone

eventRead :: Event
eventRead = Event $ fromEnum PollIn

eventWrite :: Event
eventWrite = Event $ fromEnum PollOut

eventErr :: Event
eventErr = Event $ fromEnum PollErr

eventHup :: Event
eventHup = Event $ fromEnum PollHup

eventPri :: Event
eventPri = Event $ fromEnum PollPri

eventNval :: Event
eventNval = Event $ fromEnum PollNval

instance Monoid Event where
  mempty = eventNone
  mappend (Event a) (Event b) = Event (a .|. b)

to_event :: CInt -> Event
to_event e = Event $ fromIntegral e

from_event :: Event -> CInt
from_event (Event e) = fromIntegral e

{# fun nfs_which_events as whichEvents { withForeignPtr* `Context' } -> `Event' to_event #}

{# fun nfs_service as service { withForeignPtr* `Context'
                              , from_event `Event'
                              } -> `()' #}

{# fun nfs_get_fd as getFd { withForeignPtr* `Context' } -> `Fd' fromIntegral #}

{# fun nfs_queue_length as queueLength { withForeignPtr* `Context' } -> `Integer' fromIntegral #}

-- For now - while I'm still in exploratory mode - errors will be treated
-- with this (odd?) Either. At a later point exceptions could turn out to be a
-- better idea.
handle_ret_error :: Context -> Integer -> IO (Either String ())
handle_ret_error _ 0 = return $ Right ()
handle_ret_error ctx _ = do
  maybe_err <- getError ctx
  case maybe_err of
    Nothing -> return $ Left "unspecified error"
    Just s -> return $ Left s

-- There's not much point in threading the Context or any private data through
-- libnfs calls as we can use partial application.
type CCallback = CInt -> -- err
                 Ptr () -> -- ctx - we have no need for it
                 Ptr () -> -- data - to be cast
                 Ptr () -> -- private_data - we have no need for it
                 IO ()

foreign import ccall "wrapper"
  wrap_cb :: CCallback -> IO (FunPtr (CCallback))

type DataExtractor a = CInt -> Ptr () -> IO a

handle_cb_error :: CInt -> Ptr () -> DataExtractor a -> IO (Either String a)
handle_cb_error status ptr extract
  | status >= 0 = liftM Right $ extract status ptr
  | otherwise = liftM Left $ peekCString (castPtr ptr)

type Callback a = (Either String a) -> IO ()

to_callback_ptr :: Callback a ->
                   DataExtractor a ->
                   IO (FunPtr CCallback)
to_callback_ptr cb extractor = wrap_cb =<< to_c_callback cb extractor
  where
    to_c_callback :: Callback a ->
                     DataExtractor a ->
                     IO CCallback
    to_c_callback cb extractor = return $ \err _ ptr _ ->
      handle_cb_error err ptr extractor >>= cb

type NoDataCallback = Callback ()
-- type NoDataCallback = Either String () -> IO ()

no_data_callback_to_c :: NoDataCallback -> IO CCallback
no_data_callback_to_c cb = return $ \err _ ptr _ ->
  handle_cb_error err ptr (\_ _ -> return ()) >>= cb

type MountCallback = NoDataCallback

type WrappedAction = FunPtr CCallback -> Ptr () -> IO Integer

wrap_action :: Context ->
               WrappedAction ->
               Callback a ->
               DataExtractor a ->
               IO (Either String ())
wrap_action ctx action cb extract = do
  ccb <- to_callback_ptr cb extract
  handle_ret_error ctx =<< action ccb nullPtr

type ServerAddress = String
type ExportName = String

extract_nothing :: DataExtractor ()
extract_nothing _ _ = return ()

-- C2HS can't go all the way for this one:
-- - I cannot seem to get the marshalling of the callback right
-- - the private_data ptr of the C call won't be used by us
-- - converting the Integer error code to our Either String () requires access
--   to the Context
{# fun nfs_mount_async as mount_async { withForeignPtr* `Context'
                                      , withCString* `ServerAddress'
                                      , withCString* `ExportName'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `Integer' fromIntegral #}

mountAsync :: Context ->
              ServerAddress ->
              ExportName ->
              MountCallback ->
              IO (Either String ())
mountAsync ctx addr export cb =
  wrap_action ctx (mount_async ctx addr export) cb extract_nothing

{# pointer* nfsdir as Dir foreign #}

foreign import ccall "nfsc/libnfs.h &nfs_closedir"
  close_dir :: FunPtr (Ptr () -> Ptr () -> IO ())

closeDir :: Dir -> IO ()
closeDir = reset_foreign_ptr

type OpenDirCallback = Either String Dir -> IO ()

{# fun nfs_opendir_async as opendir_async { withForeignPtr* `Context'
                                          , withCString* `FilePath'
                                          , id `FunPtr CCallback'
                                          , id `Ptr ()' } -> `Integer' fromIntegral #}

openDirAsync :: Context ->
                FilePath ->
                OpenDirCallback ->
                IO (Either String ())
openDirAsync ctx path cb = do
  ccb <- wrap_cb =<< opendir_callback_to_c cb
  handle_ret_error ctx =<< opendir_async ctx path ccb nullPtr
    where
      opendir_callback_to_c :: OpenDirCallback -> IO CCallback
      opendir_callback_to_c cb' = return $ \err _ ptr _ ->
        handle_cb_error err ptr (get_dir ctx) >>= cb'

      get_dir :: Context -> CInt -> Ptr () -> IO Dir
      get_dir ctx' _ ptr = withForeignPtr ctx $ \cctx ->
        newForeignPtrEnv close_dir cctx $ castPtr ptr

-- These ones are only available in linux/nfs3.h which isn't exactly portable.
-- So let's define a C enum here and have c2hs generate accessors / converters.
#c
enum ftype3 {
  NF3None = 0,
  NF3Reg  = 1,
  NF3Dir  = 2,
  NF3Blk  = 3,
  NF3Chr  = 4,
  NF3Link = 5,
  NF3Sock = 6,
  NF3Fifo = 7,
};
#endc

{# enum ftype3 as FType3 {upcaseFirstLetter} deriving (Eq, Show) #}

data TimeVal = TimeVal { tvSec :: EpochTime
                       , tvUSec :: CSUSeconds
                       } deriving (Eq, Show)

#c
typedef struct timeval timeval;
#endc

{# pointer* timeval as TimeValPtr -> TimeVal #}

peek_timeval_ptr :: TimeValPtr -> IO TimeVal
peek_timeval_ptr ptr = do
  sec <- {# get timeval->tv_sec #} ptr
  usec <- {# get timeval->tv_usec #} ptr
  return $ TimeVal { tvSec = fromIntegral sec
                   , tvUSec = fromIntegral usec }

poke_timeval_ptr :: TimeValPtr -> TimeVal -> IO ()
poke_timeval_ptr ptr tv = do
  {# set timeval->tv_sec #} ptr $ to_clong $ tvSec tv
  {# set timeval->tv_usec #} ptr $ to_clong $ tvUSec tv
    where
      -- Why are EpochTime (CTime) and CSUSeconds not instances of the
      -- Integral class? This is ... yucky:
      to_clong :: Real a => a -> CLong
      to_clong = fromIntegral.round.realToFrac

instance Storable TimeVal where
  sizeOf _ = {# sizeof timeval #}
  alignment _ = {# alignof timeval #}
  peek = peek_timeval_ptr
  poke = poke_timeval_ptr

-- TODO:
-- * {a,c,m}time
data Dirent = Dirent { direntName :: String
                     , direntInode :: FileID
                     , direntFType3 :: FType3
                     , direntMode :: FileMode
                     , direntSize :: FileOffset
                     , direntUid :: UserID
                     , direntGid :: GroupID
                     , direntATime :: TimeVal
                     , direntMTime :: TimeVal
                     , direntCTime :: TimeVal
                     } deriving (Eq, Show)

#c
typedef struct nfsdirent nfsdirent;
#endc

{# pointer* nfsdirent as DirentPtr -> Dirent #}

peek_dirent_ptr :: DirentPtr -> IO Dirent
peek_dirent_ptr ptr = do
  name <- peekCString =<< {# get nfsdirent->name #} ptr
  inode <- {# get nfsdirent->inode #} ptr
  typ <- {# get nfsdirent->type #} ptr
  mode <- {# get nfsdirent->mode #} ptr
  size <- {# get nfsdirent->size #} ptr
  uid <- {# get nfsdirent->uid #} ptr
  gid <- {# get nfsdirent->gid #} ptr
  atime <- peek $ ptr `plusPtr` {# offsetof nfsdirent->atime #}
  mtime <- peek $ ptr `plusPtr` {# offsetof nfsdirent->mtime #}
  ctime <- peek $ ptr `plusPtr` {# offsetof nfsdirent->ctime #}
  return Dirent { direntName = name
                , direntInode = fromIntegral inode
                , direntFType3 = toEnum $ fromIntegral typ
                , direntMode = fromIntegral mode
                , direntSize = fromIntegral size
                , direntUid = fromIntegral uid
                , direntGid = fromIntegral gid
                , direntATime = atime
                , direntMTime = mtime
                , direntCTime = ctime }

poke_dirent_ptr :: DirentPtr -> Dirent -> IO ()
poke_dirent_ptr _ _ = fail "We don't write to a DirentPtr. Ever."

instance Storable Dirent where
  sizeOf _ = {# sizeof nfsdirent #}
  alignment _ = {# alignof nfsdirent #}
  peek = peek_dirent_ptr
  poke = poke_dirent_ptr

extract_maybe_dirent :: DirentPtr -> IO (Maybe Dirent)
extract_maybe_dirent ptr
  | ptr == nullPtr = return Nothing
  | otherwise = liftM Just $ peek ptr

{# fun nfs_readdir as readDir { withForeignPtr* `Context'
                              , withForeignPtr* `Dir' } -> `Maybe Dirent' extract_maybe_dirent* #}


{# fun nfs_mkdir_async as mkdir_async { withForeignPtr* `Context'
                                      , withCString* `FilePath'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `Integer' fromIntegral #}

type MkDirCallback = NoDataCallback

mkDirAsync :: Context -> FilePath -> MkDirCallback -> IO (Either String ())
mkDirAsync ctx path cb = do
  ccb <- wrap_cb =<< no_data_callback_to_c cb
  handle_ret_error ctx =<< mkdir_async ctx path ccb nullPtr

{# fun nfs_rmdir_async as rmdir_async { withForeignPtr* `Context'
                                      , withCString* `FilePath'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `Integer' fromIntegral #}

type RmDirCallback = NoDataCallback

rmDirAsync :: Context -> FilePath -> RmDirCallback -> IO (Either String ())
rmDirAsync ctx path cb = do
  ccb <- wrap_cb =<< no_data_callback_to_c cb
  handle_ret_error ctx =<< rmdir_async ctx path ccb nullPtr

type TruncateCallback = NoDataCallback

{#fun nfs_truncate_async as truncate_async { withForeignPtr* `Context'
                                           , withCString* `FilePath'
                                           , fromIntegral `Word64'
                                           , id `FunPtr CCallback'
                                           , id `Ptr ()'} -> `Integer' fromIntegral #}

truncateAsync :: Context ->
                 FilePath ->
                 Word64 ->
                 TruncateCallback ->
                 IO (Either String ())
truncateAsync ctx path len cb = do
  ccb <- wrap_cb =<< no_data_callback_to_c cb
  handle_ret_error ctx =<< truncate_async ctx path len ccb nullPtr

type RenameCallback = NoDataCallback

{# fun nfs_rename_async as rename_async { withForeignPtr* `Context'
                                        , withCString* `FilePath'
                                        , withCString* `FilePath'
                                        , id `FunPtr CCallback'
                                        , id `Ptr ()' } -> `Integer' fromIntegral #}

renameAsync :: Context ->
               FilePath ->
               FilePath ->
               RenameCallback ->
               IO (Either String ())
renameAsync ctx from to cb = do
  ccb <- wrap_cb =<< no_data_callback_to_c cb
  handle_ret_error ctx =<< rename_async ctx from to ccb nullPtr

type SymlinkCallback = NoDataCallback

{# fun nfs_symlink_async as symlink_async { withForeignPtr* `Context'
                                          , withCString* `FilePath'
                                          , withCString* `FilePath'
                                          , id `FunPtr CCallback'
                                          , id `Ptr ()' } -> `Integer' fromIntegral #}

symlinkAsync :: Context ->
                FilePath ->
                FilePath ->
                SymlinkCallback ->
                IO (Either String ())
symlinkAsync ctx from to cb = do
  ccb <- wrap_cb =<< no_data_callback_to_c cb
  handle_ret_error ctx =<< symlink_async ctx from to ccb nullPtr

type LinkCallback = NoDataCallback

{# fun nfs_link_async as link_async { withForeignPtr* `Context'
                                    , withCString* `FilePath'
                                    , withCString* `FilePath'
                                    , id `FunPtr CCallback'
                                    , id `Ptr ()' } -> `Integer' fromIntegral #}

linkAsync :: Context ->
             FilePath ->
             FilePath ->
             LinkCallback ->
             IO (Either String ())
linkAsync ctx from to cb = do
  ccb <- wrap_cb =<< no_data_callback_to_c cb
  handle_ret_error ctx =<< link_async ctx from to ccb nullPtr

{# pointer* nfsfh as Fh foreign #}

-- Even though nfs_close is declared to return an int (< 0 indicating an error),
-- the implementation of it always returns 0 so we can simply ignore the return value
-- and use it as a finalizer.
-- We also won't bother with nfs_close_async as that does nothing more than invoking
-- the callback immediately.
foreign import ccall "&nfs_close"
  close_fh :: FunPtr (Ptr () -> Ptr () -> IO ())

closeFh :: Fh -> IO ()
closeFh = reset_foreign_ptr

type OpenFileCallback = Either String Fh -> IO ()

open_file_callback_to_c :: Context -> OpenFileCallback -> IO CCallback
open_file_callback_to_c ctx cb = return $ \err _ ptr _ ->
  handle_cb_error err ptr (get_fh ctx) >>= cb
  where
    get_fh :: Context -> CInt -> Ptr () -> IO Fh
    get_fh ctx' _ ptr = withForeignPtr ctx' $ \cctx ->
      newForeignPtrEnv close_fh cctx $ castPtr ptr

open_mode_to_cint :: OpenMode -> CInt
open_mode_to_cint ReadOnly = 0
open_mode_to_cint WriteOnly = 1
open_mode_to_cint ReadWrite = 2

{# fun nfs_creat_async as creat_async { withForeignPtr* `Context'
                                      , withCString* `FilePath'
                                      , open_mode_to_cint `OpenMode'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `Integer' fromIntegral #}

creatAsync :: Context ->
              FilePath ->
              OpenMode ->
              OpenFileCallback ->
              IO (Either String ())
creatAsync ctx path mode cb = do
  ccb <- wrap_cb =<< open_file_callback_to_c ctx cb
  handle_ret_error ctx =<< creat_async ctx path mode ccb nullPtr

{# fun nfs_open_async as open_async { withForeignPtr* `Context'
                                    , withCString* `FilePath'
                                    , open_mode_to_cint `OpenMode'
                                    , id `FunPtr CCallback'
                                    , id `Ptr ()' } -> `Integer' fromIntegral #}

openAsync :: Context ->
             FilePath ->
             OpenMode ->
             OpenFileCallback ->
             IO (Either String ())
openAsync ctx path mode cb = do
  ccb <- wrap_cb =<< open_file_callback_to_c ctx cb
  handle_ret_error ctx =<< open_async ctx path mode ccb nullPtr

type WriteCallback = Either String Integer -> IO ()

write_callback_to_c :: WriteCallback -> IO CCallback
write_callback_to_c cb = return $ \err _ ptr _ ->
  handle_cb_error err ptr written_size >>= cb
    where
      written_size :: CInt -> Ptr () -> IO Integer
      written_size size _ = return $ fromIntegral size

-- c2hs won't allow the "BS." prefix. Oh well.
bs_as_cstring :: BS.ByteString -> (CString -> IO a) -> IO a
bs_as_cstring = BS.useAsCString

{# fun nfs_write_async as write_async { withForeignPtr* `Context'
                                      , withForeignPtr* `Fh'
                                      , fromIntegral `Int'
                                      , bs_as_cstring* `BS.ByteString'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `Integer' fromIntegral #}

 -- TODO: use a distinct, unsigned type for data size!?
writeAsync :: Context ->
              Fh ->
              BS.ByteString ->
              WriteCallback ->
              IO (Either String ())
writeAsync ctx fh bs cb = do
  ccb <- wrap_cb =<< write_callback_to_c cb
  handle_ret_error ctx =<< write_async ctx fh (BS.length bs) bs ccb nullPtr

{# fun nfs_pwrite_async as pwrite_async { withForeignPtr* `Context'
                                        , withForeignPtr* `Fh'
                                        , fromIntegral `FileOffset'
                                        , fromIntegral `Int'
                                        , bs_as_cstring* `BS.ByteString'
                                        , id `FunPtr CCallback'
                                        , id `Ptr ()' } -> `Integer' fromIntegral #}

pwriteAsync :: Context ->
               Fh ->
               BS.ByteString ->
               FileOffset ->
               WriteCallback ->
               IO (Either String ())
pwriteAsync ctx fh bs off cb = do
  ccb <- wrap_cb =<< write_callback_to_c cb
  handle_ret_error ctx =<< pwrite_async ctx fh off (BS.length bs) bs ccb nullPtr

{# fun nfs_read_async as read_async { withForeignPtr* `Context'
                                    , withForeignPtr* `Fh'
                                    , fromIntegral `Word64'
                                    , id `FunPtr CCallback'
                                    , id `Ptr ()' } -> `Integer' fromIntegral #}

type ReadCallback = Either String BS.ByteString -> IO ()

read_callback_to_c :: ReadCallback -> IO CCallback
read_callback_to_c cb = return $ \ err _ ptr _ ->
  handle_cb_error err ptr read_data >>= cb
    where
      read_data :: CInt -> Ptr () -> IO BS.ByteString
      read_data len ptr = BS.packCStringLen (castPtr ptr, fromIntegral len)

readAsync :: Context ->
             Fh ->
             Word64 ->
             ReadCallback ->
             IO (Either String ())
readAsync ctx fh size cb = do
  ccb <- wrap_cb =<< read_callback_to_c cb
  handle_ret_error ctx =<< read_async ctx fh size ccb nullPtr

{# fun nfs_pread_async as pread_async { withForeignPtr* `Context'
                                      , withForeignPtr* `Fh'
                                      , fromIntegral `FileOffset'
                                      , fromIntegral `Word64'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `Integer' fromIntegral #}

preadAsync :: Context ->
              Fh ->
              Word64 ->
              FileOffset ->
              ReadCallback ->
              IO (Either String ())
preadAsync ctx fh size off cb = do
  ccb <- wrap_cb =<< read_callback_to_c cb
  handle_ret_error ctx =<< pread_async ctx fh off size ccb nullPtr

type FSyncCallback = NoDataCallback

{# fun nfs_fsync_async as fsync_async { withForeignPtr* `Context'
                                      , withForeignPtr* `Fh'
                                      , id `FunPtr CCallback'
                                      , id `Ptr () ' } -> `Integer' fromIntegral #}

fsyncAsync :: Context ->
              Fh ->
              FSyncCallback ->
              IO (Either String ())
fsyncAsync ctx fh cb = do
  ccb <- wrap_cb =<< no_data_callback_to_c cb
  handle_ret_error ctx =<< fsync_async ctx fh ccb nullPtr

type LSeekCallback = Either String FileOffset -> IO ()

lseek_callback_to_c :: LSeekCallback -> IO CCallback
lseek_callback_to_c cb = return $ \err _ ptr _ ->
  handle_cb_error err ptr position >>= cb
    where
      position :: CInt -> Ptr () -> IO FileOffset
      position _ ptr = peek $ castPtr ptr

{#fun nfs_lseek_async as lseek_async { withForeignPtr* `Context'
                                     , withForeignPtr* `Fh'
                                     , fromIntegral `FileOffset'
                                     , fromIntegral `Int'
                                     , id `FunPtr CCallback'
                                     , id `Ptr ()' } -> `Integer' fromIntegral #}

lseekAsync :: Context ->
              Fh ->
              FileOffset ->
              SeekMode ->
              LSeekCallback ->
              IO (Either String ())
lseekAsync ctx fh off mode cb = do
  ccb <- wrap_cb =<< lseek_callback_to_c cb
  handle_ret_error ctx =<< lseek_async ctx fh off (fromEnum mode) ccb nullPtr

{# fun nfs_ftruncate_async as ftruncate_async { withForeignPtr* `Context'
                                              , withForeignPtr* `Fh'
                                              , fromIntegral `Word64'
                                              , id `FunPtr CCallback'
                                              , id `Ptr ()' } -> `Integer' fromIntegral #}

ftruncateAsync :: Context ->
                  Fh ->
                  Word64 ->
                  TruncateCallback ->
                  IO (Either String ())
ftruncateAsync ctx fh len cb = do
  ccb <- wrap_cb =<< no_data_callback_to_c cb
  handle_ret_error ctx =<< ftruncate_async ctx fh len ccb nullPtr

{# fun nfs_get_current_offset as getCurrentOffset { withForeignPtr* `Fh' } -> `FileOffset' fromIntegral #}

type ChownCallback = NoDataCallback

{# fun nfs_chown_async as chown_async { withForeignPtr* `Context'
                                      , withCString* `FilePath'
                                      , fromIntegral `UserID'
                                      , fromIntegral `GroupID'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `Integer' fromIntegral #}

chownAsync :: Context ->
              FilePath ->
              UserID ->
              GroupID ->
              ChownCallback ->
              IO (Either String ())
chownAsync ctx path uid gid cb = do
  ccb <- wrap_cb =<< no_data_callback_to_c cb
  handle_ret_error ctx =<< chown_async ctx path uid gid ccb nullPtr

{# fun nfs_fchown_async as fchown_async { withForeignPtr* `Context'
                                        , withForeignPtr* `Fh'
                                        , fromIntegral `UserID'
                                        , fromIntegral `GroupID'
                                        , id `FunPtr CCallback'
                                        , id `Ptr ()' } -> `Integer' fromIntegral #}

fchownAsync :: Context ->
               Fh ->
               UserID ->
               GroupID ->
               ChownCallback ->
               IO (Either String ())
fchownAsync ctx fh uid gid cb = do
  ccb <- wrap_cb =<< no_data_callback_to_c cb
  handle_ret_error ctx =<< fchown_async ctx fh uid gid ccb nullPtr

type ChmodCallback = NoDataCallback

{# fun nfs_chmod_async as chmod_async { withForeignPtr* `Context'
                                      , withCString* `FilePath'
                                      , fromIntegral `FileMode'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `Integer' fromIntegral #}

chmodAsync :: Context ->
              FilePath ->
              FileMode ->
              ChmodCallback ->
              IO (Either String ())
chmodAsync ctx path mode cb = do
  ccb <- wrap_cb =<< no_data_callback_to_c cb
  handle_ret_error ctx =<< chmod_async ctx path mode ccb nullPtr

{# fun nfs_fchmod_async as fchmod_async { withForeignPtr* `Context'
                                        , withForeignPtr* `Fh'
                                        , fromIntegral `FileMode'
                                        , id `FunPtr CCallback'
                                        , id `Ptr ()' } -> `Integer' fromIntegral #}

fchmodAsync :: Context ->
               Fh ->
               FileMode ->
               ChmodCallback ->
               IO (Either String ())
fchmodAsync ctx fh mode cb = do
  ccb <- wrap_cb =<< no_data_callback_to_c cb
  handle_ret_error ctx =<< fchmod_async ctx fh mode ccb nullPtr

type ChdirCallback = NoDataCallback

{# fun nfs_chdir_async as chdir_async { withForeignPtr* `Context'
                                      , withCString* `FilePath'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `Integer' fromIntegral #}

chdirAsync :: Context ->
              FilePath ->
              ChdirCallback ->
              IO (Either String ())
chdirAsync ctx path cb = do
  ccb <- wrap_cb =<< no_data_callback_to_c cb
  handle_ret_error ctx =<< chdir_async ctx path ccb nullPtr

{# fun nfs_getcwd as get_cwd { withForeignPtr* `Context'
                             , id `Ptr CString' } -> `()' #}

getCwd :: Context -> IO FilePath
getCwd ctx = alloca $ \ptr -> do
    get_cwd ctx ptr
    peekCString =<< peek ptr

{#enum define AccessMode' { R_OK as ReadOk
                          , W_OK as WriteOk
                          , X_OK as ExecOk
                          , F_OK as ExistsOk } deriving (Eq, Show) #}

newtype AccessMode = AccessMode Int deriving (Eq, Show)

accessModeRead :: AccessMode
accessModeRead = AccessMode $ fromEnum ReadOk

accessModeWrite :: AccessMode
accessModeWrite = AccessMode $ fromEnum WriteOk

accessModeExec :: AccessMode
accessModeExec = AccessMode $ fromEnum ExecOk

accessModeExists :: AccessMode
accessModeExists = AccessMode $ fromEnum ExistsOk

instance Monoid AccessMode where
  mempty = accessModeExists
  mappend (AccessMode a) (AccessMode b) = AccessMode (a .|. b)


type AccessCallback = NoDataCallback

from_access_mode :: AccessMode -> CInt
from_access_mode (AccessMode m) = fromIntegral m

{# fun nfs_access_async as access_async { withForeignPtr* `Context'
                                        , withCString* `FilePath'
                                        , from_access_mode `AccessMode'
                                        , id `FunPtr CCallback'
                                        , id `Ptr ()' } -> `Integer' fromIntegral #}

accessAsync :: Context ->
               FilePath ->
               AccessMode ->
               AccessCallback ->
               IO (Either String ())
accessAsync ctx path mode cb = do
  ccb <- wrap_cb =<< no_data_callback_to_c cb
  handle_ret_error ctx =<< access_async ctx path mode ccb nullPtr

type ReadLinkCallback = Either String FilePath -> IO ()

{# fun nfs_readlink_async as readlink_async { withForeignPtr* `Context'
                                            , withCString* `FilePath'
                                            , id `FunPtr CCallback'
                                            , id `Ptr ()' } -> `Integer' fromIntegral #}

readlinkAsync :: Context ->
                 FilePath ->
                 ReadLinkCallback ->
                 IO (Either String ())
readlinkAsync ctx path cb = do
  ccb <- wrap_cb =<< readlink_callback_to_c cb
  handle_ret_error ctx =<< readlink_async ctx path ccb nullPtr
    where
      readlink_callback_to_c :: ReadLinkCallback -> IO CCallback
      readlink_callback_to_c cb' = return $ \err _ ptr _ ->
        handle_cb_error err ptr linktarget >>= cb'

      linktarget :: CInt -> Ptr () -> IO FilePath
      linktarget _ ptr = peekCString $ castPtr ptr

#c
typedef struct statvfs statvfs;
#endc

{# pointer* statvfs as StatVFSPtr -> StatVFS #}

peek_statvfs_ptr :: StatVFSPtr -> IO StatVFS
peek_statvfs_ptr ptr = do
  bsize <- {# get statvfs->f_bsize #} ptr
  frsize <- {# get statvfs->f_frsize #} ptr
  blocks <- {# get statvfs->f_blocks #} ptr
  bfree <- {# get statvfs->f_bfree #} ptr
  bavail <- {# get statvfs->f_bavail #} ptr
  files <- {# get statvfs->f_files #} ptr
  ffree <- {# get statvfs->f_ffree #} ptr
  favail <- {# get statvfs->f_favail #} ptr
  fsid <- {# get statvfs->f_fsid #} ptr
  flag <- {# get statvfs->f_flag #} ptr
  namemax <- {# get statvfs->f_namemax #} ptr
  return $ StatVFS { statVFS_bsize = bsize
                   , statVFS_frsize = frsize
                   , statVFS_blocks = fromIntegral blocks
                   , statVFS_bfree = fromIntegral bfree
                   , statVFS_bavail = fromIntegral bavail
                   , statVFS_files = fromIntegral files
                   , statVFS_ffree = fromIntegral ffree
                   , statVFS_favail = fromIntegral favail
                   , statVFS_fsid = fsid
                   , statVFS_flag = flag
                   , statVFS_namemax = namemax }

poke_statvfs_ptr :: StatVFSPtr -> StatVFS -> IO ()
poke_statvfs_ptr _ _ = fail "We don't write to a StatVFSPtr. Ever."

instance Storable StatVFS where
  sizeOf _ = {# sizeof statvfs #}
  alignment _ = {# alignof statvfs #}
  peek = peek_statvfs_ptr
  poke = poke_statvfs_ptr

type StatVFSCallback = Either String StatVFS -> IO ()

{#fun nfs_statvfs_async as statvfs_async { withForeignPtr* `Context'
                                         , withCString* `FilePath'
                                         , id `FunPtr CCallback'
                                         , id `Ptr ()' } -> `Integer' fromIntegral #}

statvfsAsync :: Context ->
                FilePath ->
                StatVFSCallback ->
                IO (Either String ())
statvfsAsync ctx path cb = do
  ccb <- wrap_cb =<< statvfs_callback_to_c cb
  handle_ret_error ctx =<< statvfs_async ctx path ccb nullPtr
    where
      statvfs_callback_to_c :: StatVFSCallback -> IO CCallback
      statvfs_callback_to_c cb' = return $ \err _ ptr _ ->
        handle_cb_error err ptr get_statvfs >>= cb'

      get_statvfs :: CInt -> Ptr () -> IO StatVFS
      get_statvfs _ ptr = peek $ castPtr ptr

type BlockCount = Word64
type BlockSize = Word64

data Stat = Stat { statDev :: DeviceID
                 , statIno :: FileID
                 , statMode :: FileMode
                 , statNLink :: LinkCount
                 , statUid :: UserID
                 , statGid :: GroupID
                 , statRDev :: DeviceID
                 , statSize :: FileOffset
                 , statBlkSize :: BlockSize
                 , statBlocks :: BlockCount
                 , statATime :: EpochTime
                 , statMTime :: EpochTime
                 , statCTime :: EpochTime
                 } deriving (Eq, Show)

#c
typedef struct stat stat;
#endc

{# pointer* stat as StatPtr -> Stat #}

peek_stat_ptr :: StatPtr -> IO Stat
peek_stat_ptr ptr = do
  dev <- {# get stat->st_dev #} ptr
  ino <- {# get stat->st_ino #} ptr
  mode <- {# get stat->st_mode #} ptr
  nlink <- {# get stat->st_nlink #} ptr
  uid <- {# get stat->st_uid #} ptr
  gid <- {# get stat->st_gid #} ptr
  rdev <- {# get stat->st_rdev #} ptr
  size <- {# get stat->st_size #} ptr
  blksize <- {# get stat->st_blksize #} ptr
  blocks <- {# get stat->st_blocks #} ptr

  -- TODO:
  -- src/System/Nfs.chs:975: (column 25) [ERROR]  >>> Unknown member name!
  -- The structure has no member called `st_atime'.  The structure is defined at
  -- ("/usr/include/x86_64-linux-gnu/bits/stat.h",46,1).

  -- atime <- {# get stat->st_atime #} ptr
  -- mime <- {# get stat->st_mtime #} ptr
  -- ctime <- {# get stat->st_ctime #} ptr
  return $ Stat { statDev = fromIntegral dev
                , statIno = fromIntegral ino
                , statMode = fromIntegral mode
                , statNLink = fromIntegral nlink
                , statUid = fromIntegral uid
                , statGid = fromIntegral gid
                , statRDev = fromIntegral rdev
                , statSize = fromIntegral size
                , statBlkSize = fromIntegral blksize
                , statBlocks = fromIntegral blocks
                , statATime = 0
                , statMTime = 0
                , statCTime = 0 }

poke_stat_ptr :: StatPtr -> Stat -> IO ()
poke_stat_ptr _ _ = fail "We don't write to a StatPtr. Ever."

instance Storable Stat where
  sizeOf _ = {# sizeof stat #}
  alignment _ = {# alignof stat #}
  peek = peek_stat_ptr
  poke = poke_stat_ptr

type StatCallback = Either String Stat -> IO ()

stat_callback_to_c :: StatCallback -> IO CCallback
stat_callback_to_c cb = return $ \err _ ptr _ ->
  handle_cb_error err ptr get_stat >>= cb
    where
      get_stat :: CInt -> Ptr () -> IO Stat
      get_stat _ ptr = peek $ castPtr ptr

{# fun nfs_stat_async as stat_async { withForeignPtr* `Context'
                                    , withCString* `FilePath'
                                    , id `FunPtr CCallback'
                                    , id `Ptr ()' } -> `Integer' fromIntegral #}

statAsync :: Context ->
             FilePath ->
             StatCallback ->
             IO (Either String ())
statAsync ctx path cb = do
  ccb <- wrap_cb =<< stat_callback_to_c cb
  handle_ret_error ctx =<< stat_async ctx path ccb nullPtr

{# fun nfs_fstat_async as stat_fasync { withForeignPtr* `Context'
                                      , withForeignPtr* `Fh'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `Integer' fromIntegral #}

fstatAsync :: Context ->
              Fh ->
              StatCallback ->
              IO (Either String ())
fstatAsync ctx fh cb = do
  ccb <- wrap_cb =<< stat_callback_to_c cb
  handle_ret_error ctx =<< stat_fasync ctx fh ccb nullPtr

data UTimesCallback = NoDataCallback

{# fun nfs_utimes_async as utimes_async { withForeignPtr* `Context'
                                        , withCString* `FilePath'
                                        , id `TimeValPtr'
                                        , id `FunPtr CCallback'
                                        , id `Ptr ()' } -> `Integer' fromIntegral #}

-- Local Variables: **
-- mode: haskell **
-- compile-command: "cd ../.. && cabal install -v" **
-- End: **
