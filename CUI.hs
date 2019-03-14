----------------------------------------------------------------------------------------------------
---- �������������� ������������ � ���� ���������� ��������� (CUI - Console User Interface).  ------
----------------------------------------------------------------------------------------------------
module CUI where

import Prelude hiding (catch)
import Control.Monad
import Control.Concurrent.MVar
import Data.Char
import Data.IORef
import Foreign
import Foreign.C
import System.IO
#ifdef FREEARC_UNIX
import System.Posix.IO
import System.Posix.Terminal
#endif

import Utils
import Errors
import Files


-- |������������� ������� � UI
syncUI = withMVar mvarSyncUI . const;  mvarSyncUI = unsafePerformIO$ newMVar "mvarSyncUI"

----------------------------------------------------------------------------------------------------
---- ������� � ������������ ("������������ ����?" � �.�.) ------------------------------------------
----------------------------------------------------------------------------------------------------

-- |������, ���������� �� ����� ����� ������������ ��� ���������� ����� �� ������ � ����
askHelp = unlines [ "  Valid answers are:"
                  , "    y - yes"
                  , "    n - no"
                  , "    a - always, answer yes to all remaining queries"
                  , "    s - skip, answer no to all remaining queries"
                  , "    u - update remaining files (yes for each extracted file that is newer than file on disk)"
                  , "    q - quit program"
                  ]

valid_answers = "y/n/a/s/u/q"

-- |����� �������� ��� ������ �������� � ������������
ask question ref_answer answer_on_u =  do
  syncUI $ do
  old_answer <- readIORef ref_answer
  new_answer <- case old_answer of
                  "a" -> return old_answer
                  "u" -> return old_answer
                  "s" -> return old_answer
                  _   -> do let go = do putStr$ "\n  "++question++" ("++valid_answers++")? "
                                        hFlush stdout
                                        answer  <-  getLine >>== strLower
                                        when (answer=="q") $ do
                                            registerError TERMINATED
                                        if (answer `elem` (split '/' valid_answers))
                                          then return answer
                                          else putStr askHelp >> go
                            --
                            go
  writeIORef ref_answer new_answer
  case new_answer of
    "u" -> return answer_on_u
    _   -> return (new_answer `elem` ["y","a"])

-- |������ � ���������� �����
askOverwrite filename  =  ask ("Overwrite " ++ str2terminal filename)


----------------------------------------------------------------------------------------------------
---- ������ ������� --------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

ask_passwords = (ask_encryption_password, ask_decryption_password)

-- |������ ������ ��� ������. ������������ ��������� ����
-- � ������ ����������� ������ ��� ���������� ������ ��� ��� �����
ask_encryption_password =
  syncUI $ do
  withoutEcho $ go where
    go = do putStr "\n  Enter encryption password:"
            hFlush stdout
            answer <- getHiddenLine
            putStr "  Reenter encryption password:"
            hFlush stdout
            answer2 <- getHiddenLine
            if answer/=answer2
              then do putStrLn "  Passwords are different. You need to repeat input"
                      go
              else return answer

-- |������ ������ ��� ����������. ������������ ��������� ����
ask_decryption_password = do
  syncUI $ do
  withoutEcho $ do
  putStr "\n  Enter decryption password:"
  hFlush stdout
  getHiddenLine

-- |������ ������, �� ��������� � �� ������
getHiddenLine = go ""
  where go s = do c <- getHiddenChar
                  case c of
                    '\r' -> do putStrLn ""; return s
                    '\n' -> do putStrLn ""; return s
                    c    -> go (s++[c])


#ifdef FREEARC_WIN

-- |��������� ������� � ����� ���������� �����
withoutEcho = id
-- |������ ������ ��� ���
getHiddenChar = liftM (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
   c_getch :: IO CChar

#else

getHiddenChar = getChar

withoutEcho action = do
  let setAttr attr = setTerminalAttributes stdInput attr Immediately
      disableEcho = do origAttr <- getTerminalAttributes stdInput
                       setAttr$ origAttr.$ flip withMode ProcessInput
                                        .$ flip withoutMode EnableEcho
                                        .$ flip withMode KeyboardInterrupts
                                        .$ flip withoutMode IgnoreBreak
                                        .$ flip withMode InterruptOnBreak
                       return origAttr
  --
  bracketCtrlBreak disableEcho setAttr (\_ -> action)

#endif


----------------------------------------------------------------------------------------------------
---- ����/����� ������������ � ������  -------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |������� �� ����� ����������� � ������
uiPrintArcComment arcComment = do
  when (arcComment>"") $ do
    putStrLn arcComment

-- |������ � stdin ����������� � ������
uiInputArcComment = do
  putStrLn "Enter archive comment, ending with \".\" on separate line:"
  hFlush stdout
  let go xs = do line <- getLine
                 if line/="."
                   then go (line:xs)
                   else return$ joinWith "\n" $ reverse xs
  --
  go []


----------------------------------------------------------------------------------------------------
----- External functions ---------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Set console title
setConsoleTitle title = do
  withCString title c_SetConsoleTitle

-- |Set console title (external)
foreign import ccall unsafe "Environment.h EnvSetConsoleTitle"
  c_SetConsoleTitle :: Ptr CChar -> IO ()

-- |Reset console title
foreign import ccall unsafe "Environment.h EnvResetConsoleTitle"
  resetConsoleTitle :: IO ()


{-
  ��������� UI:
  - ���� �������, ���������� ���������� �� ��������/���������� � ������������ ���������
      �������������� � UI:
        ui_PROCESS pipe = do
          (StartCommand cmd) <- receiveP pipe
            (StartArchive cmd) <- receiveP pipe
              (StartFile fi fi) <- receiveP pipe
                (UnpackedData n) <- receiveP pipe
                (CompressedData n) <- receiveP pipe
            (EndArchive) <- receiveP pipe
          (EndCommand) <- receiveP pipe
         (EndProgram) <- receiveP pipe
      ���� ������� ���������� ������� ��������� UI � SampleVar
-}
