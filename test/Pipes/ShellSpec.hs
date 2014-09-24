module Pipes.ShellSpec where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char
import           Data.Monoid
import           Pipes
import qualified Pipes.Prelude         as P
import           Pipes.Safe
import           Pipes.Shell
import           System.Exit
import           Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "meta tests" $ do
    it "collectOutput works as expected" $ do
      (err, out) <- collectOutput boring
      out `shouldBe` "1234"
      err `shouldBe` "abcd"

  describe "features" $ do
    it "works with tr" $ do
      (err, out) <- collectOutput trTest
      out `shouldBe` "AAA"
      err `shouldBe` ""

    it "handles stdout *and* stderr" $ do
      (err, out) <- collectOutput catEchoTest
      out `shouldBe` "out\nput\n"
      err `shouldBe` "err\nor\n"

    it "handles env variables" $ do
      (err, out) <- collectOutput envTest
      out `shouldBe` "value\n"
      err `shouldBe` ""

    it "handles exit codes" $ do
      false <- runShell $ falseTest >-> P.drain
      false `shouldBe` Just (ExitFailure 1)
      true <- runShell $ trueTest >-> P.drain
      true `shouldBe` Just ExitSuccess

  describe "robustness" $ do
    it "can handle /usr/share/dict/*" $ do
      wrds <- wordsTest
      wrdsRef <- wordsRef
      wrds `shouldBe` wrdsRef

boring :: Producer (Either BS.ByteString BS.ByteString) (SafeT IO) ()
boring = each [Left $ BSC.pack "ab",
               Left $ BSC.pack "cd",
               Right $ BSC.pack "12",
               Right $ BSC.pack "34"]

-- collect the output of the pruducer as a (String,String)
-- these are not [String], because chunking is not really deterministic
collectOutput ::
  Producer (Either BS.ByteString BS.ByteString) (SafeT IO) r ->
  IO (String, String)
collectOutput = runSafeT . P.fold combine ([],[]) fixUp . void
  where
    combine (err, out) (Left x)  = (x:err, out)
    combine (err, out) (Right x) = (err  , x:out)

    fixUp (err, out) = (concat $ reverse $ map BSC.unpack err,
                        concat $ reverse $ map BSC.unpack out)

yieldForCmd :: Monad m => a -> Producer a m (Maybe b)
yieldForCmd = (Nothing <$) . yield

trTest :: Producer (Either BS.ByteString BS.ByteString) (SafeT IO) (Maybe ExitCode)
trTest = yieldForCmd (BSC.pack "aaa") >?> cmd "tr 'a' 'A'"

falseTest :: Producer (Either BS.ByteString BS.ByteString) (SafeT IO) (Maybe ExitCode)
falseTest = cmd "false"

trueTest :: Producer (Either BS.ByteString BS.ByteString) (SafeT IO) (Maybe ExitCode)
trueTest = cmd "true"

catEchoTest :: Producer (Either BS.ByteString BS.ByteString) (SafeT IO) (Maybe ExitCode)
catEchoTest = yieldForCmd (BSC.pack "out\nput\n") >?>
               cmd ("cat; " <>
                    "echo 'err\nor' > /dev/stderr; sync")

envTest :: Producer (Either BS.ByteString BS.ByteString) (SafeT IO) (Maybe ExitCode)
envTest = cmdEnv env "echo $VARIABLE"
  where
    env = Just [("VARIABLE","value")]

wordsRef :: IO Int
wordsRef = do
  (lines':_) <- runSafeT . P.toListM $ void (cmd' "cat /usr/share/dict/* | wc -l" :: Producer BSC.ByteString (SafeT IO) (Maybe ExitCode))
  return $ read $ BSC.unpack lines'

wordsTest :: IO Int
wordsTest = runShell $ countNewlines $ void (cmd' "cat /usr/share/dict/*" :: Producer (BSC.ByteString) (Effect (SafeT IO)) (Maybe ExitCode))
  where
    -- yay, double fold
    countNewlines = P.fold countInChunk 0 id
    countInChunk soFar chunk = soFar + BS.foldl' countInBS 0 chunk
    countInBS soFar wrd
      | wrd == fromIntegral (ord '\n') = soFar + 1
      | otherwise                      = soFar
