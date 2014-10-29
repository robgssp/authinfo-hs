{-# LANGUAGE OverloadedStrings #-}
module System.Authinfo (readAuthinfo, getPassword) where
import System.Environment
import Data.Attoparsec.Text hiding (take)
import Data.List
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad
import Network
import Data.Maybe
import Data.Char

type Line = (T.Text, T.Text, T.Text, Maybe PortNumber)
dropBack :: Int -> [a] -> [a]
dropBack n l = take (length l - n) l

quoted = char '"' *> takeWhile1 (not . (=='"')) <* char '"'
lstring s = lexeme $ string s
lexeme p = skipSpace *> p <* skipSpace
word = takeWhile1 $ not . isSpace
attempt p = fmap Just p <|> return Nothing
-- a possibly-quoted field
field = do p <- peekChar'
           if p == '"'
              then quoted
              else word

-- parse line
line :: Parser Line
line = fmap (\(Just h, Just l, Just p, r) -> (h,l,p,r)) 
            (line' (Nothing,Nothing,Nothing,Nothing))
  where line' s@(h,l,p,r) = 
          msum [ char '\n' >> return s
               , lstring "machine" >> field >>= (\h -> line' (Just h,l,p,r))
               , lstring "login" >> field >>= (\l -> line' (h,Just l,p,r))
               , lstring "password" >> field >>= (\p -> line' (h,l,Just p,r))
               , lstring "port" >> decimal >>= (\r -> line' (h,l,p,Just r))
               ] <?> "line"

file = many line

-- |Parses whole authinfo file
readAuthinfo :: IO [Line]
readAuthinfo = do
  home <- getEnv "HOME"
  Right res <- fmap (parseOnly file) $ T.readFile $ home ++ "/.authinfo"
  return res
  
-- |Gets the given user info out of AuthInfo
getPassword :: T.Text -> T.Text -> IO (Maybe (T.Text, Maybe PortNumber))
getPassword host user =
  fmap (fmap (\(_,_,pass,port) -> (pass,port)) . 
        find (\(h,u,_,_) -> h == host && u == user))
       readAuthinfo
