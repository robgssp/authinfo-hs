{-# LANGUAGE OverloadedStrings #-}
import System.Authinfo

main = getPassword "gmail.com" "ablabla" >>= print
