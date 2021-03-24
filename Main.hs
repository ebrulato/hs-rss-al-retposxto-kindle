{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Environment
import           Control.Lens
import           Network.HaskellNet.POP3
import           Network.HaskellNet.POP3.Connection
import           Network.HaskellNet.POP3.SSL
import           Network.Wreq
import qualified Data.ByteString.Lazy as BL (toChunks)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C (pack)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, decodeASCII)
import           Text.HTML.Parser
import qualified Codec.MIME.QuotedPrintable as M (decode)
import           Codec.MIME.Parse

retposxtaServo = "POP_SERVO"
retposxtaSalutnomo = "POP_SALUTNOMO"
retposxtaPasvorto = "POP_PASVORTO"

type Servo = String
type Salutnomo = String
type Pasvorto = String

sercxiRetposxto :: IO (Maybe (Servo, Salutnomo, Pasvorto))
sercxiRetposxto = do
  mbRetposxtaServo <- lookupEnv retposxtaServo
  mbRetposxtaSalutnomo <- lookupEnv retposxtaSalutnomo
  mbRetposxtaPasvorto <- lookupEnv retposxtaPasvorto
  case (mbRetposxtaServo, mbRetposxtaSalutnomo, mbRetposxtaPasvorto) of 
    (Just servo, Just salutnomo, Just pasvorto) -> return $ Just $ (servo, salutnomo, pasvorto)
    _ -> return $ Nothing


legiMajlon :: POP3Connection -> Int -> IO (Bool)
legiMajlon con kie = do
  datumo <- retr con kie
  --putStrLn $ show $ C.pack $ M.decode $ parto datumo
  if priKindelo datumo then do
    putStrLn "El Amazon pri Kindelo"
    putStrLn $ ligilo datumo
    r <- get $ ligilo datumo
    if r ^. responseStatus . statusCode == 200 then do
      --putStrLn $ show $ decodeUtf8 $ BS.concat . BL.toChunks $ r ^. responseBody
      return False
    else do
      putStrLn $ "La retpagxo ne legeblas kial la responsa kodo : " ++ (show $ r ^. responseStatus . statusCode)
      return False
  else do
    putStrLn $ "Ignoras majlon el " ++ (T.unpack $ from datumo)
    return False
  where
    majlo datumo = T.splitOn "\n" $ decodeUtf8 datumo 
    from datumo = T.drop 6 $ head $ filter ("From:" `T.isPrefixOf`) $ majlo datumo
    parto datumo = T.unpack $ T.concat $ drop 3 $ snd $ break ("------=_Part_" `T.isPrefixOf`) $ majlo datumo
    priKindelo datumo = (from datumo) == "Support Amazon Kindle <do-not-reply@amazon.com>"
    ligiloj datumo = map korektiLigilojn $ filter sercxiLigilojn (parseTokens $ T.pack $ M.decode $ parto datumo)
    sercxiLigilojn token =
      case token of
        TagOpen "a" _ -> True
        _ -> False
    korektiLigilojn token =
      case token of
        (TagOpen "a" (x:xs)) ->
          case x of
            Attr "href" valoro -> valoro
            _ -> korektiLigilojn (TagOpen "a" xs)
        (TagOpen "a" []) -> "eraro" -- eraro
    ligilo datumo =
      case ligiloj datumo of
        (x:xs) -> T.unpack x

main :: IO ()
main = do
  mbRetposxto <- sercxiRetposxto
  case mbRetposxto of
    Just (servo, salutnomo, pasvorto) -> do
      putStrLn $ salutnomo ++ " / " ++ servo
      con <- connectPop3SSL servo
      putStrLn "Konektita"
      userPass con salutnomo pasvorto
      putStrLn "Ensalutita"
      (kvanto, dimensio) <- stat con
      putStrLn $ (show kvanto) ++ " mesaĝo(j)"
      legiMajlon con 1
      closePop3 con
      putStrLn "Malkonektita"
      
    Nothing -> do
      putStrLn $ "Vi devu krei mediajn parametrojn "++ retposxtaServo ++", "++ retposxtaSalutnomo  ++" kaj "++ retposxtaPasvorto  ++"."
