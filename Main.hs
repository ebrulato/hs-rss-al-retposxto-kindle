{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Codec.MIME.Parse
import qualified Codec.MIME.QuotedPrintable         as M (decode)
import           Control.Exception
import           Control.Lens
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Char8              as C (pack)
import qualified Data.ByteString.Lazy               as BL (toChunks)
import           Data.List
import qualified Data.Text                          as T
import           Data.Text.Encoding                 (decodeASCII, decodeUtf8)
import           Network.HTTP.Client                (HttpException)
import           Network.HTTP.Types.URI             (parseQuery)
import           Network.HaskellNet.POP3
import           Network.HaskellNet.POP3.Connection
import           Network.HaskellNet.POP3.SSL
import           Network.Wreq
import           System.Environment
import           Text.HTML.Parser

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
    _                                           -> return $ Nothing


legiMajlon :: BS.ByteString -> IO (Bool)
legiMajlon datumo = do
  if priKindelo datumo then do
    putStrLn "El Amazon pri Kindelo"
    --putStrLn $ ligilo datumo
    --r <- get $ ligilo datumo
    --putStrLn $ enLigilo datumo
    r <- get $ enLigilo datumo
    if r ^. responseStatus . statusCode == 200 then do
      --putStrLn $ show $ majlKorpo r
      --putStrLn $ show $ seMajloKonfirmitas r
      return $ seMajloKonfirmitas r
    else do
      putStrLn $ "La retpaĝo ne legeblas tial la responsa kodo : " ++ (show $ r ^. responseStatus . statusCode)
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
        _             -> False
    korektiLigilojn token =
      case token of
        (TagOpen "a" (x:xs)) ->
          case x of
            Attr "href" valoro -> valoro
            _                  -> korektiLigilojn (TagOpen "a" xs)
        (TagOpen "a" []) -> "eraro" -- eraro
    ligilo datumo =
      case ligiloj datumo of
        (x:xs) -> T.unpack x
    enLigilo datumo =
      case lookup "U" $ parseQuery $ C.pack $ ligilo datumo of
        Just (Just v) -> T.unpack $ T.replace "=" "" $ decodeUtf8 v
        _             -> "eraro"
    majlKorpo r = decodeUtf8 $ BS.concat . BL.toChunks $ r ^. responseBody
    seMajloKonfirmitas r =
            ("m.media-amazon.com/images/G/01/kindledocs/fail-x" `isSubsequenceOf` (T.unpack $ majlKorpo r :: [Char]))
            || ("m.media-amazon.com/images/G/01/kindledocs/verified-check" `isSubsequenceOf` (T.unpack $ majlKorpo r :: [Char]))


legiMajlojn :: POP3Connection -> Int -> IO ()
legiMajlojn con 0 = return ()
legiMajlojn con kie = do
  (kvanto, dimensio) <- stat con
  if kie > kvanto then do
    putStrLn $ "Ne ekzistas " ++ (show kie) ++ "-e majlo…"
    return ()
  else do
    majlo <- retr con kie
    rezulto <- try (legiMajlon majlo)
    case (rezulto :: (Either HttpException Bool))  of
       Left eraro -> putStrLn $ "Ignoras " ++ (show kie) ++ "-e majlon (ĉar eraro okazis)."
       Right malaperigotus -> do
         if malaperigotus then do
           dele con kie
           putStrLn $ "Malaperigis " ++ (show kie) ++ "-e majlon."
         else
           putStrLn $ "Ignoras " ++ (show kie) ++ "-e majlon."
    legiMajlojn con (kie-1)

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
      legiMajlojn con kvanto
      closePop3 con
      putStrLn "Malkonektita"

    Nothing -> do
      putStrLn $ "Vi devu krei mediajn parametrojn "++ retposxtaServo ++", "++ retposxtaSalutnomo  ++" kaj "++ retposxtaPasvorto  ++"."
