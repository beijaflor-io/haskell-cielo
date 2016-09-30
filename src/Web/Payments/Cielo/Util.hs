module Web.Payments.Cielo.Util where

import           Control.Exception
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy     as BL
import           Data.Convertible
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import           Data.UUID
import           Data.UUID.V4
import           Network.HTTP.Client      (HttpException (..))
import           Network.HTTP.Types       (Status (..))
import           Network.Wreq             hiding (get, post, put)

import           Web.Payments.Cielo.Types

cieloRequestOptions :: MonadCielo m => m Options
cieloRequestOptions = do
    CieloConfig{..} <- ask
    let Merchant{..} = cieloConfigMerchant
        Environment{..} = cieloConfigEnvironment

    uuid <- liftIO $ toASCIIBytes <$> nextRandom
    return $ defaults & headers .~ [ ("Accept", "application/json")
                                    -- , ("Accept-Encoding", "gzip")
                                    -- , ("User-Agent", "Haskell Cielo")
                                   , ("MerchantId", convert merchantId)
                                   , ("MerchantKey", convert merchantKey)
                                   , ("RequestId", uuid)
                                   ]

sendRaw
    :: (MonadCielo m, ToJSON arg)
    => (Options -> String -> Value -> IO (Response BL.ByteString))
    -> String -> arg -> m (Response BL.ByteString)
sendRaw requester url arg = do
    CieloConfig _ Environment{..} <- ask
    opts <- cieloRequestOptions
    let targetUrl = convert environmentApiUrl <> url
        payload = toJSON arg
    eret <- liftIO $ try $ requester opts targetUrl payload
    case eret of
        Left ex@(StatusCodeException status _ _) -> case status of
            Status 404 _ -> throwError (CieloNotFoundError url ex)
            Status 400 _ -> throwError (CieloInvalidError url ex (Just payload))
            _ -> throwError (CieloHttpException url ex (Just payload))
        Left ex -> throwError (CieloHttpException url ex (Just payload))
        Right ret -> return ret

getRaw
    :: (MonadCielo m)
    => String -> [(Text, Text)] -> m (Response BL.ByteString)
getRaw url query = do
    CieloConfig _ Environment{..} <- ask
    opts <- cieloRequestOptions
    let targetUrl = convert environmentApiQueryUrl <> url
        opts' = opts & params .~ query
    eret <- liftIO $ try $ getWith opts' targetUrl
    case eret of
        Left ex@(StatusCodeException status _ _) -> case status of
            Status 404 _ -> throwError (CieloNotFoundError url ex)
            Status 400 _ -> throwError (CieloInvalidError url ex Nothing)
            _ -> throwError (CieloHttpException url ex Nothing)
        Left ex -> throwError (CieloHttpException url ex Nothing)
        Right ret -> return ret

get
  :: (MonadCielo m, FromJSON b)
  => String -> [(Text, Text)] -> m b
get url query = do
    res <- getRaw url query
    case asJSON res of
        Right res' -> return (res' ^. responseBody)
        Left err -> throwError (CieloJSONError url err (res ^. responseBody))

send
    :: (MonadCielo m, ToJSON arg, FromJSON ret)
    => (Options -> String -> Value -> IO (Response BL.ByteString))
    -> String -> arg -> m ret
send requester url arg = do
    res <- sendRaw requester url arg
    case asJSON res of
        Right res' -> return (res' ^. responseBody)
        Left err -> throwError (CieloJSONError url err (res ^. responseBody))

post :: (MonadCielo m, ToJSON arg, FromJSON ret) => String -> arg -> m ret
post = send postWith

put :: (MonadCielo m, ToJSON arg, FromJSON ret) => String -> arg -> m ret
put = send putWith
