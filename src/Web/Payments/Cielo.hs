-- void setHttpClient(HttpClient httpClient) {
-- Sale createSale(Sale sale) throws IOException, CieloRequestException {
-- Sale querySale(String paymentId) throws IOException, CieloRequestException {
-- Sale cancelSale(String paymentId, Integer amount) throws IOException, CieloRequestException {
-- Sale cancelSale(String paymentId) throws IOException, CieloRequestException {
-- Sale captureSale(String paymentId, Integer amount, Integer serviceTaxAmount)
-- Sale captureSale(String paymentId, Integer amount) throws IOException, CieloRequestException {
-- Sale captureSale(String paymentId) throws IOException, CieloRequestException {

-- request.addHeader("Accept", "application/json");
-- request.addHeader("Accept-Encoding", "gzip");
-- request.addHeader("Content-Type", "application/json");
-- request.addHeader("User-Agent", "CieloEcommerce/3.0 Android SDK");
-- request.addHeader("MerchantId", merchant.getId());
-- request.addHeader("MerchantKey", merchant.getKey());
-- request.addHeader("RequestId", UUID.randomUUID().toString());
module Web.Payments.Cielo where

import Data.Monoid ((<>))
import Data.UUID
import Data.UUID.V4
import Web.Payments.Cielo.Types
import Control.Monad.Reader
import Control.Monad.IO.Class
import           Data.Text  ( pack, Text )
import System.Environment
import Network.Wreq
import Control.Lens
import Data.Aeson
import Data.Convertible

data CieloConfig = CieloConfig { cieloConfigMerchant :: Merchant
                               , cieloConfigEnvironment :: Environment
                               }

type CieloM m = ( MonadIO m
                , MonadReader CieloConfig m
                )

createSale :: CieloM m => Sale -> m Sale
createSale sale = do
    CieloConfig{..} <- ask
    uuid <- liftIO $ toASCIIBytes <$> nextRandom
    let Merchant{..} = cieloConfigMerchant
        Environment{..} = cieloConfigEnvironment
        options = defaults & headers .~ [ ("Accept", "application/json")
                                        -- , ("Accept-Encoding", "gzip")
                                        -- , ("User-Agent", "Haskell Cielo")
                                        , ("MerchantId", convert merchantId)
                                        , ("MerchantKey", convert merchantKey)
                                        , ("RequestId", uuid)
                                        ]
    liftIO $ do
        print (options)
        print (toJSON sale)
    res <- liftIO $ postWith
           options
           (convert environmentApiUrl <> "1/sales") (toJSON sale) >>= asJSON
    return (res ^. responseBody)

querySale :: CieloM m => Text -> m ()
querySale =
    undefined -- GET /:apiurl/1/sales/:id

updateSale :: CieloM m => Text -> Text -> m ()
updateSale =
    undefined -- PUT /:apiurl/1/sales/:id

runCielo :: CieloConfig -> ReaderT CieloConfig IO a -> IO a
runCielo cnf rdr = runReaderT rdr cnf

cieloConfigFromEnv = do
    m <- Merchant <$> (pack <$> getEnv "CIELO_MERCHANTID")
                  <*> (pack <$> getEnv "CIELO_MERCHANTKEY")
    return $ CieloConfig m sandboxEnv
