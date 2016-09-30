module Web.Payments.Cielo where

import           Control.Exception
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy     as BL
import           Data.Convertible
import           Data.Monoid              ((<>))
import           Data.Text                (Text, pack)
import           Data.UUID
import           Data.UUID.V4
import           Network.HTTP.Client      (HttpException (..))
import           Network.HTTP.Types       (Status (..))
import           Network.Wreq             hiding (get, post, put)
import           System.Environment

import           Web.Payments.Cielo.Types
import           Web.Payments.Cielo.Util

getMerchantOrderId :: MonadIO m => m Text
getMerchantOrderId = liftIO $ toText <$> nextRandom

createSale :: MonadCielo m => Sale -> m Sale
createSale = post "/1/sales"

querySale :: MonadCielo m => Text -> m Sale
querySale paymentId = get ("/1/sales/" <> convert paymentId) []

updateSale :: MonadCielo m => Text -> Text -> m ()
updateSale paymentId paymentType =
    put ("/1/sales/" <> convert paymentId <> "/" <> convert paymentType) ()

runCielo :: CieloConfig -> CieloM a -> IO a
runCielo cnf rdr = do
    eret <- runExceptT (runReaderT rdr cnf)
    case eret of
        Left err -> throwIO err
        Right ret -> return ret

cieloConfigFromEnv :: IO CieloConfig
cieloConfigFromEnv = do
    m <- Merchant <$> (pack <$> getEnv "CIELO_MERCHANTID")
                  <*> (pack <$> getEnv "CIELO_MERCHANTKEY")
    return $ CieloConfig m sandboxEnv
