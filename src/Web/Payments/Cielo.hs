module Web.Payments.Cielo where

import           Control.Exception
import           Control.Lens
import           Control.Monad
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
createSale = post "/1/sales" []

querySale :: MonadCielo m => Text -> m Sale
querySale paymentId = get ("/1/sales/" <> convert paymentId) []

querySalesByMerchantOrderId :: MonadCielo m => Text -> m SalesByMerchantOrderQuery
querySalesByMerchantOrderId merchantOrderId = get "/1/sales" [("merchantOrderId", merchantOrderId)]

queryRecurrentSale :: MonadCielo m => Text -> m RecurrentPaymentQuery
queryRecurrentSale recurrentPaymentId = get ("/1/RecurrentPayment/" <> convert recurrentPaymentId) []

captureSale :: MonadCielo m => Text -> Maybe Int -> Maybe Int -> m SaleUpdate
captureSale paymentId mamount mserviceTaxAmount =
    updateSale paymentId "capture" query
  where
    query =
        maybe [] (\a -> [("amount", convert (show a))]) mamount ++
        maybe [] (\a -> [("serviceTaxAmount", convert (show a))]) mserviceTaxAmount

voidSale :: MonadCielo m => Text -> Maybe Int -> m SaleUpdate
voidSale paymentId mamount = updateSale paymentId "void" query
  where
    query = maybe [] (\a -> [("amount", convert (show a))]) mamount

updateSale :: MonadCielo m => Text -> Text -> [(Text, Text)] -> m SaleUpdate
updateSale paymentId paymentType query = put
    ("/1/sales/" <> convert paymentId <> "/" <> convert paymentType)
    query
    (object [])

cancelRecurrentPayment :: MonadCielo m => Text -> m ()
cancelRecurrentPayment recurrentPaymentId = void $ sendRaw putWith
    ("/1/RecurrentPayment/" <> convert recurrentPaymentId <> "/Deactivate")
    []
    (object [])

updateRecurrentPaymentEndDate :: MonadCielo m => Text -> Text -> m ()
updateRecurrentPaymentEndDate recurrentPaymentId endDate = void $ sendRaw putWith
    ("/1/RecurrentPayment/" <> convert recurrentPaymentId <> "/EndDate")
    []
    endDate

updateRecurrentPaymentInstallments :: MonadCielo m => Text -> Int -> m ()
updateRecurrentPaymentInstallments recurrentPaymentId installments = void $ sendRaw putWith
    ("/1/RecurrentPayment/" <> convert recurrentPaymentId <> "/Installments")
    []
    installments

updateRecurrentPaymentInterval :: MonadCielo m => Text -> Int -> m ()
updateRecurrentPaymentInterval recurrentPaymentId interval = void $ sendRaw putWith
    ("/1/RecurrentPayment/" <> convert recurrentPaymentId <> "/Interval")
    []
    interval

updateRecurrentPaymentRecurrencyDay :: MonadCielo m => Text -> Int -> m ()
updateRecurrentPaymentRecurrencyDay recurrentPaymentId dayOfMonth = void $ sendRaw putWith
    ("/1/RecurrentPayment/" <> convert recurrentPaymentId <> "/Amount")
    []
    dayOfMonth

updateRecurrentPaymentNextPaymentAmount :: MonadCielo m => Text -> Int -> m ()
updateRecurrentPaymentNextPaymentAmount recurrentPaymentId amount = void $ sendRaw putWith
    ("/1/RecurrentPayment/" <> convert recurrentPaymentId <> "/Amount")
    []
    amount

updateRecurrentPaymentNextPaymentDate :: MonadCielo m => Text -> Text -> m ()
updateRecurrentPaymentNextPaymentDate recurrentPaymentId date = void $ sendRaw putWith
    ("/1/RecurrentPayment/" <> convert recurrentPaymentId <> "/NextPaymentDate")
    []
    date

updateRecurrentPaymentPayment :: MonadCielo m => Text -> Payment -> m ()
updateRecurrentPaymentPayment recurrentPaymentId payment = void $ sendRaw putWith
    ("/1/RecurrentPayment/" <> convert recurrentPaymentId <> "/Payment")
    []
    payment

uncancelRecurrentPayment :: MonadCielo m => Text -> m ()
uncancelRecurrentPayment recurrentPaymentId = void $ sendRaw putWith
    ("/1/RecurrentPayment/" <> convert recurrentPaymentId <> "/Reactivate")
    []
    (object [])

updateRecurrentPaymentCustomer :: MonadCielo m => Text -> Customer -> m ()
updateRecurrentPaymentCustomer recurrentPaymentId = void . sendRaw putWith
    ("/1/RecurrentPayment/" <> convert recurrentPaymentId <> "/Customer")
    []

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
