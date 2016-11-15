module Web.Payments.Cielo
    (
      -- * Running the API calls
      runCielo
    , cieloConfigFromEnv
    , productionEnv
    , sandboxEnv
      -- * Available API Calls
      -- ** Generating Merchant Order IDs
    , getMerchantOrderId
      -- ** Sales
    , createSale
    , querySale
    , captureSale
    , voidSale

      -- *** Low-Level
    , updateSale

      -- ** Querying Sales
    , querySalesByMerchantOrderId
    , queryRecurrentSale

      -- ** Recurrent Sales
      -- *** Cancelling / Uncancelling
    , cancelRecurrentPayment
    , uncancelRecurrentPayment

      -- *** Updates
    , updateRecurrentPaymentEndDate
    , updateRecurrentPaymentPayment
    , updateRecurrentPaymentInterval
    , updateRecurrentPaymentInstallments
    , updateRecurrentPaymentCustomer
    , updateRecurrentPaymentRecurrencyDay
    , updateRecurrentPaymentNextPaymentDate
    , updateRecurrentPaymentNextPaymentAmount

      -- * Types
    , CieloConfig(..)
    , CreditCard(..)
    , MonadCielo
    , CieloError(..)
    , Merchant(..)
    , Environment(..)
    , Sale(..)
    , PaymentProvider(..)
    , PaymentType(..)
    , Currency(..)
    , Interval(..)
    , RecurrentPayment(..)
    , RecurrentPaymentQuery(..)
    , Address(..)
    , Customer(..)
    , Payment(..)
    , SalesByMerchantOrderQuery(..)

      -- * Re-exports
    , Default(..)
    , MonadIO
    , liftIO
    , FromJSON(..)
    , ToJSON(..)
    )
  where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Convertible
import           Data.Default
import           Data.Monoid              ((<>))
import           Data.Text                (Text, pack)
import           Data.UUID
import           Data.UUID.V4
import           Network.Wreq             hiding (get, post, put)
import           System.Environment

import           Web.Payments.Cielo.Types
import           Web.Payments.Cielo.Util

-- | API Calls happen in a 'MonadCielo' type-class
-- 'CieloM' is a helper instance of this class, which runs exceptions on the IO
-- monad and exposes the configuration through a 'ReaderT'
runCielo :: CieloConfig -> CieloM a -> IO a
runCielo cnf rdr = do
    eret <- runExceptT (runReaderT rdr cnf)
    case eret of
        Left err -> throwIO err
        Right ret -> return ret

-- | We can load 'CieloConfig' from the @"CIELO_MERCHANTID"@ and
-- @"CIELO_MERCHANTKEY"@ environment variables
cieloConfigFromEnv :: IO CieloConfig
cieloConfigFromEnv = do
    m <- Merchant <$> (pack <$> getEnv "CIELO_MERCHANTID")
                  <*> (pack <$> getEnv "CIELO_MERCHANTKEY")
    return $ CieloConfig m sandboxEnv

-- | Generates a new merchant UIID
getMerchantOrderId :: MonadIO m => m Text
getMerchantOrderId = liftIO $ toText <$> nextRandom

-- | Creates a 'Sale'
createSale :: MonadCielo m => Sale -> m Sale
createSale = post "/1/sales" []

-- | Queries for a 'Sale' given it's paymentId
querySale :: MonadCielo m => Text -> m Sale
querySale paymentId = get ("/1/sales/" <> convert paymentId) []

-- | Queries for a sales given a merchant UUID
querySalesByMerchantOrderId :: MonadCielo m => Text -> m SalesByMerchantOrderQuery
querySalesByMerchantOrderId merchantOrderId = get "/1/sales" [("merchantOrderId", merchantOrderId)]

-- | Queries for a recurrent sale given it's paymentId
queryRecurrentSale :: MonadCielo m => Text -> m RecurrentPaymentQuery
queryRecurrentSale recurrentPaymentId = get ("/1/RecurrentPayment/" <> convert recurrentPaymentId) []

-- | Captures a sale
captureSale
    :: MonadCielo m
    => Text
    -- ^ Payment ID
    -> Maybe Int
    -- ^ Amount
    -> Maybe Int
    -- ^ Service Tax Amount
    -> m SaleUpdate
captureSale paymentId mamount mserviceTaxAmount =
    updateSale paymentId "capture" query
  where
    query =
        maybe [] (\a -> [("amount", convert (show a))]) mamount ++
        maybe [] (\a -> [("serviceTaxAmount", convert (show a))]) mserviceTaxAmount

-- | Voids a sale
voidSale
    :: MonadCielo m
    => Text
    -- ^ Payment ID
    -> Maybe Int
    -- ^ Amount
    -> m SaleUpdate
voidSale paymentId mamount = updateSale paymentId "void" query
  where
    query = maybe [] (\a -> [("amount", convert (show a))]) mamount

-- | Updates a sale (Generalized PUT over a type of Sale update @/1/sales/:id/:type@)
updateSale
    :: MonadCielo m
    => Text
    -- ^ Payment ID
    -> Text
    -- ^ :type
    -> [(Text, Text)]
    -- ^ Data to PUT
    -> m SaleUpdate
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
