{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Payments.Cielo.Types where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson                          (Value (..))
import qualified Data.ByteString.Lazy                as BL
import           Data.Default
import           Data.Text                           (Text)
import           Network.HTTP.Client
import           Web.Payments.Cielo.Types.DeriveJSON

type MonadCielo m = ( MonadIO m
                    , MonadReader CieloConfig m
                    , MonadError CieloError m
                    )
type CieloM a = ReaderT CieloConfig (ExceptT CieloError IO) a

data CieloConfig = CieloConfig { cieloConfigMerchant    :: Merchant
                               , cieloConfigEnvironment :: Environment
                               }

data CieloError
    = CieloJSONError String SomeException BL.ByteString
    | CieloInvalidError String HttpException (Maybe Value)
    | CieloNotFoundError String HttpException
    | CieloHttpException String HttpException (Maybe Value)
  deriving(Show)

instance Exception CieloError where

data Merchant = Merchant { merchantId  :: Text
                         , merchantKey :: Text
                         }
  deriving(Read, Show)

data Environment = Environment { environmentApiUrl      :: Text
                               , environmentApiQueryUrl :: Text
                               }
  deriving(Read, Show)

instance Default Environment where
    def = sandboxEnv

productionEnv :: Environment
productionEnv = Environment "https://api.cieloecommerce.cielo.com.br"
                            "https://apiquery.cieloecommerce.cielo.com.br"

sandboxEnv :: Environment
sandboxEnv = Environment "https://apisandbox.cieloecommerce.cielo.com.br"
                         "https://apiquerysandbox.cieloecommerce.cielo.com.br"

data PaymentProvider = PaymentProviderBradesco
                     | PaymentProviderBancoDoBrasil
                     | PaymentProviderSimulado
  deriving(Read, Show)

deriveJSON ''PaymentProvider

data PaymentType = PaymentTypeCreditCard
                 | PaymentTypeDebitCard
                 | PaymentTypeElectronicTransfer
                 | PaymentTypeBoleto
  deriving(Read, Show)

instance Default PaymentType where
    def = PaymentTypeCreditCard

deriveJSON ''PaymentType

data Currency = CurrencyBRL
              | CurrencyUSD
              | CurrencyMXN
              | CurrencyCOP
              | CurrencyCLP
              | CurrencyARS
              | CurrencyPEN
              | CurrencyEUR
              | CurrencyPYN
              | CurrencyUYU
              | CurrencyVEB
              | CurrencyVEF
              | CurrencyGBP
  deriving(Read, Show)

instance Default Currency where
    def = CurrencyBRL

deriveJSON ''Currency

data Interval = IntervalMonthly
              | IntervalBimonthly
              | IntervalQuarterly
              | IntervalSemiAnnual
              | IntervalAnnual
  deriving(Read, Show)

instance Default Interval where
    def = IntervalMonthly

deriveJSON ''Interval

data RecurrentPayment = RecurrentPayment { recurrentPaymentAuthorizeNow :: Bool
                                         , recurrentPaymentEndDate      :: Maybe Text
                                         , recurrentPaymentInterval     :: Maybe Interval
                                         }
  deriving(Read, Show)

instance Default RecurrentPayment where
    def = RecurrentPayment True Nothing (Just IntervalMonthly)

deriveJSON ''RecurrentPayment

data Address = Address { addressStreet     :: Maybe Text
                       , addressNumber     :: Maybe Text
                       , addressComplement :: Maybe Text
                       , addressZipCode    :: Maybe Text
                       , addressCity       :: Maybe Text
                       , addressState      :: Maybe Text
                       , addressCountry    :: Maybe Text
                       }
  deriving(Read, Show)

instance Default Address where
    def = Address Nothing Nothing Nothing Nothing Nothing Nothing Nothing

deriveJSON ''Address

data CreditCard = CreditCard { creditCardCardNumber     :: Text
                             , creditCardHolder         :: Text
                             , creditCardExpirationDate :: Text
                             , creditCardSecurityCode   :: Maybe Text
                             , creditCardSaveCard       :: Maybe Bool
                             , creditCardBrand          :: Text
                             , creditCardCardToken      :: Maybe Text
                             }
  deriving(Read, Show)

instance Default CreditCard where
    def = CreditCard "" "" "" Nothing Nothing "" Nothing

deriveJSON ''CreditCard

data Customer = Customer { customerName            :: Text
                         , customerEmail           :: Maybe Text
                         , customerBirthDate       :: Maybe Text
                         , customerIdentity        :: Maybe Text
                         , customerIdentityType    :: Maybe Text
                         , customerAddress         :: Maybe Address
                         , customerDeliveryAddress :: Maybe Address
                         }
  deriving(Read, Show)

instance Default Customer where
    def = Customer "" Nothing Nothing Nothing Nothing Nothing Nothing

deriveJSON ''Customer

data Payment = Payment { paymentServiceTaxAmount    :: Int
                       , paymentInstallments        :: Int
                       , paymentInterest            :: Value
                       -- ^ Sometimes this is a string, sometimes it's an int... :P
                       , paymentCreditCard          :: CreditCard
                       , paymentType                :: PaymentType
                       , paymentAmount              :: Int
                       , paymentCapture             :: Maybe Bool
                       , paymentAuthenticate        :: Maybe Bool
                       , paymentRecurrent           :: Maybe Bool
                       , paymentRecurrentPayment    :: Maybe RecurrentPayment
                       , paymentTid                 :: Maybe Text
                       , paymentProofOfSale         :: Maybe Text
                       , paymentAuthorizationCode   :: Maybe Text
                       , paymentSoftDescriptor      :: Maybe Text
                       , paymentReturnUrl           :: Maybe Text
                       , paymentProvider            :: Maybe PaymentProvider
                       , paymentPaymentId           :: Maybe Text
                       , paymentReceivedDate        :: Maybe Text
                       , paymentCapturedAmount      :: Maybe Int
                       , paymentCapturedDate        :: Maybe Text
                       , paymentCurrency            :: Maybe Currency
                       , paymentCountry             :: Maybe Text
                       , paymentReturnCode          :: Maybe Text
                       , paymentReturnMessage       :: Maybe Text
                       , paymentStatus              :: Maybe Int
                       , paymentLinks               :: Maybe [Value]
                       , paymentExtraDataCollection :: Maybe [Value]
                       , paymentExpirationDate      :: Maybe Text
                       , paymentUrl                 :: Maybe Text
                       , paymentNumber              :: Maybe Text
                       , paymentBarCodeNumber       :: Maybe Text
                       , paymentDigitableLine       :: Maybe Text
                       , paymentAddress             :: Maybe Text
                       }
  deriving(Read, Show)

instance Default Payment where
    def = Payment { paymentServiceTaxAmount    = 0
                  , paymentInstallments        = 1
                  , paymentInterest            = Null
                  , paymentCapture             = Nothing
                  , paymentAuthenticate        = Nothing
                  , paymentRecurrent           = Nothing
                  , paymentRecurrentPayment    = Nothing
                  , paymentCreditCard          = def
                  , paymentTid                 = Nothing
                  , paymentProofOfSale         = Nothing
                  , paymentAuthorizationCode   = Nothing
                  , paymentSoftDescriptor      = Nothing
                  , paymentReturnUrl           = Nothing
                  , paymentProvider            = Nothing
                  , paymentPaymentId           = Nothing
                  , paymentType                = def
                  , paymentAmount              = 0
                  , paymentReceivedDate        = Nothing
                  , paymentCapturedAmount      = Nothing
                  , paymentCapturedDate        = Nothing
                  , paymentCurrency            = Nothing
                  , paymentCountry             = Nothing
                  , paymentReturnCode          = Nothing
                  , paymentReturnMessage       = Nothing
                  , paymentStatus              = Nothing
                  , paymentLinks               = Nothing
                  , paymentExtraDataCollection = Nothing
                  , paymentExpirationDate      = Nothing
                  , paymentUrl                 = Nothing
                  , paymentNumber              = Nothing
                  , paymentBarCodeNumber       = Nothing
                  , paymentDigitableLine       = Nothing
                  , paymentAddress             = Nothing
                  }

deriveJSON ''Payment

data Sale = Sale { saleMerchantOrderId :: Text
                 , saleCustomer        :: Customer
                 , salePayment         :: Payment
                 }
  deriving(Read, Show)

deriveJSON ''Sale
