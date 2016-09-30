{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Payments.Cielo.Types where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy                as BL
import           Data.Convertible
import           Data.Default
import           Data.Monoid                         ((<>))
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
  deriving(Read, Show, Eq)

data Environment = Environment { environmentApiUrl      :: Text
                               , environmentApiQueryUrl :: Text
                               }
  deriving(Read, Show, Eq)

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
  deriving(Read, Show, Eq)

deriveJSON ''PaymentProvider

data PaymentType = PaymentTypeCreditCard
                 | PaymentTypeDebitCard
                 | PaymentTypeElectronicTransfer
                 | PaymentTypeBoleto
  deriving(Read, Show, Eq)

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
  deriving(Read, Show, Eq)

instance Default Currency where
    def = CurrencyBRL

deriveJSON ''Currency

data Interval = IntervalMonthly
              | IntervalBimonthly
              | IntervalQuarterly
              | IntervalSemiAnnual
              | IntervalAnnual
  deriving(Read, Show, Eq)

instance Default Interval where
    def = IntervalMonthly

instance ToJSON Interval where
    toJSON IntervalMonthly = "Monthly"
    toJSON IntervalBimonthly = "Bimonthly"
    toJSON IntervalQuarterly = "Quarterly"
    toJSON IntervalSemiAnnual = "SemiAnnual"
    toJSON IntervalAnnual = "Annual"

instance FromJSON Interval where
    parseJSON (String m) = case m of
        "Monthly" -> return IntervalMonthly
        "Bimonthly" -> return IntervalBimonthly
        "Quarterly" -> return IntervalQuarterly
        "SemiAnnual" -> return IntervalSemiAnnual
        "Annual" -> return IntervalAnnual
        str -> fail ("Failed to parse Interval " <> convert str)
    parseJSON (Number m) = case m of
        1 -> return IntervalMonthly
        2 -> return IntervalBimonthly
        4 -> return IntervalQuarterly
        6 -> return IntervalSemiAnnual
        12 -> return IntervalAnnual
        n -> fail ("Failed to parse Interval " <> show n)
    parseJSON invalid = typeMismatch "Interval" invalid

data RecurrentPayment = RecurrentPayment { recurrentPaymentAuthorizeNow       :: Maybe Bool
                                         , recurrentPaymentEndDate            :: Maybe Text
                                         , recurrentPaymentStartDate          :: Maybe Text
                                         , recurrentPaymentInterval           :: Maybe Interval
                                         , recurrentPaymentRecurrentPaymentId :: Maybe Text
                                         }
  deriving(Read, Show, Eq)

instance Default RecurrentPayment where
    def = RecurrentPayment { recurrentPaymentAuthorizeNow = Just True
                           , recurrentPaymentEndDate = Nothing
                           , recurrentPaymentStartDate = Nothing
                           , recurrentPaymentInterval = Just IntervalMonthly
                           , recurrentPaymentRecurrentPaymentId = Nothing
                           }

deriveJSON ''RecurrentPayment

data Address = Address { addressStreet     :: Maybe Text
                       , addressNumber     :: Maybe Text
                       , addressComplement :: Maybe Text
                       , addressZipCode    :: Maybe Text
                       , addressCity       :: Maybe Text
                       , addressState      :: Maybe Text
                       , addressCountry    :: Maybe Text
                       }
  deriving(Read, Show, Eq)

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
  deriving(Read, Show, Eq)

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
  deriving(Read, Show, Eq)

instance Default Customer where
    def = Customer "" Nothing Nothing Nothing Nothing Nothing Nothing

deriveJSON ''Customer

data Payment = Payment { paymentServiceTaxAmount    :: Maybe Int
                       , paymentInstallments        :: Maybe Int
                       , paymentInterest            :: Maybe Value
                       -- ^ Sometimes this is a string, sometimes it's an int... :P
                       , paymentCreditCard          :: Maybe CreditCard
                       , paymentType                :: Maybe PaymentType
                       , paymentAmount              :: Maybe Int
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
  deriving(Read, Show, Eq)

instance Default Payment where
    def = Payment { paymentServiceTaxAmount    = Just 0
                  , paymentInstallments        = Just 1
                  , paymentInterest            = Just Null
                  , paymentCapture             = Nothing
                  , paymentAuthenticate        = Nothing
                  , paymentRecurrent           = Nothing
                  , paymentRecurrentPayment    = Nothing
                  , paymentCreditCard          = Just def
                  , paymentTid                 = Nothing
                  , paymentProofOfSale         = Nothing
                  , paymentAuthorizationCode   = Nothing
                  , paymentSoftDescriptor      = Nothing
                  , paymentReturnUrl           = Nothing
                  , paymentProvider            = Nothing
                  , paymentPaymentId           = Nothing
                  , paymentType                = Just def
                  , paymentAmount              = Just 0
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
  deriving(Read, Show, Eq)

deriveJSON ''Sale

data SaleUpdate = SaleUpdate { saleUpdateStatus        :: Int
                             , saleUpdateReturnCode    :: Text
                             , saleUpdateReturnMessage :: Text
                             , saleUpdateLinks         :: [Value]
                             }
  deriving(Read, Show, Eq)

deriveJSON ''SaleUpdate

data RecurrentPaymentQuery
    = RecurrentPaymentQuery { recurrentPaymentQueryRecurrentPayment :: RecurrentPayment
                            , recurrentPaymentQueryCustomer         :: Customer
                            }
  deriving(Read, Show, Eq)

deriveJSON ''RecurrentPaymentQuery

data SalesByMerchantOrderQuery
    = SalesByMerchantOrderQuery { salesByMerchantOrderQueryPayments :: [Payment]
                                }
  deriving(Read, Show, Eq)

deriveJSON ''SalesByMerchantOrderQuery
