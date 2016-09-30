{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Payments.Cielo.Types where

import           Data.Aeson                          (Value)
import qualified Data.Aeson.TH                       as Aeson
import           Data.Text                           (Text)
import           Web.Payments.Cielo.Types.DeriveJSON

data Merchant = Merchant { merchantId  :: Text
                         , merchantKey :: Text
                         }
  deriving(Read, Show)

data Environment = Environment { environmentApiUrl      :: Text
                               , environmentApiQueryUrl :: Text
                               }
  deriving(Read, Show)

productionEnv :: Environment
productionEnv = Environment "https://api.cieloecommerce.cielo.com.br/"
                            "https://apiquery.cieloecommerce.cielo.com.br/"

sandboxEnv :: Environment
sandboxEnv = Environment "https://apisandbox.cieloecommerce.cielo.com.br/"
                         "https://apiquerysandbox.cieloecommerce.cielo.com.br/"

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

deriveJSON ''Currency

data Interval = IntervalMonthly
              | IntervalBimonthly
              | IntervalQuarterly
              | IntervalSemiAnnual
              | IntervalAnnual
  deriving(Read, Show)

deriveJSON ''Interval

data RecurrentPayment = RecurrentPayment { recurrentPaymentAuthorizeNow :: Bool
                                         , recurrentPaymentEndData      :: Text
                                         , recurrentPaymentInterval     :: Maybe Interval
                                         }
  deriving(Read, Show)

deriveJSON ''RecurrentPayment

data Address = Address { addressStreet     :: Text
                       , addressNumber     :: Text
                       , addressComplement :: Text
                       , addressZipCode    :: Text
                       , addressCity       :: Text
                       , addressState      :: Text
                       , addressCountry    :: Text
                       }
  deriving(Read, Show)

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

deriveJSON ''Customer

data Payment = Payment { paymentServiceTaxAmount    :: Int
                       , paymentInstallments        :: Int
                       , paymentInterest            :: Maybe Int
                       , paymentCapture             :: Maybe Bool
                       , paymentAuthenticate        :: Maybe Bool
                       , paymentRecurrent           :: Maybe Bool
                       , paymentRecurrentPayment    :: Maybe RecurrentPayment
                       , paymentCreditCard          :: CreditCard
                       , paymentTid                 :: Maybe Text
                       , paymentProofOfSale         :: Text
                       , paymentAuthorizationCode   :: Text
                       , paymentSoftDescriptor      :: Maybe Text
                       , paymentReturnUrl           :: Text
                       , paymentProvider            :: PaymentProvider
                       , paymentPaymentId           :: Maybe Text
                       , paymentType                :: PaymentType
                       , paymentAmount              :: Int
                       , paymentReceivedDate        :: Maybe Text
                       , paymentCapturedAmount      :: Maybe Int
                       , paymentCapturedDate        :: Maybe Text
                       , paymentCurrency            :: Maybe Currency
                       , paymentCountry             :: Maybe Text
                       , paymentReturnCode          :: Maybe Text
                       , paymentReturnMessage       :: Maybe Text
                       , paymentStatus              :: Maybe Int
                       , paymentLinks               :: [Value]
                       , paymentExtraDataCollection :: [Value]
                       , paymentExpirationDate      :: Maybe Text
                       , paymentUrl                 :: Maybe Text
                       , paymentNumber              :: Maybe Text
                       , paymentBarCodeNumber       :: Maybe Text
                       , paymentDigitableLine       :: Maybe Text
                       , paymentAddress             :: Maybe Text
                       }
  deriving(Read, Show)

deriveJSON ''Payment

data Sale = Sale { saleMerchantOrderId :: Text
                 , saleCustomer        :: Customer
                 , salePayment         :: Payment
                 }
  deriving(Read, Show)

deriveJSON ''Sale
