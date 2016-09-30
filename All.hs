{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module All where

import           Data.Aeson ( Value )
import           DeriveJSON
import           Data.Text  ( Text )

data Environment = Environment { environmentApiUrl      :: Text
                               , environmentApiQueryUrl :: Text
                               }

productionEnv = Environment "https://api.cieloecommerce.cielo.com.br/"
                            "https://apiquery.cieloecommerce.cielo.com.br/"

sandboxEnv = Environment "https://apisandbox.cieloecommerce.cielo.com.br/"
                         "https://apiquerysandbox.cieloecommerce.cielo.com.br/"

data PaymentProvider = PaymentProviderBradesco
                     | PaymentProviderBancoDoBrasil
                     | PaymentProviderSimulado

deriveJSON ''PaymentProvider

data PaymentType = PaymentTypeCreditCard
                 | PaymentTypeDebitCard
                 | PaymentTypeElectronicTransfer
                 | PaymentTypeBoleto

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

deriveJSON ''Currency

data Interval = IntervalMonthly
              | IntervalBimonthly
              | IntervalQuarterly
              | IntervalSemiAnnual
              | IntervalAnnual

deriveJSON ''Interval

data RecurrentPayment = RecurrentPayment { recurrentPaymentAuthorizeNow :: Bool
                                         , recurrentPaymentEndData      :: Text
                                         , recurrentPaymentInterval     :: Interval
                                         }

deriveJSON ''RecurrentPayment

data Address = Address { addressStreet     :: Text
                       , addressNumber     :: Text
                       , addressComplement :: Text
                       , addressZipCode    :: Text
                       , addressCity       :: Text
                       , addressState      :: Text
                       , addressCountry    :: Text
                       }

deriveJSON ''Address

data CreditCard = CreditCard { creditCardCardNumber     :: Text
                             , creditCardHolder         :: Text
                             , creditCardExpirationDate :: Text
                             , creditCardSecurityCode   :: Text
                             , creditCardSaveCard       :: Bool
                             , creditCardBrand          :: Text
                             , creditCardCardToken      :: Text
                             }

deriveJSON ''CreditCard

data Customer = Customer { customerName            :: Text
                         , customerEmail           :: Text
                         , customerBirthDate       :: Text
                         , customerIdentity        :: Text
                         , customerIdentityType    :: Text
                         , customerAddress         :: Address
                         , customerDeliveryAddress :: Address
                         }

deriveJSON ''Customer

data Payment = Payment { paymentServiceTaxAmount    :: Int
                       , paymentInstallments        :: Int
                       , paymentInterest            :: Text
                       , paymentCapture             :: Bool
                       , paymentAuthenticate        :: Bool
                       , paymentRecurrent           :: Bool
                       , paymentRecurrentPayment    :: RecurrentPayment
                       , paymentCreditCard          :: CreditCard
                       , paymentTid                 :: Text
                       , paymentProofOfSale         :: Text
                       , paymentAuthorizationCode   :: Text
                       , paymentSoftDescriptor      :: Text
                       , paymentReturnUrl           :: Text
                       , paymentProvider            :: PaymentProvider
                       , paymentPaymentId           :: Text
                       , paymentType                :: PaymentType
                       , paymentAmount              :: Int
                       , paymentReceivedDate        :: Text
                       , paymentCapturedAmount      :: Int
                       , paymentCapturedDate        :: Text
                       , paymentCurrency            :: Currency
                       , paymentCountry             :: Text
                       , paymentReturnCode          :: Text
                       , paymentReturnMessage       :: Text
                       , paymentStatus              :: Int
                       , paymentLinks               :: [Value]
                       , paymentExtraDataCollection :: [Value]
                       , paymentExpirationDate      :: Text
                       , paymentUrl                 :: Text
                       , paymentNumber              :: Text
                       , paymentBarCodeNumber       :: Text
                       , paymentDigitableLine       :: Text
                       , paymentAddress             :: Text
                       }

deriveJSON ''Payment

data Sale = Sale { saleMerchantOrderId :: Text
                 , saleCustomer        :: Customer
                 , salePayment         :: Payment
                 }

deriveJSON ''Sale
