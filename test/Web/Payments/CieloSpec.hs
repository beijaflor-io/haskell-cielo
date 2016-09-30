module Web.Payments.CieloSpec where

import           Data.Text                (Text)
import           Data.UUID
import           Data.UUID.V4
import           Test.Hspec
import           Text.Show.Pretty
import           Web.Payments.Cielo
import           Web.Payments.Cielo.Types

data DocTestCase = DocTestCase Text Text Int Text
docTestCases = [ DocTestCase "Autorizado" "0000000000000001" 4 "Operação realizada com sucesso"
               , DocTestCase "Autorizado" "0000000000000004" 4 "Operação realizada com sucesso"
               , DocTestCase "Não Autorizado" "0000000000000002" 2 "Não Autorizada"
               , DocTestCase "Não Autorizado" "0000000000000007" 77 "Cartão Cancelado"
               , DocTestCase "Não Autorizado" "0000000000000008" 70 "Problemas com o Cartão de Crédito"
               , DocTestCase "Não Autorizado" "0000000000000005" 78 "Cartão Bloqueado"
               , DocTestCase "Não Autorizado" "0000000000000003" 57 "Cartão Expirado"
               , DocTestCase "Não Autorizado" "0000000000000006" 99 "Time Out"
               ]

data DocRandomTestCase = DocRandomTestCase Text Text [Int] [Text]
docRandomTestCase = DocRandomTestCase "Autorização Aleatória" "0000000000000009" [ 4, 99 ] [ "Operation Successful", "Time Out" ]

spec :: Spec
spec = describe "cielo" $ do
    cnf <- runIO cieloConfigFromEnv
    describe "createSale" $
        it "creates a sale with cielo" $ do
            uuid <- toText <$> nextRandom
            r <- runCielo cnf $
                createSale Sale { saleMerchantOrderId = uuid
                                , saleCustomer = Customer { customerName            = "Pedro Tacla Yamada"
                                                          , customerEmail           = Nothing
                                                          , customerBirthDate       = Nothing
                                                          , customerIdentity        = Nothing
                                                          , customerIdentityType    = Nothing
                                                          , customerAddress         = Nothing
                                                          , customerDeliveryAddress = Nothing
                                                          }
                                , salePayment = Payment { paymentServiceTaxAmount    = 0
                                                        , paymentInstallments        = 1
                                                        , paymentInterest            = Nothing
                                                        , paymentCapture             = Nothing
                                                        , paymentAuthenticate        = Nothing
                                                        , paymentRecurrent           = Nothing
                                                        , paymentRecurrentPayment    = Nothing
                                                        , paymentCreditCard          = CreditCard { creditCardCardNumber     = "0000000000000001"
                                                                                                  , creditCardHolder         = "Pedro Tacla Yamada"
                                                                                                  , creditCardExpirationDate = "12/2023"
                                                                                                  , creditCardSecurityCode   = Just "123"
                                                                                                  , creditCardSaveCard       = Nothing
                                                                                                  , creditCardBrand          = "visa"
                                                                                                  , creditCardCardToken      = Nothing
                                                                                                  }
                                                        , paymentTid                 = Nothing
                                                        , paymentProofOfSale         = "123123"
                                                        , paymentAuthorizationCode   = "123456"
                                                        , paymentSoftDescriptor      = Nothing
                                                        , paymentReturnUrl           = ""
                                                        , paymentProvider            = PaymentProviderBradesco
                                                        , paymentPaymentId           = Nothing
                                                        , paymentType                = PaymentTypeCreditCard
                                                        , paymentAmount              = 10000
                                                        , paymentReceivedDate        = Nothing
                                                        , paymentCapturedAmount      = Nothing
                                                        , paymentCapturedDate        = Nothing
                                                        , paymentCurrency            = Nothing
                                                        , paymentCountry             = Nothing
                                                        , paymentReturnCode          = Nothing
                                                        , paymentReturnMessage       = Nothing
                                                        , paymentStatus              = Nothing
                                                        , paymentLinks               = []
                                                        , paymentExtraDataCollection = []
                                                        , paymentExpirationDate      = Nothing
                                                        , paymentUrl                 = Nothing
                                                        , paymentNumber              = Nothing
                                                        , paymentBarCodeNumber       = Nothing
                                                        , paymentDigitableLine       = Nothing
                                                        , paymentAddress             = Nothing
                                                        }
                                }
            pPrint r
