module Web.Payments.CieloSpec where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Default
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
    describe "createSale" $ do
        it "creates a sale with cielo" $
            runCielo cnf $ do
                uuid <- getMerchantOrderId
                sale <- createSale Sale { saleMerchantOrderId = uuid
                                        , saleCustomer = def { customerName = "Pedro Tacla Yamada"
                                                             }
                                        , salePayment = def { paymentCreditCard = Just def { creditCardCardNumber = "0000000000000001"
                                                                                      , creditCardHolder = "Pedro Tacla Yamada"
                                                                                      , creditCardExpirationDate = "12/2023"
                                                                                      , creditCardSecurityCode = Just "123"
                                                                                      , creditCardBrand = "visa"
                                                                                      }
                                                            , paymentAmount = Just 10000
                                                            }
                                        }
                liftIO $ pPrint sale

        it "creates recurrent sales" $
            runCielo cnf $ do
                uuid <- getMerchantOrderId
                sale <- createSale Sale { saleMerchantOrderId = uuid
                                        , saleCustomer = def { customerName = "Pedro Tacla Yamada"
                                                             }
                                        , salePayment = def { paymentCreditCard = Just def { creditCardCardNumber = "0000000000000001"
                                                                                      , creditCardHolder = "Pedro Tacla Yamada"
                                                                                      , creditCardExpirationDate = "12/2023"
                                                                                      , creditCardSecurityCode = Just "123"
                                                                                      , creditCardBrand = "visa"
                                                                                      }
                                                            , paymentAmount = Just 10000
                                                            , paymentRecurrentPayment = Just def
                                                            }
                                        }
                liftIO $ pPrint sale

        it "creates recurrent sales with start dates" $
            runCielo cnf $ do
                uuid <- getMerchantOrderId
                sale <- createSale Sale { saleMerchantOrderId = uuid
                                        , saleCustomer = def { customerName = "Pedro Tacla Yamada"
                                                             }
                                        , salePayment = def { paymentCreditCard = Just def { creditCardCardNumber = "0000000000000001"
                                                                                      , creditCardHolder = "Pedro Tacla Yamada"
                                                                                      , creditCardExpirationDate = "12/2023"
                                                                                      , creditCardSecurityCode = Just "123"
                                                                                      , creditCardBrand = "visa"
                                                                                      }
                                                            , paymentAmount = Just 10000
                                                            , paymentRecurrentPayment = Just def { recurrentPaymentStartDate = Just "2018-12-03"
                                                                                                 }
                                                            }
                                        }
                liftIO $ pPrint sale

        it "creates recurrent sales with annual intervals" $ do
            sale <- runCielo cnf $ do
                        uuid <- getMerchantOrderId
                        sale <- createSale Sale { saleMerchantOrderId = uuid
                                                , saleCustomer = def { customerName = "Pedro Tacla Yamada"
                                                                     }
                                                , salePayment = def { paymentCreditCard = Just def { creditCardCardNumber = "0000000000000001"
                                                                                              , creditCardHolder = "Pedro Tacla Yamada"
                                                                                              , creditCardExpirationDate = "12/2023"
                                                                                              , creditCardSecurityCode = Just "123"
                                                                                              , creditCardBrand = "visa"
                                                                                              }
                                                                    , paymentAmount = Just 10000
                                                                    , paymentRecurrentPayment = Just def { recurrentPaymentStartDate = Just "2018-12-03"
                                                                                                         , recurrentPaymentInterval = Just IntervalAnnual
                                                                                                         }
                                                                    }
                                                }
                        liftIO $ pPrint sale
                        return sale
            let Just intl = recurrentPaymentInterval <$> paymentRecurrentPayment (salePayment sale)
            intl `shouldBe` Just IntervalAnnual

    describe "cancelRecurrentPayment" $
        it "cancels recurrent payments" $
            runCielo cnf $ do
                uuid <- getMerchantOrderId
                sale <- createSale Sale { saleMerchantOrderId = uuid
                                        , saleCustomer = def { customerName = "Pedro Tacla Yamada"
                                                             }
                                        , salePayment = def { paymentCreditCard = Just def { creditCardCardNumber = "0000000000000001"
                                                                                      , creditCardHolder = "Pedro Tacla Yamada"
                                                                                      , creditCardExpirationDate = "12/2023"
                                                                                      , creditCardSecurityCode = Just "123"
                                                                                      , creditCardBrand = "visa"
                                                                                      }
                                                            , paymentAmount = Just 10000
                                                            , paymentRecurrentPayment = Just def { recurrentPaymentStartDate = Just "2018-12-03"
                                                                                                 , recurrentPaymentInterval = Just IntervalAnnual
                                                                                                 }
                                                            }
                                        }
                liftIO $ pPrint sale
                let Just paymentId = join (recurrentPaymentRecurrentPaymentId <$> paymentRecurrentPayment (salePayment sale))
                cancelRecurrentPayment paymentId

    describe "updateRecurrentPaymentCustomer" $
        it "updates recurrent payments' customer" $ do
            eml <- runCielo cnf $ do
                       uuid <- getMerchantOrderId
                       sale <- createSale Sale { saleMerchantOrderId = uuid
                                               , saleCustomer = def { customerName = "Pedro Tacla Yamada"
                                                                    }
                                               , salePayment = def { paymentCreditCard = Just def { creditCardCardNumber = "0000000000000001"
                                                                                                  , creditCardHolder = "Pedro Tacla Yamada"
                                                                                                  , creditCardExpirationDate = "12/2023"
                                                                                                  , creditCardSecurityCode = Just "123"
                                                                                                  , creditCardBrand = "visa"
                                                                                                  }
                                                                   , paymentAmount = Just 10000
                                                                   , paymentRecurrentPayment = Just def { recurrentPaymentStartDate = Just "2018-12-03"
                                                                                                        , recurrentPaymentInterval = Just IntervalAnnual
                                                                                                        }
                                                                   }
                                               }
                       liftIO $ pPrint sale
                       let Just paymentId = join (recurrentPaymentRecurrentPaymentId <$> paymentRecurrentPayment (salePayment sale))
                       updateRecurrentPaymentCustomer paymentId
                                                      (def { customerEmail = Just "yamada@beijaflor.io"
                                                           , customerName = "Pedro Tacla Yamada"
                                                           })
                       RecurrentPaymentQuery{..} <- queryRecurrentSale paymentId
                       return $ customerEmail recurrentPaymentQueryCustomer
            eml `shouldBe` Just "yamada@beijaflor.io"

    describe "salesByMerchantOrderQuery" $
        it "responds with all payments for a merchant uuid" $
            runCielo cnf $ do
                uuid <- getMerchantOrderId
                sale <- createSale Sale { saleMerchantOrderId = uuid
                                        , saleCustomer = def { customerName = "Pedro Tacla Yamada"
                                                             }
                                        , salePayment = def { paymentCreditCard = Just def { creditCardCardNumber = "0000000000000001"
                                                                                           , creditCardHolder = "Pedro Tacla Yamada"
                                                                                           , creditCardExpirationDate = "12/2023"
                                                                                           , creditCardSecurityCode = Just "123"
                                                                                           , creditCardBrand = "visa"
                                                                                           }
                                                            , paymentAmount = Just 10000
                                                            , paymentRecurrentPayment = Just def { recurrentPaymentStartDate = Just "2018-12-03"
                                                                                                 , recurrentPaymentInterval = Just IntervalAnnual
                                                                                                 }
                                                            }
                                        }
                liftIO $ pPrint sale
                query <- querySalesByMerchantOrderId uuid
                liftIO $ pPrint query

    describe "uncancelRecurrentPayment" $
        it "uncancels recurrent payments" $
            runCielo cnf $ do
                uuid <- getMerchantOrderId
                sale <- createSale Sale { saleMerchantOrderId = uuid
                                        , saleCustomer = def { customerName = "Pedro Tacla Yamada"
                                                             }
                                        , salePayment = def { paymentCreditCard = Just def { creditCardCardNumber = "0000000000000001"
                                                                                           , creditCardHolder = "Pedro Tacla Yamada"
                                                                                           , creditCardExpirationDate = "12/2023"
                                                                                           , creditCardSecurityCode = Just "123"
                                                                                           , creditCardBrand = "visa"
                                                                                           }
                                                            , paymentAmount = Just 10000
                                                            , paymentRecurrentPayment = Just def { recurrentPaymentStartDate = Just "2018-12-03"
                                                                                                 , recurrentPaymentInterval = Just IntervalAnnual
                                                                                                 }
                                                            }
                                        }
                liftIO $ pPrint sale
                let Just paymentId = join (recurrentPaymentRecurrentPaymentId <$> paymentRecurrentPayment (salePayment sale))
                cancelRecurrentPayment paymentId
                uncancelRecurrentPayment paymentId

    describe "updateRecurrentPaymentPayment" $
        it "works" $
            runCielo cnf $ do
                uuid <- getMerchantOrderId
                sale <- createSale Sale { saleMerchantOrderId = uuid
                                        , saleCustomer = def { customerName = "Pedro Tacla Yamada"
                                                             }
                                        , salePayment = def { paymentCreditCard = Just def { creditCardCardNumber = "0000000000000001"
                                                                                           , creditCardHolder = "Pedro Tacla Yamada"
                                                                                           , creditCardExpirationDate = "12/2023"
                                                                                           , creditCardSecurityCode = Just "123"
                                                                                           , creditCardBrand = "visa"
                                                                                           }
                                                            , paymentAmount = Just 10000
                                                            , paymentRecurrentPayment = Just def { recurrentPaymentStartDate = Just "2018-12-03"
                                                                                                 , recurrentPaymentInterval = Just IntervalAnnual
                                                                                                 }
                                                            }
                                        }
                liftIO $ pPrint sale
                let Just paymentId = join (recurrentPaymentRecurrentPaymentId <$> paymentRecurrentPayment (salePayment sale))
                updateRecurrentPaymentPayment paymentId
                    def { paymentCreditCard = Just def { creditCardCardNumber = "0000000000000001"
                                                       , creditCardHolder = "Pedro Tacla Yamada"
                                                       , creditCardExpirationDate = "12/2023"
                                                       , creditCardSecurityCode = Just "123"
                                                       , creditCardBrand = "visa"
                                                       }
                        , paymentAmount = Just 123
                        , paymentInstallments = Just 1
                        }
                rpayment <- queryRecurrentSale paymentId
                liftIO $ pPrint rpayment

    describe "updateRecurrentPaymentNextPaymentAmount" $
        it "works" $
            runCielo cnf $ do
                uuid <- getMerchantOrderId
                sale <- createSale Sale { saleMerchantOrderId = uuid
                                        , saleCustomer = def { customerName = "Pedro Tacla Yamada"
                                                             }
                                        , salePayment = def { paymentCreditCard = Just def { creditCardCardNumber = "0000000000000001"
                                                                                           , creditCardHolder = "Pedro Tacla Yamada"
                                                                                           , creditCardExpirationDate = "12/2023"
                                                                                           , creditCardSecurityCode = Just "123"
                                                                                           , creditCardBrand = "visa"
                                                                                           }
                                                            , paymentAmount = Just 10000
                                                            , paymentRecurrentPayment = Just def { recurrentPaymentStartDate = Just "2018-12-03"
                                                                                                 , recurrentPaymentInterval = Just IntervalAnnual
                                                                                                 }
                                                            }
                                        }
                liftIO $ pPrint sale
                let Just rpaymentId = join (recurrentPaymentRecurrentPaymentId <$> paymentRecurrentPayment (salePayment sale))
                updateRecurrentPaymentNextPaymentAmount rpaymentId 123413
                rpayment <- queryRecurrentSale rpaymentId
                liftIO $ pPrint rpayment

    describe "updateRecurrentPaymentNextPaymentDate" $
        it "works" $
            runCielo cnf $ do
                uuid <- getMerchantOrderId
                sale <- createSale Sale { saleMerchantOrderId = uuid
                                        , saleCustomer = def { customerName = "Pedro Tacla Yamada"
                                                             }
                                        , salePayment = def { paymentCreditCard = Just def { creditCardCardNumber = "0000000000000001"
                                                                                           , creditCardHolder = "Pedro Tacla Yamada"
                                                                                           , creditCardExpirationDate = "12/2023"
                                                                                           , creditCardSecurityCode = Just "123"
                                                                                           , creditCardBrand = "visa"
                                                                                           }
                                                            , paymentAmount = Just 10000
                                                            , paymentRecurrentPayment = Just def { recurrentPaymentStartDate = Just "2018-12-03"
                                                                                                 , recurrentPaymentInterval = Just IntervalAnnual
                                                                                                 }
                                                            }
                                        }
                liftIO $ pPrint sale
                let Just rpaymentId = join (recurrentPaymentRecurrentPaymentId <$> paymentRecurrentPayment (salePayment sale))
                updateRecurrentPaymentNextPaymentDate rpaymentId "2016-12-12"
                rpayment <- queryRecurrentSale rpaymentId
                liftIO $ pPrint rpayment

    describe "querySale" $
        it "works" $
            runCielo cnf $ do
                uuid <- getMerchantOrderId
                sale <- createSale Sale { saleMerchantOrderId = uuid
                                        , saleCustomer = def { customerName = "Pedro Tacla Yamada"
                                                             }
                                        , salePayment = def { paymentCreditCard = Just def { creditCardCardNumber = "0000000000000001"
                                                                                           , creditCardHolder = "Pedro Tacla Yamada"
                                                                                           , creditCardExpirationDate = "12/2023"
                                                                                           , creditCardSecurityCode = Just "123"
                                                                                           , creditCardBrand = "visa"
                                                                                           }
                                                            , paymentAmount = Just 10000
                                                            }
                                        }
                liftIO $ pPrint sale
                let Just paymentId = paymentPaymentId (salePayment sale)
                sale' <- querySale paymentId
                liftIO $ pPrint sale'

    describe "captureSale / voidSale" $
        it "works" $
            runCielo cnf $ do
                uuid <- getMerchantOrderId
                sale <- createSale Sale { saleMerchantOrderId = uuid
                                        , saleCustomer = def { customerName = "Pedro Tacla Yamada"
                                                             }
                                        , salePayment = def { paymentCreditCard = Just def { creditCardCardNumber = "0000000000000001"
                                                                                      , creditCardHolder = "Pedro Tacla Yamada"
                                                                                      , creditCardExpirationDate = "12/2023"
                                                                                      , creditCardSecurityCode = Just "123"
                                                                                      , creditCardBrand = "visa"
                                                                                      }
                                                            , paymentAmount = Just 10000
                                                            }
                                        }
                liftIO $ pPrint sale
                let Just paymentId = paymentPaymentId (salePayment sale)
                sale''' <- captureSale paymentId (Just 9000) Nothing
                liftIO $ pPrint sale'''
                sale'' <- voidSale paymentId (Just 1000)
                liftIO $ pPrint sale''
