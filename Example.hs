{-# LANGUAGE OverloadedStrings #-}
import           Web.Payments.Cielo

main :: IO ()
main = do
    cnf <- cieloConfigFromEnv
    runCielo cnf $ do
        uuid <- getMerchantOrderId
        let customer = def { customerName = "Pedro Tacla Yamada"
                           }
            payment = def { paymentCreditCard =
                                Just def { creditCardCardNumber = "0000000000000001"
                                         , creditCardHolder = "Pedro Tacla Yamada"
                                         , creditCardExpirationDate = "12/2023"
                                         , creditCardSecurityCode = Just "123"
                                         , creditCardBrand = "visa"
                                         }
                          , paymentAmount = Just 10000
                          }
        sale <- createSale Sale { saleMerchantOrderId = uuid
                                , saleCustomer = customer
                                , salePayment = payment
                                }
        liftIO $ print sale
