# haskell-cielo
Haskell bindings to the Cielo API v3. AGPLv3 licensed.

[Hackage Link](https://hackage.haskell.org/package/cielo)

All endpoints are wrapped; and smoke tested. A lot of the code was generated
from Java bindings; the types aren't properly specified, sometimes the Cielo API
uses different request/response types so some things are just set to the `Value`
aeson type, since we can only type it as being JSON.

[Official API documentation](http://developercielo.github.io/Webservice-3.0/english.html)

## Basic Usage
```haskell
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
```

## License
AGPLv3
