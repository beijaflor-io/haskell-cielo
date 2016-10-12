# haskell-cielo
All endpoints are wrapped; and smoke tested. A lot of the code was generated
from Java bindings; the types aren't properly specified, sometimes the Cielo API
uses different request/response types so some things are just set to the `Value`
aeson type, since we can only type it as being JSON.

[Official API documentation](http://developercielo.github.io/Webservice-3.0/english.html)
