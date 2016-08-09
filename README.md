## RESTful Bitcoin address index server

[Bitcoin Core w/ address-index patch](https://github.com/btcdrak/bitcoin/tree/addrindex-0.12) wrapped in a thin RESTful HTTP server.

Exposes a two resources:

* `/outputs/<address>/unspent` (list of all unspent outputs paying to `<address>`)
* `/publishTx` (publish/push hex-encoded transaction to the network; return txid)


### Limitations:
Unspent outputs are not returned until they have at least a single confirmation. However, if a new, unconfirmed transaction redeems an output, this output will not be included in the returned results. In other words, you cannot get information about an unspent output until it has at least one confirmation, but it will disappear from the return result as a soon as a transaction appears which spends it.

### Example usage and output (**needs update**):

        $ curl -v 'http://localhost:8000/unspentOutputs/2N414xMNQaiaHCT5D7JamPz7hJEc9RG7469'
        *   Trying 127.0.0.1...
        * Connected to localhost (127.0.0.1) port 8000 (#0)
        > GET /unspentOutputs/2N414xMNQaiaHCT5D7JamPz7hJEc9RG7469 HTTP/1.1
        > Host: localhost:8000
        > User-Agent: curl/7.43.0
        > Accept: */*
        > 
        < HTTP/1.1 200 OK
        < Server: Snap/0.9.5.1
        < Date: Tue, 05 Jul 2016 21:30:02 GMT
        < Transfer-Encoding: chunked
        < 
        [
            {
                "value": "0.00158262",
                "address": "2N414xMNQaiaHCT5D7JamPz7hJEc9RG7469",
                "confirmations": 3,
                "funding_txid": "7f7e37e129034117df68ce65f28d4d6ce62d81714a595057aa4902cd5697e333",
                "funding_vout": 0
            }
    ]
  
