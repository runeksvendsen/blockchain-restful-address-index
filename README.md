## RESTful Bitcoin address index server

Thin, RESTful HTTP server-wrapper for [Bitcoin Core w/ address-index patch](https://github.com/btcdrak/bitcoin/tree/addrindex-0.12).

Exposes a two resources:

* `/outputs/<address>/unspent` (list of all unspent outputs paying to `<address>`)
* `/publishTx`
  * Request body: Hex-encoded transaction data (**Content-Type: text/plain; charset=utf-8**)
  * Successful response body: Hex-encoded transaction ID

### Limitations:
Unspent outputs are not returned until they have at least a single confirmation. However, if a new, unconfirmed transaction redeems an output, this output will not be included in the returned results. In other words, you cannot get information about an unspent output until it has at least one confirmation, but it will disappear from the return result as a soon as a transaction appears which spends it.

### Example requests (TODO):

    $ curl --silent https://blockchain.runeks.me/outputs/17RGKU1iHhiTBLoBFFFSJ6jX66NriVoanz/unspent | jq
    [
      {
        "value": 834997,
        "address": "17RGKU1iHhiTBLoBFFFSJ6jX66NriVoanz",
        "confirmations": 5,
        "funding_txid": "ee1eb388048f9b10857fc047cf4539519efbb5e29b387fe77e3f821a18fcd134",
        "funding_vout": 2
      }
    ]
    
    $ curl -X POST -H "Content-Type: text/plain; charset=utf-8" --data-ascii "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff6403e3790637e4b883e5bda9e7a59ee4bb99e9b1bcbe1ad6e8398f0ff0f24124d45fb351df38602884633dfbabe8c4fa9af264957102000000f09f909f0f4d696e656420627920777a77363534000000000000000000000000000000000000000000000000bd43000001bac4da4c000000001976a914c825a1ecf2a6830c4401620c3a16f1995057c2ab88ac4d4c7c33" https://blockchain.runeks.me/publishTx
    b2632e6c65776cb014a37b3ed54243a21ee98df8d89d8a2cf4055c5d2e4e21d8

