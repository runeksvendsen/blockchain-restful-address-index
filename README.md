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
    
    # TODO: fix
    $ curl -X POST -H "Content-Type: text/plain; charset=utf-8" --data-ascii "0100000001de5ad8f627ebaf5a988d55fcc32d40099ac382cb7219e74babe0e8b13b77aa1e010000006b483045022100f35e797cf452116027a836c67e109877589f731f711e911752ff8a0e0a90c71e02201bdda13a6f62630378f2a16aedd52297aac1a04cff200120e094f5d1fe63ed700121022a78631a0d3a16277ebdbb20a296841fb4d3702dbf84f32b5c0ec9e91e0e1063feffffff0241ce4500000000001976a9147540693005068b8df2e65c7380e0d7845fc150c388ac18be01000000000017a9142b2ff9a35834e780a6bc40bc2fa3ea2e11552d7187f6a10d00" https://blockchain.runeks.me/publishTx
    b2632e6c65776cb014a37b3ed54243a21ee98df8d89d8a2cf4055c5d2e4e21d8

