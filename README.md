## RESTful Bitcoin address index server

Thin, RESTful HTTP server-wrapper for [Bitcoin Core w/ address-index patch](https://github.com/btcdrak/bitcoin/tree/addrindex-0.12).

Exposes a two resources:

* `/outputs/<address>/unspent` (list of all unspent outputs paying to `<address>`)
* `/publishTx` (publish/push hex-encoded transaction to the network; return txid)

### Limitations:
Unspent outputs are not returned until they have at least a single confirmation. However, if a new, unconfirmed transaction redeems an output, this output will not be included in the returned results. In other words, you cannot get information about an unspent output until it has at least one confirmation, but it will disappear from the return result as a soon as a transaction appears which spends it.

