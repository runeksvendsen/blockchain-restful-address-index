## RESTful Bitcoin address index server

Thin RESTful HTTP wrapper for [address-index patched Bitcoin Core](https://github.com/btcdrak/bitcoin/tree/addrindex-0.12).

Exposes two resources:

* **GET** `/outputs/<address>/unspent` (list of all unspent outputs paying to `<address>`)
  * Response body: `Content-Type: application/json` (see example below)
* **POST** `/publishTx` (push hex-encoded transaction to the network) 
  * Request body: `Content-Type: text/plain; charset=utf-8`
  * Response body: Hex-encoded transaction ID (also `Content-Type: text/plain; charset=utf-8`)

### Limitations:
Unspent outputs are not returned until they have at least a single confirmation. However, if a new, unconfirmed transaction redeems an output, this output will not be included in the returned results. In other words, you cannot get information about an unspent output until it has at least one confirmation, but the output will disappear from the return result as a soon as a spending transaction, confirmed or not, appears.

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
    
    $ curl --silent -X POST -H "Content-Type: text/plain; charset=utf-8" --data-ascii "0100000001c06626039bbb4710a60d3d469f84fb7d0cdd7eece891a0ee77d95b522ef337f900000000fdfe000048304502210081c7d8c575e5aa06bd75ec03a98674168e57842baaf0241263e57e322d5823a202202ed1df8f0caa1d038d15882cab38bb4030ce85d890e9d62587ca4ed13dc7681a01483045022100949b0b6da2057e382e342ce23c6294f1505878200086957d9a447e58d0d8f0bb022075e574bb023885d3ba97d34151009b09ed8fc8ed02e827113f1bcd041202b8d7014c6952210312d19d5027fa7094f644fad5d35d46349adcc08ac69c3bdf2f62b20b6eb8f18921020c37c1efafe5e84a0535e5436547551ad5c6c17c832c11ddb579b76e724e627521034f8136cf717830cb36e95f63864f0bc1aca9b0a14215e44a2698cd162df9e1b753aeffffffff024de4ad020000000017a9147788d956aa1d4d3240934ff22b2d066132a2e34f87bd9f0e00000000001976a91440bd51c2a0449540a14b4668cb05749f0713645a88ac00000000" https://blockchain.runeks.me/publishTx
    b2632e6c65776cb014a37b3ed54243a21ee98df8d89d8a2cf4055c5d2e4e21d8

