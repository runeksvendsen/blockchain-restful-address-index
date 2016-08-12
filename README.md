# RESTful Bitcoin address index server

Thin RESTful HTTP wrapper for [address-index patched Bitcoin Core](https://github.com/btcdrak/bitcoin/tree/addrindex-0.12).

### Exposed resources

* **GET** `/outputs/<address>/all` (list of all outputs paying to `<address>`)
  * Response body: `Content-Type: application/json`
* **GET** `/outputs/<address>/unspent` (list of all **unspent** outputs paying to `<address>`)
  * Response body: `Content-Type: application/json`
* **POST** `/publishTx` (publish transaction to the network) 
  * Request body: Hex-encoded transaction (`Content-Type: text/plain; charset=utf-8`)
  * Response body: Hex-encoded transaction ID (also `Content-Type: text/plain; charset=utf-8`)

### Limitations
Unspent outputs are not returned until they have at least a single confirmation. However, if a new, unconfirmed transaction redeems an output, this output will not be included in the returned results. In other words, you cannot get information about an unspent output until it has at least one confirmation, but the output will disappear from the return result as a soon as a spending transaction, confirmed or not, appears.

So far, pagination is also unsupported, because I can't get bitcoind to do it: https://github.com/btcdrak/bitcoin/issues/11

### Building
Building the server requires the build tool *stack*, which is available in Ubuntu 16.04 as the `haskell-stack` package. For distributions without the `stack` build tool available, install it [using this command](https://docs.haskellstack.org/en/stable/README/#how-to-install), and remove the `haskell-stack` package from the `apt-get install` line.

    apt-get update && apt-get install -y autoconf autogen libtool xz-utils git-core haskell-stack
    git clone https://github.com/runeksvendsen/blockchain-restful-address-index.git
    cd blockchain-restful-address-index/
    stack setup && stack build

### Running
First, get *addr-index patched* Bitcoin Core up and running by following [this guide](http://counterparty.io/docs/bitcoin_core/). Regular Bitcoin Core will not suffice, as it only has a *transaction id-to-address* index, and no *address-to-transaction id* index. When addr-index Bitcoin Core is done indexing you're ready to start the RESTful address index server.

The server `rest-addr` executable takes as its only argument the path to its config file. The config file specifies the Bitcoin Core RPC configuration (hostname, port, user, password) and also whether this Bitcoin Core is running on livenet or testnet. Example config files can be found in <a href="config/">config/</a>, which has configuration files for Bitcoin (live) and testnet3 (<a href="config/live/config/server.cfg">config/live/config/server.cfg</a> and <a href="config/test/config/server.cfg">config/test/config/server.cfg</a>, respectfully). 

### Example requests
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

