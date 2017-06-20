# RESTful Bitcoin address index server
[![Build Status](https://api.travis-ci.org/runeksvendsen/blockchain-restful-address-index.svg?branch=master)](https://travis-ci.org/runeksvendsen/blockchain-restful-address-index)

Thin RESTful HTTP wrapper for [address-index patched Bitcoin Core](https://github.com/CounterpartyXCP/Documentation/blob/master/Installation/bitcoin_core.md).

### Exposed resources

* **GET** `/outputs/<address>/all` (list all outputs paying to `<address>`)
  * Response body: `Content-Type: application/json` (see schema in example below)
* **GET** `/outputs/<address>/unspent` (list all **unspent** outputs paying to `<address>`)
  * Response body: `Content-Type: application/json` (same schema as `/all`)
* **GET** `/txOutProof/<txid>` (obtain proof that a transaction was included in a block)
  * Response body: `{ 'proof_data' : "<hex-encoded proof data>", 'proof_tx_data' : "<hex-encoded tx data>" }` (`Content-Type: application/json`)
  * Notes: Documentation of proof data format: https://bitcoin.org/en/developer-reference#merkleblock
* **POST** `/publishTx` (publish transaction to the network) 
  * Request body: `{ 'tx_data' : "<hex-encoded transaction>" }` (`Content-Type: application/json`)
  * Response body: `{ 'tx_id' : "<transaction id>" }` (`Content-Type: application/json`)

### Limitations
An output needs at least a single confirmation before it appears in the returned list (it needs to be in a block). However, if a new unconfirmed transaction appears which redeems this output, it will not be included in the list of unspent outputs. In other words, you cannot get information about an output until it has at least one confirmation, but the output will disappear immediately from the list of unspent outputs if a spending transaction appears.

So far, pagination is also unsupported, because I can't get bitcoind to do it: https://github.com/btcdrak/bitcoin/issues/11

### Building
Building the server requires the build tool *stack*, which is available in Ubuntu 16.04 as the `haskell-stack` package. For distributions without the `stack` build tool available, install it [using this command](https://docs.haskellstack.org/en/stable/README/#how-to-install), and remove the `haskell-stack` package from the `apt-get install` line below.

    sudo apt-get update && sudo apt-get install -y autoconf autogen libtool xz-utils git-core haskell-stack
    git clone https://github.com/runeksvendsen/blockchain-restful-address-index.git
    cd blockchain-restful-address-index/
    stack setup && stack build

### Running
First, get *addr-index patched* Bitcoin Core up and running by following [this guide](http://counterparty.io/docs/bitcoin_core/). Regular Bitcoin Core will not suffice, as it only has a *transaction id-to-address* index, and no *address-to-transaction id* index.

The RESTful server executable (`rest-addr`) takes as its only argument the path to a config file. The config file specifies the Bitcoin Core RPC configuration (hostname, port, user, password) and also whether this Bitcoin Core is running on livenet or testnet. Example config files can be found in <a href="config/">config/</a>, which has configuration files for Bitcoin (live) and testnet3 (<a href="config/live/config/server.cfg">config/live/config/server.cfg</a> and <a href="config/test/config/server.cfg">config/test/config/server.cfg</a>, respectfully). 

### Bugs/questions/comments
If you find a bug somewhere - either in the code, documentation or elsewhere - create an issue. Do the same if you have any questions.

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
    
    $ curl --silent -X POST -d "{ \"tx_data\" : \"0100000004387784a109347db68456333dd555ae29308f43af68b43c2c3feb07d40af630bb010000008b483045022100d76ce7b8ce52de584a5f65d498744f3cf38719f93c2e9bb70e17f673d33e483902200e1717aa43cea97a0143d3f7de3b76811706a6cf5fe613d232c4c186c567e025014104f5f1eae6299b2666606556ee8ba8e7bc98e4e7d382137922ce9255723d5844a32338d1bbb39e1fedaf5c6d47ba055e98b6efff0ed8d4ad7258b459f89e21d44efeffffff86c0510ed392c7bfe7f75d928290b505d1b1a5a91e099cf6a73b0c28142cbab8000000006a473044022055d2fa3f6d3ac66f05212305b1b57a8922681015e9c99155d90e324c305cc3bc022050b7e91495b8f10bebddf01130fe8e3328042f742b57561c2303f0f1844898a9012102fa9409446e881c4450c13cb73635564e56a5d7aa2ca4d021feb02534fbfc2108feffffffe586cdb4575f26499016dfda5c21508a0fead4725dddd0bae1f42e225aa6c889010000006a4730440220312af4a1d1e141f5d377da41281b58705c647191701a39e15b3db333f10cdc810220653744b38c38a9ed75f7ec6b0617ee39ed02ab75a82fa1641787c4bd57fff460012103376c866adb5c2a5234af7a5d88cbefaefbb770a39ac84df11f40e9717ace9580feffffffb2b0b65c2307bb9a346ec49adab60339b36a4d36563fbaad89d5eaef0d4679d6000000006a47304402207077e8311cafe54161fd5e0e918174de689a9a520d2b74f6769ee0b6c2cab44d02202d23ce9da048105601893f4efd1f5ba91123b930f0eb039fd51859ae05adf90b0121035f3722b2e100919ec1bb561d6832b44504eff1185679e4b2e2e83c79fa71c3f3feffffff024d7d3806000000001976a914fbc4b1c9cad2e76c31e168aa166319b7f73c959c88acab690f00000000001976a9140e4260e7f4db85a344ce35399e29964a6dfbed7688ac38b90600\" }" -H "Content-Type: application/json" https://blockchain.runeks.me/publishTx
    {
       "tx_id": "4f294af919ab9fe2bb15f2aabd716df223b68399beec531f15635dea70ed3cb3"
    }

### Test servers
Bitcoin: https://blockchain.runeks.me

testnet3: https://blockchaintest.runeks.me
