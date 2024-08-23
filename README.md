# Cost Model Test
This repository is meant for testing the `minFeeRefScriptCostPerByte` parameter in the new cost model introduced with the Chang hard fork.

## Steps to run
- use cardano-cli to generate payment address, signing keys and stake keys
  - Example here: https://www.cardano2vn.io/docs/stake-pool-course/handbook/keys-addresses
- fund your payment address with the [sancho faucet](https://sancho.network/faucet/) (you may need to create 2 UTxOs because one may be used for collateral)
- run sancho-node; and export the socket path
    ```sh
    export CARDANO_NODE_SOCKET_PATH=../path-to-your-node-socket
    ```
- export 2 environment variables: 
  ```sh
  export SIGNKEY_FILE = ../path-to-your-signkey-file
  export WALLET_ADDRESS= your-payment-address-in-bech32-format
  ``` 
- to run the test, 
    ```sh
    NETWORK=4 cabal run cost-model/
    ```
- the log associated with the test run will be available in the test-reports directory

> details regarding the verification procedure are available here: [docs/verification-procedure](./docs/verification-procedure.md)