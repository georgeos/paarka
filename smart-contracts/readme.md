# Smart contracts

## Configure

1. ```cabal repl```
2. Check your PubKeyHash of Wallet 1 is equal to configured in ```src/Utils.hs```. This is because Wallet 1 is considered as Paarka wallet which signs transactions.
    1. ```import Ledger```
    2. ```import Wallet.Emulator```
    3. ```pubKeyHash $ walletPubKey $ Wallet 1```
3. ```runPaaarka```

## PAB

1. ```cabal run paarka-pab --enable-executable-dynamic```