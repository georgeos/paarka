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

## PubKeyHash (Temporal)

1. Wallet 1: 35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3
2. Wallet 2: 977efb35ab621d39dbeb7274ec7795a34708ff4d25a01a1df04c1f27
3. Wallet 3: 7f8a76c0ebaa4ad20dfdcd51a5de070ab771f4bf377f2c41e6b71c0a
4. Wallet 4: bcc083ade3fdd0a372cb6c43ef00ef02fcb52e9532941117d7609d6a