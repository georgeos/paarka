const router = require("express").Router();
const axios = require('axios');

router.post("/", async (req, res) => {
    // Call to activate contract
    axios({
        method: 'post',
        url: 'http://localhost:9080/api/contract/activate/',
        data: {
            caID: "Buy",
            caWallet: { getWallet: 1 }
        }
    })
    .then(response => {
        contractInstance = response.data.unContractInstanceId;
        console.log('activated ' + contractInstance)
        // If successful call to buy
        axios({
            method: 'post',
            url: 'http://localhost:9080/api/contract/instance/' + contractInstance + '/endpoint/buy',
            data: {
                "nftSale": {
                    "ownerPkh": {
                        "getPubKeyHash": "7f8a76c0ebaa4ad20dfdcd51a5de070ab771f4bf377f2c41e6b71c0a"
                    },
                    "currency": {
                        "unCurrencySymbol": req.body.unCurrencySymbol
                    },
                    "token": {
                        "unTokenName": req.body.unTokenName
                    }
                },
                "amt": 10,
                "buyerPkh": { 
                    "getPubKeyHash": "bcc083ade3fdd0a372cb6c43ef00ef02fcb52e9532941117d7609d6a"
                }
            }                    
        })
        .then(_response => {
            console.log('purchase completed')
            return res.send({ status: "ok" })
        })
        .catch(error => {
            return res.send(error)
        });
    })
    .catch(error => {
        return res.send(error)
    });
});

module.exports = router;