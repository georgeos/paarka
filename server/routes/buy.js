const router = require("express").Router();
const axios = require('axios');
const User = require("../models/User");
const jwt = require( "jwt-decode")

router.post("/", async (req, res) => {
    const token = req.header('auth-token');
    const account = jwt(token)
    const userId = account._id
    const userData = await User.findById (userId)        
    console.log(userData)
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
                        "getPubKeyHash": "977efb35ab621d39dbeb7274ec7795a34708ff4d25a01a1df04c1f27"
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
                    "getPubKeyHash": userData.adress
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