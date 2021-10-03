const router = require("express").Router();
const axios = require('axios');
const startSale = require('../lib/startSale')
const User = require("../models/User");
const jwt = require( "jwt-decode")

router.post("/mint-nft", async (req, res) => {
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
            caID: "MintNFT",
            caWallet: { getWallet: userData.walletId }
        }
    })
    .then(response => {
        contractInstance = response.data.unContractInstanceId;
        console.log('activated ' + contractInstance)
        // If successful call to mint-nft
        axios({
            method: 'post',
            url: 'http://localhost:9080/api/contract/instance/' + contractInstance + '/endpoint/mint-nft',
            data: {
                "unTokenName":req.body.name
            }            
        })
        .then(async _response => {
            console.log('minted')
            // If successful call to mint-nft/status
            url = 'http://localhost:9080/api/contract/instance/' + contractInstance + '/status'
            setTimeout(
                function () {
                    axios(url)
                    .then(status => {
                        console.log(status.data.cicCurrentState.observableState);
                        response = {
                            unCurrencySymbol: status.data.cicCurrentState.observableState[0].unCurrencySymbol,
                            unTokenName: status.data.cicCurrentState.observableState[1].unTokenName
                        }
                        return startSale.startSale(userData, response, res)
                    })
                    .catch(error => {
                        return res.send(error);
                    })
                }
            , 2000);
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