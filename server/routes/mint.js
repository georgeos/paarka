const router = require("express").Router();
const axios = require('axios');

router.get("/mint-nft", async (req, res) => {
    // Call to activate contract
    axios({
        method: 'post',
        url: 'http://localhost:9080/api/contract/activate/',
        data: {
            caID: "MintNFT",
            caWallet: { getWallet: 3 }
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
                "unTokenName":"A"
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
                        return res.send(response);
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