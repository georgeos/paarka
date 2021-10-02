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
        .then(_response => {
            console.log('minted')
            // If successful call to mint-nft/status
            url = 'http://localhost:9080/api/contract/instance/' + contractInstance + '/status'
            axios(url)
            .then(status => {
                // Still not working, call to status doesn't return observableState, maybe it's too fast to make the call
                console.log(status.data)
                return res.send(status.data)
            })
            .catch(error => {
                return res.send(error)
            })
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