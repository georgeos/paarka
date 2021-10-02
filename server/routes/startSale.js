const router = require("express").Router();
const axios = require('axios');

router.post("/start-sale", async (req, res) => {
    // Call to activate contract
    axios({
        method: 'post',
        url: 'http://localhost:9080/api/contract/activate/',
        data: {
            caID: "StartSale",
            caWallet: { getWallet: 3 }
        }
    })
    .then(response => {
        contractInstance = response.data.unContractInstanceId;
        console.log('activated StartSale contract ' + contractInstance)
        // If successful call to mstart
        axios({
            method: 'post',
            url: 'http://localhost:9080/api/contract/instance/' + contractInstance + '/endpoint/start',
            data: {
                "sCurrency": {
                    "unCurrencySymbol": req.body.unCurrencySymbol
                },
                "sToken": {
                    "unTokenName": req.body.unTokenName
                }
            }
        })
        .then(_response => {
            console.log('started sale')
            // If successful call to start/status
            url = 'http://localhost:9080/api/contract/instance/' + contractInstance + '/status'
            setTimeout(
                function() {
                    axios(url)
                    .then(status => {
                        console.log(status.data)
                        return res.send(status.data)
                    })
                    .catch(error => {
                        return res.send(error)
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