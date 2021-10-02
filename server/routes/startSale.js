const router = require("express").Router();
const axios = require('axios');

router.get("/start-sale", async (req, res) => {
    // Call to activate contract
    axios({
        method: 'post',
        url: 'http://localhost:9080/api/contract/activate/',
        data: {
            caID: "StartSale",
            caWallet: { getWallet: 2 }
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
                "sCurrency": {"unCurrencySymbol": "b2c19a5d4ffd9de9a0f7d32f8947e9f3c1e3ec931da1147eb34ad04f"},
                "sToken":{"unTokenName":"A"}
            }
        })
        .then(_response => {
            console.log('started sale')
            // If successful call to start/status
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