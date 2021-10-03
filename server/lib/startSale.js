const axios = require('axios');

function startSale (user, data, res) {
    axios({
        method: 'post',
        url: 'http://localhost:9080/api/contract/activate/',
        data: {
            caID: "StartSale",
            caWallet: { getWallet: user.walletId }
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
                    "unCurrencySymbol": data.unCurrencySymbol
                },
                "sToken": {
                    "unTokenName": data.unTokenName
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
}

module.exports = { startSale };