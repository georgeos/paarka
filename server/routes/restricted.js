const router = require('express').Router();
const verifyToken = require('./veryfyToken')

router.get('/restricted', verifyToken , (req,res) => {
        res.send("ohh babe... you've got into the to VIP Zone!")
})

module.exports = router;