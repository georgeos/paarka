const router = require('express').Router();
const verifyToken = require('./veryfyToken')
const User = require("../models/User");
const jwt_decode = require( "jwt-decode")




router.get('/restricted', verifyToken , async (req,res) => {
        const token = req.header('auth-token');
        const account = jwt_decode(token)
        const userId = account._id
        const userData = await User.findById (userId)
        res.send(userData)
})

/* router.post('/restricted', verifyToken, async (req,res) =>   { 
        const IdtoQuery = req.body.Id
        const userData = await User.findById (IdtoQuery);  
        //res.send(userData)
        res.send(userData)
       
}  ) */

module.exports = router;