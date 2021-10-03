const router = require("express").Router();
const { restart } = require("nodemon");
const User = require("../models/User");
// const Wallet = require("../models/Wallet");
const validations = require("./validations");
const bcrypt = require("bcrypt");
const dotenv = require("dotenv");
const jwt = require("jsonwebtoken")
dotenv.config();

let getWalletid = async function(useremail) {
  u = await User.findOne({email: useremail});
  if (u.walletId != null) {
    return u.walletId;
  }

  User.find().sort('walletId').exec(async function (err, model) {
    let wid = 1
    if (model.length != 0) {
      console.log(model[model.length-1]);
      wid = model[model.length-1].walletId + 1;
    }
    return wid;
  });
};

router.post("/register", async (req, res) => {
  //VALIDATION OF THE DATA
  const registerValidation =  validations.register(req);
  // DEPENDING OF THE RESULT OF THE VALIDATION WE DECIDE IF WE UPLOAD TO DB
  if (registerValidation.error) {
    return res.status(400).send(registerValidation.error.details);
  }
  // IF GRAMAR IS OK WE LOOK IF THAT EMAIL IS ALREADY REGISTERED
  const queryUser = await User.findOne({ email: req.body.email });
  // IF EMAIL IS ALREADY IN DB
  if (queryUser) {
    return res.status(400).send("That user email is already registered");
  }
  const salt = await bcrypt.genSalt(10);
  const hashPassword = await bcrypt.hash(req.body.password, salt);

  // wid = await getWalletid(req.body.email);

  const user = new User({
    userName: req.body.userName,
    email: req.body.email,
    password: hashPassword,
    walletId: req.body.walletId
  });
  const savedUser = await user.save();
  return res.send("You register was succesfull");
});

router.post("/login", async (req, res) => {
  //VALIDATION OF GRAMMAR THE DATA
  const loginValidation = validations.login(req);
  // DEPENDING OF THE RESULT OF THE GRAMAR VALIDATION WE DECIDE IF WE LOOK UP IN DB
  if (loginValidation.error) {
    return res.status(400).send(loginValidation.error.details);
  } // IF GRAMMAR IS OK THEN  VALIDATE DE PASSWORD

  const user = await User.findOne({ email: req.body.email });
  if (user) {
      const match = await bcrypt.compare(
        req.body.password,
        user.password
      );
      if (!match) {
        return res.status(400).send("invalid password");
      }
    else if (match) {const token = jwt.sign({_id: user._id }, process.env.TOKEN_SECRET);
    res.header('auth-token', token)
    return res.status(200).send(token)}

    } else return res.status(400).send("Wrong User or Password" );

});

router.post("/edit", async (req, res) => {
  const user = await User.findOne({ email: req.body.email });
  user.walletId = req.body.wallet;
  await user.save();
  console.log(user);
  return res.send("Updated user data");
});


module.exports = router;
