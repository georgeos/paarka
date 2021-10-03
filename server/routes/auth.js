const router = require("express").Router();
const { restart } = require("nodemon");
const User = require("../models/User");
const validations = require("./validations");
const bcrypt = require("bcrypt");
const dotenv = require("dotenv");
const jwt = require("jsonwebtoken")
dotenv.config();

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

  const user = new User({
    userName: req.body.userName,
    email: req.body.email,
    password: hashPassword,
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

module.exports = router;
