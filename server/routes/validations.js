//VALIDATION SCHEMA
const Joi = require('@hapi/joi');

const registerSchema= Joi.object({
    userName: Joi.string()
    .min(6)
    .required(),
    email: Joi.string()
    .min(6)
    .required()
    .email(),
    password: Joi.string()
    .min(6)
    .max(200)
    .required(),
    walletId: Joi.number()
    .required()
});
const loginSchema = Joi.object({
    password: Joi.string()
    .min(6)
    .max(200)
    .required(), 
    email: Joi.string()
    .min(6)
    .required()
    .email()
});
const register =  (req) =>   
     registerSchema.validate (req.body);
const login = (req) => loginSchema.validate(req.body);

module.exports = { register , login}