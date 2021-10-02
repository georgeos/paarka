
const express = require('express')
const multer = require('multer')
const upload = multer({dest:'uploads/'})
const cors = require('cors')
const dotenv = require('dotenv')
const fs = require('fs')
const util = require('util')
const unlinkFile = util.promisify(fs.unlink)
const authRoute = require('./routes/auth');
const mintRoute = require('./routes/mint')
const restrictedRoute = require('./routes/restricted')
const mongoose = require('mongoose')
const app = express()
app.use(express.json());
dotenv.config();


const { uploadFile , getFileStream  } = require('./storage')

app.use(cors())

const port = 3001



app.post('/images', upload.single('image')  ,  async (req, res) => {
  console.log(`this is the ${req.file}`)  
  const file  = req.file
  const description = req.body.description  
  console.log("hey")
  const result = await uploadFile(file)
  console.log(result)
  await unlinkFile(file.path)
  res.send(result.Key ) 
} )

app.get('/images/:key' ,   (req,res ) => {
  const key = req.params.key
  console.log(key)
  const readStream =  getFileStream(key)
  console.log(readStream)
  readStream.pipe(res)
})
mongoose.connect( process.env.DB_CONNECT , {useNewUrlParser : true} ,
  () => console.log( 'connected to DB!'));

app.use('/api/user', authRoute );
app.use('/api/mint', mintRoute );
app.use('/api', restrictedRoute);

app.listen(port, () => {
  console.log(` app listening at http://localhost:${port}`)
})
