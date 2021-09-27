
const express = require('express')
const multer = require('multer')
const upload = multer({dest:'uploads/'})
//const cors = require('cors')
const fs = require('fs')
const util = require('util')
const unlinkFile = util.promisify(fs.unlink)

const app = express()


const { uploadFile , getFileStream  } = require('./storage')

//app.use(cors)

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


app.listen(port, () => {
  console.log(` app listening at http://localhost:${port}`)
})
