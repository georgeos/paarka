const S3 = require( "aws-sdk/clients/s3" )
const fs = require('fs')
require('dotenv').config()

const accessKeyId = process.env.ACCES_KEY
const secretAccessKey =  process.env.SECRET_ACCES_KEY 
const endpoint =  process.env.ENDPOINT  
const bucketName = process.env.BUCKET_NAME

const s3 = new S3({
  accessKeyId  ,
  secretAccessKey ,
  endpoint ,
  s3ForcePathStyle: true,
  signatureVersion: "v4",
  connectTimeout: 0,
  httpOptions: { timeout: 0 }
});


const params = {
  Bucket: process.env.BUCKET_NAME
 };

 const getObjets_ =  async   () => { try{
 objets = await  s3.listObjects(params).promise() 
 console.log(objets)}catch(e) {console.log(e)}
 }

// getObjets_();




// GETS A FILE STREAM FORM S3 

const   getFileStream =  function (fileKey) { 
  const downloadParams={
    Bucket: process.env.BUCKET_NAME,
    Key : fileKey
   };
  return  s3.getObject(downloadParams).createReadStream()
}

exports.getFileStream = getFileStream;


// Uploads a file to JSTOR

 const uploadFile =  function (file){

  const fileStream = fs.createReadStream(file.path);
  console.log(file,file.path)

  const uploadParams ={
    Bucket : bucketName,
    Body: fileStream,
    Key: file.filename
  };
  return s3.upload(uploadParams , {
    partSize: 64 * 1024 * 1024
  }).promise()
  
}

exports.uploadFile = uploadFile  




 const createbucket = async () => { try{   
  const { bucket } = await s3.createBucket( params).promise();
  console.log(bucket);
 } catch(err) { 
   console.log(err);
 }
  
};

// createbucket(); 
 


const deletebucket = async () => { try{   
  const { bucket } = await s3.deleteBucket( params).promise();
  console.log(bucket);
 } catch(err) { 
   console.log(err);
 }
  
};
