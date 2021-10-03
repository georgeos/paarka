import { useState } from 'react'
import { create as ipfsHttpClient } from 'ipfs-http-client'
import axios from 'axios'
import { useRouter } from 'next/router'

export default function CreateItem() {

  const router = useRouter()
  const [fileUrl, setFileUrl] = useState(null)
  const [formInput, updateFormInput] = useState({ price: '', name: '', description: '' })
  const [file, setFile] = useState()
  const [videoUrl , setvideoUrl] = useState(null)

  async function postImage({image}) {
    const formData = new FormData();
    formData.append("image", image)
    const result = await axios.post('http://localhost:3001/images', formData, { headers: {'Content-Type': 'multipart/form-data'}})
    console.log(result.data)
    setvideoUrl(result.data)
    return result.data
  }

  const fileSelected = event => {
    const file = event.target.files[0]
		setFile(file)
  }

  const submit = async event => {
    event.preventDefault()
    await postImage({image: file})
  }

  const client = ipfsHttpClient('https://ipfs.infura.io:5001/api/v0')

  async function onChange(e) {
    const file = e.target.files[0]
    try {
      const added = await client.add(
        file,
        {
          progress: (prog) => console.log(`received: ${prog}`)
        }
      )
      const url = `https://ipfs.infura.io/ipfs/${added.path}`
      setFileUrl(url)
    } catch (error) {
      console.log('Error uploading file: ', error)
    }  
  }
 
  async function createNFT() {
    const { name, description, price } = formInput
    if (!name || !description || !price || !fileUrl) return
    /* first, upload to IPFS */
    const data = JSON.stringify({
      name, description, image: fileUrl
    })
    try {
      const added = await client.add(data)
      const url = `https://ipfs.infura.io/ipfs/${added.path}` 
      // await mintNFT();
      console.log(data, videoUrl);
      console.log("Here we  create the  NFT!! " , url)
    } catch (error) {
      console.log('Error uploading file: ', error)
    }  
  }

  const mintNFT = async () => {
    try {
      const accessToken = window.localStorage.getItem('auth-token') 
      let nft = {
        price: formInput.price,
        name: formInput.name
      }
      await axios.post('http://localhost:3001/api/mint/mint-nft', nft, {
        headers: {
          'auth-token':accessToken
        }
      })
      router.push('/')
    } catch (error){
      console.log(error)
    }
  }

  return (
    <div className="flex justify-center">
      <div className="w-1/2 flex flex-col pb-12">
        <input 
          placeholder="Asset Name"
          className="mt-8 border rounded p-4"
          onChange={e => updateFormInput({ ...formInput, name: e.target.value })}
        />
        {/* <textarea
          placeholder="Asset Description"
          className="mt-2 border rounded p-4"
          onChange={e => updateFormInput({ ...formInput, description: e.target.value })}
        /> */}
        <input
          placeholder="Asset Price in Ada"
          className="mt-2 border rounded p-4"
          onChange={e => updateFormInput({ ...formInput, price: e.target.value })}
        />
        <input
          type="file"
          name="Asset"
          className="my-4"
          onChange={onChange}
        />
        {fileUrl && (<img className="rounded mt-4" width="350" src={fileUrl} />)}
        {<div>  
          <form onSubmit={submit}>
            <input onChange={fileSelected} type="file" accept="video/*"></input>
            <button className={"bg-blue-600 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"} type="submit">Upload video</button>
          </form>
        </div>}
        { videoUrl && (
          <div >
            <header>
            <video controls muted>
                <source src={  `http://localhost:3001/images/${videoUrl}`} type="video/mp4"></source>
            </video>
            </header>
          </div>
        )}
        <button onClick={mintNFT} className="font-bold mt-4 bg-blue-600 hover:bg-blue-700 text-white rounded p-4 shadow-lg">
          Mint NFT
        </button>
      </div>
    </div>
  ) 
}