import { useState } from 'react'
import axios from 'axios'

async function postImage({image, description}) {
  const formData = new FormData();
  formData.append("image", image)
  formData.append("description", description)

  const result = await axios.post('http://localhost:3001/images', formData, { headers: {'Content-Type': 'multipart/form-data'}})
  return result.data
}




function App() {

  

  return (
    
     <div >
            <header>
            <video controls muted>
                <source src={"http://localhost:3001/images/7811c3e6392cab135d9373f4e5ff689b"} type="video/mp4"></source>
            </video>
            </header>
        </div>
    
  );
   
}

export default App;

