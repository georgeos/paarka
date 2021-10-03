import { useState } from 'react'
import axios from 'axios'
import jwt_decode from "jwt-decode";
import React, { useEffect } from "react";
import Router from 'next/router'




function App() {
  
  const [user, updateUser] = useState('')



  async function makeGetRequest() {
    try{
    const access_token = window.localStorage.getItem('auth-token') 
    let res = await axios.get('http://localhost:3001/api/restricted' ,{headers: {
      'auth-token':access_token
         } });       
    let data = res.data;
    console.log(data);  
    } catch (error){console.log(error)
      ; Router.push('/login')
    }
  }
  
  makeGetRequest();




    
  
  return ( 
    
    
    <div >
      <h1>Here is a list with your Token and a movie player</h1>
      <div>
      </div>
            </div>
            
  )  
}

export default App
/* const Profile = () => {
  // Fetch the user client-side
  const { user } = useUser({ redirectTo: '/login' })

  // Server-render loading state
  if (!user || user.isLoggedIn === false) {
    return <Layout>Loading...</Layout>
  }

  // Once the user request finishes, show the user
  return (
    <Layout>
      <h1>Your Profile</h1>
      <pre>{JSON.stringify(user, null, 2)}</pre>
    </Layout>
  )
}

 */