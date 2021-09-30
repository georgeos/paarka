import axios from 'axios';
import movies from './movies.json';
import Link from 'next/link'

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
      <div className="bg-blue-600">
        <div className="max-w-7xl mx-auto py-12 px-4 sm:px-6 lg:py-48 lg:px-8 lg:flex lg:items-center lg:justify-between">
          <h2 className="font-extrabold tracking-tight text-gray-900 sm:text-4xl">
            <span className="text-3xl block text-gray-300">Ready to watch movies?</span>
            <span className="text-4xl block text-white">Our platform makes it easy.</span>
          </h2>
          <div className="mt-8 flex lg:mt-0 lg:flex-shrink-0">
            <div className="inline-flex rounded-md shadow">
              <a
                href="#movies"
                className="inline-flex items-center justify-center px-5 py-3 border border-transparent text-base font-medium rounded-md text-blue-600 bg-white"
              >
                Get started
              </a>
            </div>
            <div className="ml-3 inline-flex rounded-md shadow">
              <a
                href=""
                className="inline-flex items-center justify-center px-5 py-3 border border-white text-base font-medium rounded-md text-white bg-blue-600"
              >
                Learn more
              </a>
            </div>
          </div>
        </div>
      </div>
      <div id={'movies'} className="max-w-2xl mx-auto py-16 px-4 sm:py-24 sm:px-6 lg:max-w-7xl lg:px-8">
        <h2 className="text-3xl font-extrabold tracking-tight text-blue-600 sm:text-4xl">
          <span className="block">Our movies</span>
        </h2>
        <div className="mt-6 grid grid-cols-1 gap-y-10 gap-x-6 sm:grid-cols-2 lg:grid-cols-2 xl:gap-x-8">
          {movies.map((movie) => (
            <div key={movie.id} className="group relative">
              <iframe width="600" height="315" src={movie.videoSrc}></iframe>
              <div className="mt-4 flex justify-between">
                <div>
                  <h3 className="text-xl font-extrabold">{movie.name}</h3>
                </div>
                <Link href="/buy/[id]" as={`/buy/${movie.id}`}
                    >
                  <a
                    className="inline-flex items-center justify-center px-5 py-3 border border-transparent text-base font-medium rounded-md text-white bg-blue-600"
                  >
                    Buy ${movie.price}
                  </a>
                </Link>
              </div>
            </div>
          ))}
        </div>
      </div>
        {/* <header>
        <video controls muted>
            <source src={"http://localhost:3001/images/7811c3e6392cab135d9373f4e5ff689b"} type="video/mp4"></source>
        </video>
        </header> */}
    </div>
    
  );
   
}

export default App;

