import { useState } from 'react'
import axios from 'axios'

async function postImage({image, description}) {
  const formData = new FormData();
  formData.append("image", image)
  formData.append("description", description)

  const result = await axios.post('http://localhost:3001/images', formData, { headers: {'Content-Type': 'multipart/form-data'}})
  return result.data
}

const movies = [
  {
    id: 1,
    name: 'Bad sport',
    href: '#',
    videoSrc: 'https://www.youtube.com/embed/IwQcnDT7xT0',
    imageAlt: "True crime and sports intersect in a docuseries that examines global controversies and scandals with firsthand accounts from those involved.",
    price: '$5'
  },
  {
    id: 2,
    name: 'Halloween Kills Final',
    href: '#',
    videoSrc: 'https://www.youtube.com/embed/pTG3sbBi54M',
    imageAlt: "Check out the Halloween Kills Official Trailer starring Jamie Lee Curtis! Let us know what you think in the comments below.",
    price: '$4'
  },
  {
    id: 3,
    name: 'Marvel\'s The Avengers',
    href: '#',
    videoSrc: 'https://www.youtube.com/embed/eOrNdBpGMv8',
    imageAlt: "Watch the first trailer for Marvel\'s The Avengers, in theaters May 4, 2012.",
    price: '$7'
  },
  {
    id: 4,
    name: 'Forrest Gump',
    href: '#',
    videoSrc: 'https://www.youtube.com/embed/bLvqoHBptjg',
    imageAlt: "Stupid is as stupid does, says Forrest Gump (played by Tom Hanks in an Oscar-winning performance) as he discusses his relative level of intelligence with a stranger while waiting for a bus. Despite his sub-normal IQ, Gump leads a truly charmed life, with a ringside seat for many of",
    price: '$4'
  },
  {
    id: 5,
    name: 'Halloween Kills Final',
    href: '#',
    videoSrc: 'https://www.youtube.com/embed/pTG3sbBi54M',
    imageAlt: "Check out the Halloween Kills Official Trailer starring Jamie Lee Curtis! Let us know what you think in the comments below.",
    price: '$4'
  },
  {
    id: 6,
    name: 'Halloween Kills Final',
    href: '#',
    videoSrc: 'https://www.youtube.com/embed/pTG3sbBi54M',
    imageAlt: "Check out the Halloween Kills Official Trailer starring Jamie Lee Curtis! Let us know what you think in the comments below.",
    price: '$4'
  },
]


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
                  <h3 className="text-xl font-extrabold">
                    <a href={movie.href}>
                      <span aria-hidden="true" className="absolute inset-0" />
                      {movie.name}
                    </a>
                  </h3>
                </div>
                <div className="inline-flex rounded-md shadow">
                  <a
                    href="#movies"
                    className="inline-flex items-center justify-center px-5 py-3 border border-transparent text-base font-medium rounded-md text-white bg-blue-600"
                  > Buy {movie.price}
                  </a>
                </div>
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

