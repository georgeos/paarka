import { StarIcon } from '@heroicons/react/solid'
import axios from 'axios';
import { useRouter } from 'next/router'
import movies from '../movies.json';

const reviews = { href: '#', average: 4, totalCount: 117 }


function classNames(...classes) {

  return classes.filter(Boolean).join(' ')
}

const Buy = () => {
  const router = useRouter();
  const { id } = router.query;
  const movie = movies.filter(movie => movie.id == id)[0];
  const buyingParams = {
    "unCurrencySymbol": "bde647055ea3261cd03f4b6a5647201e61bc87f96378062e9e45f8fe",
    "unTokenName": "A"
}
  const buyMovie = async event => {
    event.preventDefault();
    await axios.post('http://localhost:3001/api/buy', buyingParams )
  } 

  return (
    <div className="bg-white">
      <div className="pt-6">
        <div className="mt-6 max-w-2xl mx-auto sm:px-6 lg:max-w-7xl lg:px-8 lg:grid lg:grid-cols-3 lg:gap-x-8">
          <iframe width="1200" height="630" src={movie.videoSrc}></iframe>
        </div>

        {/* Product info */}
        <div className="max-w-2xl mx-auto pt-10 pb-16 px-4 sm:px-6 lg:max-w-7xl lg:pt-16 lg:pb-24 lg:px-8 lg:grid lg:grid-cols-3 lg:grid-rows-[auto,auto,1fr] lg:gap-x-8">
          <div className="lg:col-span-2 lg:border-r lg:border-gray-200 lg:pr-8">
            <h1 className="text-2xl font-extrabold tracking-tight text-gray-900 sm:text-3xl">{movie.name}</h1>
          </div>

          <div className="mt-4 lg:mt-0 lg:row-span-3">
            <h2 className="sr-only">Product information</h2>
            <p className="text-3xl text-gray-900">$ {movie.price}</p>

            <div className="mt-6">
              <h3 className="sr-only">Reviews</h3>
              <div className="flex items-center">
                <div className="flex items-center">
                  {[0, 1, 2, 3, 4].map((rating) => (
                    <StarIcon
                      key={rating}
                      className={classNames(
                        reviews.average > rating ? 'text-gray-900' : 'text-gray-200',
                        'h-5 w-5 flex-shrink-0'
                      )}
                      aria-hidden="true"
                    />
                  ))}
                </div>
                <p className="sr-only">{reviews.average} out of 5 stars</p>
                <a href={reviews.href} className="ml-3 text-sm font-medium text-blue-600 hover:text-blue-500">
                  {reviews.totalCount} reviews
                </a>
              </div>
            </div>
            <br/>
            <h3 className="text-lg font-medium leading-6 text-gray-900">Credit card information</h3>
            {/* <form action="#" method="POST"> */}
                <div className="px-0 py-5 bg-white">
                  <div className="grid grid-cols-6 gap-6">
                    <div className="col-span-12 sm:col-span-12">
                      <label htmlFor="card-name" className="block text-sm font-medium text-gray-700">
                        Name
                      </label>
                      <input
                        type="text"
                        name="card-name"
                        id="card-name"
                        autoComplete="given-name"
                        className="mt-1 focus:ring-blue-500 focus:border-blue-500 block w-full shadow-sm sm:text-sm border-gray-300 rounded-md"
                      />
                    </div>

                    <div className="col-span-6 sm:col-span-12">
                      <label htmlFor="card-number" className="block text-sm font-medium text-gray-700">
                        Number
                      </label>
                      <input
                        type="text"
                        name="card-number"
                        id="card-number"
                        autoComplete="email"
                        className="mt-1 focus:ring-blue-500 focus:border-blue-500 block w-full shadow-sm sm:text-sm border-gray-300 rounded-md"
                      />
                    </div>

                    <div className="col-span-4 sm:col-span-4 lg:col-span-4">
                      <label htmlFor="year" className="block text-sm font-medium text-gray-700">
                        Year
                      </label>
                      <input
                        type="text"
                        name="year"
                        id="year"
                        className="mt-1 focus:ring-blue-500 focus:border-blue-500 block w-full shadow-sm sm:text-sm border-gray-300 rounded-md"
                      />
                    </div>

                    <div className="col-span-4 sm:col-span-4 lg:col-span-4">
                      <label htmlFor="month" className="block text-sm font-medium text-gray-700">
                        Month
                      </label>
                      <input
                        type="text"
                        name="month"
                        id="month"
                        className="mt-1 focus:ring-blue-500 focus:border-blue-500 block w-full shadow-sm sm:text-sm border-gray-300 rounded-md"
                      />
                    </div>

                    <div className="col-span-4 sm:col-span-4 lg:col-span-4">
                      <label htmlFor="cvc" className="block text-sm font-medium text-gray-700">
                        CVC
                      </label>
                      <input
                        type="text"
                        name="cvc"
                        id="cvc"
                        autoComplete="cvc"
                        className="mt-1 focus:ring-blue-500 focus:border-blue-500 block w-full shadow-sm sm:text-sm border-gray-300 rounded-md"
                      />
                    </div>
                  </div>
                </div>
              <button onClick={buyMovie}
                type="submit"
                className="mt-2 w-full bg-blue-600 border border-transparent rounded-md py-3 px-8 flex items-center justify-center text-base font-medium text-white hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
              >
                Buy it
              </button>
            {/* </form> */}
          </div>

          <div className="py-10 lg:pt-6 lg:pb-16 lg:col-start-1 lg:col-span-2 lg:border-r lg:border-gray-200 lg:pr-8">
            {/* Description and details */}
            <div>
              <h3 className="sr-only">Storyline</h3>

              <div className="space-y-6">
                <p className="text-base text-gray-900">{movie.storyline}</p>
              </div>
            </div>
            {movie.stars &&
              <div className="mt-10" >
                <h3 className="text-sm font-medium text-gray-900">Stars</h3>

                <div className="mt-4">
                  <ul role="list" className="pl-4 list-disc text-sm space-y-2">
                    {movie.stars.map((star) => (
                      <li key={star.name} className="text-gray-400">
                        <span className="text-gray-600">{star.name}</span>
                      </li>
                    ))}
                  </ul>
                </div>
              </div>
            }
          </div>
        </div>
      </div>
    </div>
  )
}

export default Buy