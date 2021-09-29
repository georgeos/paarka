/* pages/_app.js */
import '../styles/globals.css'
import { Popover } from '@headlessui/react'

const navigation = [
  { name: 'Home', href: '/' },
  { name: 'Movies', href: '/#movies' },
  { name: 'Sell', href: '/create-item' },
  { name: 'My movies', href: '/my-assets' },
]

function MyApp({ Component, pageProps }) {
  return (
    <div>
      <Popover>
        <div className="relative py-6 pt-6 px-4 sm:px-6 lg:px-8">
          <nav className="relative flex items-center justify-between sm:h-12 lg:justify-start" aria-label="Global">
            <div className="flex items-center flex-grow flex-shrink-0 lg:flex-grow-0">
              <div className="flex items-center justify-between w-full md:w-auto">
                <a href="/">
                  <img
                    className="h-12 w-auto sm:h-8"
                    src="paarka.png"
                  />
                </a>
              </div>
            </div>
            <div className="hidden md:block md:ml-10 md:pr-4 md:space-x-8">
              {navigation.map((item) => (
                <a key={item.name} href={item.href} className="font-medium text-gray-500 hover:text-gray-900">
                  {item.name}
                </a>
              ))}
              <a href="#" className="font-medium text-blue-600 hover:text-blue-500">
                Log in
              </a>
            </div>
          </nav>
        </div>
      </Popover>
      <Component {...pageProps} />
    </div>
  )
}

export default MyApp
