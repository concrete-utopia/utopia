import { Fragment } from 'react'
import { Popover, Transition } from '@headlessui/react'
import { MenuIcon, XIcon } from '@heroicons/react/outline'

const navigation = [
  { name: ' ', href: '#' },
  { name: ' ', href: '#' },
  { name: ' ', href: '#' },
  { name: ' ', href: 'https://github.com/concrete-utopia/utopia' },
  { name: 'Create a Project', href: '/project' },
]

export function Menu() {
  return (
    <Popover className='flex-grow'>
      {({ open }) => (
        <>
          <div className='relative px-4 sm:px-6 lg:px-8 flex-grow'>
            <nav className='relative flex items-center justify-between sm:h-10 max-w-6xl m-auto font-menu'>
              <img className='h-8 w-auto sm:h-10' src='/pyramid_fullsize.png' />
              {navigation.map((item) => (
                <a
                  key={item.name}
                  href={item.href}
                  className='hidden md:block font-medium text-gray-900 hover:text-gray-900 text-lg'
                >
                  {item.name}
                </a>
              ))}
              <div className='-mr-2 flex items-center md:hidden'>
                <Popover.Button className='bg-white rounded-md p-2 inline-flex items-center justify-center text-gray-400 hover:text-gray-500 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500'>
                  <span className='sr-only'>Open main menu</span>
                  <MenuIcon className='h-6 w-6' aria-hidden='true' />
                </Popover.Button>
              </div>
            </nav>
          </div>

          <Transition
            show={open}
            as={Fragment}
            enter='duration-150 ease-out'
            enterFrom='opacity-0 scale-95'
            enterTo='opacity-100 scale-100'
            leave='duration-100 ease-in'
            leaveFrom='opacity-100 scale-100'
            leaveTo='opacity-0 scale-95'
          >
            <Popover.Panel
              focus
              static
              className='absolute top-0 inset-x-0 p-2 transition transform origin-top-right md:hidden'
            >
              <div className='rounded-lg shadow-md bg-white ring-1 ring-black ring-opacity-5 overflow-hidden'>
                <div className='px-5 pt-4 flex items-center justify-between'>
                  <div>
                    <img className='h-8 w-auto' src='/pyramid_fullsize.png' alt='' />
                  </div>
                  <div className='-mr-2'>
                    <Popover.Button className='bg-white rounded-md p-2 inline-flex items-center justify-center text-gray-400 hover:text-gray-500 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500'>
                      <span className='sr-only'>Close main menu</span>
                      <XIcon className='h-6 w-6' aria-hidden='true' />
                    </Popover.Button>
                  </div>
                </div>
                <div className='px-2 pt-2 pb-3 space-y-1'>
                  {navigation.map((item) => (
                    <a
                      key={item.name}
                      href={item.href}
                      className='block px-3 py-2 rounded-md text-base font-medium text-gray-700 hover:text-gray-900 hover:bg-gray-50'
                    >
                      {item.name}
                    </a>
                  ))}
                </div>
                <a
                  href='#'
                  className='block w-full px-5 py-3 text-center font-medium text-indigo-600 bg-gray-50 hover:bg-gray-100'
                >
                  Log in
                </a>
              </div>
            </Popover.Panel>
          </Transition>
        </>
      )}
    </Popover>
  )
}
