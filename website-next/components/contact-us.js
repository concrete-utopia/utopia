import React, { BasicEmailSignup } from './email-signup'

const navigation = [
  { name: 'Play with Utopia', href: 'https://utopia.app/projects' },
  { name: 'Join our Discord', href: 'https://discord.gg/NEEnPKCgzC' },
  { name: 'Check us on Github', href: 'https://github.com/concrete-utopia/utopia' },
  { name: 'Privacy Policy', href: '/policies#privacy-policy' },
  { name: 'Terms and Conditions', href: '/policies' },
]

export const ContactUs = (props) => (
  <>
    <div
      className='max-w-2xl h-52 md:h-36 mx-auto mb-6 font-body md:text-lg text-sm text-center'
      style={{
        position: 'relative',
        border: '1px solid black',
        padding: 16,
      }}
    >
      <div
        style={{
          position: 'absolute',
          left: 10,
          right: -10,
          top: 10,
          bottom: -10,
          border: '1px solid black',
          background: 'white',
          padding: 14,
          display: 'flex',
          flexDirection: 'column',
        }}
      >
        <span>
          Stay in touch. We’ll send you a long-form email once or twice a month <br />
          (and only when we have enough new development to write about!)
        </span>
        <BasicEmailSignup />
      </div>
    </div>
    <div className='font-body md:text-lg text-sm flex justify-center items-center'>
      {navigation.map((item) => (
        <a
          key={item.name}
          href={item.href}
          className='block px-6 py-2 rounded-md text-body hover:text-gray-900 hover:bg-gray-50 text-center'
          onMouseDown={() =>
            gtag('event', 'navigate', { category: 'links', label: item.href, value: 1 })
          }
        >
          {item.name}
        </a>
      ))}
    </div>
  </>
)
