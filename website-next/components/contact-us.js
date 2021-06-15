import { BasicEmailSignup } from './email-signup'


const navigation = [
  { name: 'Play with Utopia', href: '/project' },
  { name: 'Join our Discord', href: 'https://discord.gg/dSWs79MY' },
  { name: 'Check us on Github', href: 'https://github.com/concrete-utopia/utopia' },
]

export const ContactUs = props => (
  <>
    <div
      className='max-w-2xl sm:h-40 md:h-36 mx-auto mb-6 font-body md:text-lg sm:text-sm text-center'
      style={{
        position: 'relative',
        border: '1px solid black',
        padding: 16
      }}>
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
          flexDirection: 'column'
        }}
      >
        <span>Stay in touch. We’ll send you a long-form email once or twice a month <br/>(and only when we have enough new development to write about!)</span>
        <BasicEmailSignup />
      </div>
    </div>
    <div className='font-body md:text-lg sm:text-sm flex justify-center items-center'>
      {navigation.map((item) => (
        <a
          key={item.name}
          href={item.href}
          className='block px-6 py-2 rounded-md text-body hover:text-gray-900 hover:bg-gray-50'
        >
          {item.name}
        </a>
      ))}
    </div>
  </>
)