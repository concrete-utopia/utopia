import { BasicEmailSignup } from './email-signup'


const navigation = [
  { name: 'Play with Utopia', href: '/project' },
  { name: 'Join our Discord', href: 'https://discord.gg/dSWs79MY' },
  { name: 'Check us on Github', href: 'https://github.com/concrete-utopia/utopia' },
]

export const ContactUs = props => (
  <>
    <div className='pb-6 font-body'>
      <BasicEmailSignup />
    </div>
    <div className='font-body text-lg flex justify-center items-center'>
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