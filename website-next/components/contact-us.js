import { BasicEmailSignup } from './email-signup'


const navigation = [
  { name: 'Create a Project', href: '/project' },
  { name: 'Discord', href: 'https://discord.gg/dSWs79MY' },
  { name: 'Github', href: 'https://github.com/concrete-utopia/utopia' },
]

export const ContactUs = props => (
  <div className='flex justify-between items-center'>
    <BasicEmailSignup />
    {navigation.map((item) => (
      <a
        key={item.name}
        href={item.href}
        className='block px-3 py-2 rounded-md text-body hover:text-gray-900 hover:bg-gray-50'
      >
        {item.name}
      </a>
    ))}
  </div>
)