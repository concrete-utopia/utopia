import { Popover, Transition } from '@headlessui/react'
import React, { Fragment } from 'react'
import { useCDNLink } from '../util/cdnLink'

const mainNavigation = [
  { name: ' ', href: '#' },
  { name: ' ', href: '#' },
  { name: ' ', href: '#' },
  { name: ' ', href: '#' },
  { name: ' ', href: '#' },
  { name: ' ', href: '#' },
  { name: 'Github', href: 'https://github.com/concrete-utopia/utopia' },
  { name: 'Discord', href: 'https://discord.gg/NEEnPKCgzC' },
  {
    name: 'Play with Utopia',
    href: 'https://utopia.app/projects',
    primary: true,
  },
]

export function Menu() {
  return (
    <Popover className='flex-grow'>
      {({ open }) => (
        <>
          <div className='relative px-4 sm:px-6 lg:px-8 flex-grow'>
            <nav className='relative flex items-center justify-between sm:h-10 max-w-6xl m-auto font-body'>
              <HostedImage className='h-8 w-auto sm:h-10' src='/pyramid_small.png' />
              {mainNavigation.map((item, index) => (
                <a
                  key={`main-nav-${index}`}
                  href={item.href}
                  className='hidden md:block font-body text-lg'
                  style={{
                    color: item.primary === true ? '#FFFFFF' : '#383C4A',
                    backgroundColor: item.primary === true ? '#181818' : '#FFFFFF',
                    padding: '6px 20px',
                    borderRadius: 4,
                  }}
                >
                  {item.name}
                </a>
              ))}
              <div className='-mr-2 flex items-center md:hidden'>
                <Popover.Button className='bg-white rounded-md p-2 inline-flex items-center justify-center text-gray-400 hover:text-gray-500 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500'>
                  <span className='sr-only'>Open main menu</span>
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
                    <HostedImage className='h-8 w-auto' src='/pyramid_small.png' alt='' />
                  </div>
                  <div className='-mr-2'>
                    <Popover.Button className='bg-white rounded-md p-2 inline-flex items-center justify-center text-gray-400 hover:text-gray-500 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500'>
                      <span className='sr-only'>Close main menu</span>
                    </Popover.Button>
                  </div>
                </div>
                <div className='px-2 pt-2 pb-3 space-y-1'>
                  {mainNavigation.map((item, index) => (
                    <a
                      key={`panel-nav-${index}`}
                      href={item.href}
                      className='block px-3 py-2 rounded-md text-base font-body text-gray-700 hover:text-gray-900 hover:bg-gray-50'
                    >
                      {item.name}
                    </a>
                  ))}
                </div>
                <a
                  href='#'
                  className='block w-full px-5 py-3 text-center font-body text-indigo-600 bg-gray-50 hover:bg-gray-100'
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

export const MainTitle = React.memo(({ children }: { children: React.ReactNode }) => {
  return (
    <h1
      className='md:text-6xl text-3xl leading-normal tracking-tight font-headline'
      style={{
        color: '#383C4A',
      }}
    >
      {children}
    </h1>
  )
})
MainTitle.displayName = 'MainTitle'

export const Paragraph = React.memo(({ children }: { children: React.ReactNode }) => {
  return (
    <div
      className='md:text-xl text-base md:leading-8 leading-6 font-body pb-4 md:pb-10'
      style={{
        color: '#383C4A',
      }}
    >
      {children}
    </div>
  )
})
Paragraph.displayName = 'Paragraph'

export const EyeButton = () => (
  <div
    style={{
      borderRadius: '50%',
      height: 17,
      width: 17,
      background: 'white',
      border: '1px solid #383C4A',
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'center',
    }}
  >
    <div
      style={{
        borderRadius: '50%',
        height: 8,
        width: 8,
        background: '#383C4A',
      }}
    >
      <div
        style={{
          borderRadius: '50%',
          height: 2,
          width: 2,
          background: 'white',
          position: 'relative',
          left: 4,
          top: 3,
          display: 'block',
        }}
      />
    </div>
  </div>
)

export const GhostBrowser = (props: {
  className?: string
  title?: string
  children: React.ReactNode
}) => (
  <div
    className={props.className}
    style={{
      display: 'flex',
      fontFamily: 'Moderat-Regular',
      fontSize: 11,
      flexDirection: 'column',
      color: 'white',
      border: '1px solid #383C4A',
      borderRadius: 8,
      background: '#FFFFFF',
      overflow: 'hidden',
    }}
  >
    <div
      style={{
        display: 'flex',
        alignItems: 'center',
        borderBottom: '1px solid #383C4A',
        paddingLeft: 8,
        paddingRight: 8,
        height: 32,
        minHeight: 32,
      }}
    >
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          flexGrow: 1,
          gap: 6,
        }}
      >
        <EyeButton />
        <EyeButton />
      </div>

      <div style={{ cursor: 'grab' }}>{props.title}</div>
    </div>

    <div style={{}}>{props.children}</div>
  </div>
)

const contactUsNavigation = [
  { name: 'Play with Utopia', href: 'https://utopia.app/projects' },
  { name: 'Join our Discord', href: 'https://discord.gg/NEEnPKCgzC' },
  { name: 'Check us on Github', href: 'https://github.com/concrete-utopia/utopia' },
  { name: 'Privacy Policy', href: '/policies#privacy-policy' },
  { name: 'Terms and Conditions', href: '/policies' },
]

export const ContactUs = () => (
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
      {contactUsNavigation.map((item, index) => (
        <a
          key={`contact-us-nav-${index}`}
          href={item.href}
          className='block px-6 py-2 rounded-md text-body hover:text-gray-900 hover:bg-gray-50 text-center'
        >
          {item.name}
        </a>
      ))}
    </div>
  </>
)

type HostedImageProps = React.DetailedHTMLProps<
  React.ImgHTMLAttributes<HTMLImageElement>,
  HTMLImageElement
> & { src: string }

export function HostedImage(props: HostedImageProps) {
  const cdnLink = useCDNLink()
  return <img {...props} src={cdnLink(props.src)} />
}

export const BasicEmailSignup = React.memo(() => {
  return (
    <div
      dangerouslySetInnerHTML={{
        __html: `
        <!-- Begin Mailchimp Signup Form -->
        <link href="//cdn-images.mailchimp.com/embedcode/horizontal-slim-10_7.css" rel="stylesheet" type="text/css">
        <div id="mc_embed_signup">
        <form action="https://app.us6.list-manage.com/subscribe/post?u=45910e347a2446abcf18e9b45&amp;id=30e94ed0b5" method="post" id="mc-embedded-subscribe-form" name="mc-embedded-subscribe-form" class="validate" target="_blank" novalidate>
            <div id="mc_embed_signup_scroll">
          <label for="mce-EMAIL" className='font-normal'></label>
          <input type="email" value="" name="EMAIL" class="email" id="mce-EMAIL" placeholder="email address" required>
            <!-- real people should not fill this in and expect good things - do not remove this or risk form bot signups-->
            <div style="position: absolute; left: -5000px;" aria-hidden="true"><input type="text" name="b_45910e347a2446abcf18e9b45_30e94ed0b5" tabindex="-1" value=""></div>
            <div class="clear"><input type="submit" value="Subscribe" name="subscribe" id="mc-embedded-subscribe" class="button"></div>
            </div>
        </form>
        </div>
        <!--End mc_embed_signup-->
      `,
      }}
    />
  )
})

export function Video({ src }: { src: string }) {
  return <video controls autoPlay loop muted playsInline preload={'auto'} src={src} />
}
