import { createModifiedProject } from '../../sample-projects/sample-project-utils.test-utils'
import { TailwindConfigPath } from './tailwind-config'

export const TailwindConfigFileContents = `
const Tailwind = {
    theme: {
      colors: {
        transparent: 'transparent',
        current: 'currentColor',
        white: '#ffffff',
        purple: '#3f3cbb',
        midnight: '#121063',
        metal: '#565584',
        tahiti: '#3ab7bf',
        silver: '#ecebff',
        'bubble-gum': '#ff77e9',
        bermuda: '#78dcca',
      },
    },
    plugins: [
      function ({ addUtilities }) {
        const newUtilities = {
          '.text-shadow': {
            textShadow: '2px 2px 4px rgba(0, 0, 0, 0.1)',
          },
          '.text-shadow-md': {
            textShadow: '3px 3px 6px rgba(0, 0, 0, 0.2)',
          },
          '.text-shadow-lg': {
            textShadow: '4px 4px 8px rgba(0, 0, 0, 0.3)',
          },
          '.text-shadow-none': {
            textShadow: 'none',
          },
        }
  
        addUtilities(newUtilities, ['responsive', 'hover'])
      },
    ],
  }
  export default Tailwind
`

export const Project = createModifiedProject({
  '/utopia/storyboard.js': `import { Scene, Storyboard } from 'utopia-api'
  
  export var storyboard = (
    <Storyboard data-uid='sb'>
      <Scene
        data-uid='scene'
        id='playground-scene'
        commentId='playground-scene'
        className='absolute top-[100px] left-[200px] w-[700px] h-[1050px]'
        data-label='Playground'
      >
        <div data-uid='absolute' data-testid='absolute' className='absolute top-[33px] left-[40px] w-[620px] h-[177px] bg-tahiti'>
          <div data-uid='absolute-child' data-testid='absolute-child' className='absolute top-4 left-4 w-10 h-10 bg-midnight' />
        </div>
        <div data-uid='flex' data-testid='flex' className='absolute bg-metal left-10 top-[250px] w-[500px] h-max flex flex-row gap-10 px-5 py-[30px] items-center justify-start'>
          <div data-uid='flex-child-1' data-testid='flex-child-1' className='bg-silver w-32 h-32' />
          <div data-uid='flex-child-2' data-testid='flex-child-2' className='bg-silver w-32 h-32' />
        </div>
        <div data-uid='grid' data-testid='grid' className='absolute left-10 top-[450px] bg-bermuda w-[600px] h-[250px] grid grid-cols-4 grid-rows-4 gap-4'>
          <div data-uid='grid-child-1' data-testid='grid-child-1' className='bg-bubble-gum m-2' />
          <div data-uid='grid-child-2' data-testid='grid-child-2' className='bg-bubble-gum m-2 row-start-2 col-span-3' />
          <div data-uid='grid-child-3' data-testid='grid-child-3' className='bg-bubble-gum m-2 row-start-3 col-start-4' />
          <div data-uid='grid-child-4' data-testid='grid-child-4' className='bg-bubble-gum m-2' />
        </div>
        <div data-uid='text-shadow' data-testid='text-shadow' className='absolute left-10 top-[750px] bg-bermuda w-[600px] h-[250px] flex flex-col'>
          <span data-uid='text-shadow-1' data-testid='text-shadow-1' className='text-4xl text-shadow'>Text Shadow</span>
          <span data-uid='text-shadow-2' data-testid='text-shadow-2' class='text-3xl text-shadow-md'>This is a medium text shadow example</span>
          <span data-uid='text-shadow-3' data-testid='text-shadow-3' class='text-2xl text-shadow-lg'>This is a large text shadow example</span>
          <span data-uid='text-shadow-4' data-testid='text-shadow-4' class='text-xl text-shadow-none'>This has no text shadow</span>
        </div>
      </Scene>
    </Storyboard>
  )
  `,
  '/src/app.css': `
  @tailwind base;
  @tailwind components;
  @tailwind utilities;
  `,
  [TailwindConfigPath]: TailwindConfigFileContents,
})
