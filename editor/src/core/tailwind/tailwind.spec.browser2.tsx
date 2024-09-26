import {
  renderTestEditorWithCode,
  renderTestEditorWithModel,
} from '../../components/canvas/ui-jsx.test-utils'
import { createModifiedProject } from '../../sample-projects/sample-project-utils.test-utils'
import { wait } from '../model/performance-scripts'

const Project = createModifiedProject({
  '/utopia/storyboard.js': `import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      data-uid='scene'
      id='playground-scene'
      commentId='playground-scene'
      className='absolute top-[100px] left-[200px] w-[700px] h-[750px]'
      data-label='Playground'
    >
      <div data-uid='absolute' className='absolute top-[33px] left-[40px] w-[620px] h-[177px] bg-tahiti'>
        <div data-uid='absolute-child' className='absolute top-4 left-4 w-10 h-10 bg-midnight' />
      </div>
      <div data-uid='flex' className='absolute bg-metal left-10 top-[250px] w-[500px] h-max flex flex-row gap-10 px-5 py-[20px] items-center justify-start'>
        <div data-uid='flex-child-1' className='bg-silver w-32 h-32' />
        <div data-uid='flex-child-2' className='bg-silver w-32 h-32' />
      </div>
      <div data-uid='grid' className='absolute left-10 top-[450px] bg-bermuda w-[600px] h-[250px] grid grid-cols-4 grid-rows-4 gap-4'>
        <div data-uid='grid-child-1' className='bg-bubble-gum m-2' />
        <div data-uid='grid-child-2' className='bg-bubble-gum m-2 row-start-2 col-span-3' />
        <div data-uid='grid-child-3' className='bg-bubble-gum m-2 row-start-3 col-start-4' />
        <div data-uid='grid-child-4' className='bg-bubble-gum m-2' />
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
  'tailwind.config.js': `
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
}
export default Tailwind
`,
})

describe('rendering tailwind projects in the editor', () => {
  it('can render absolute positioning props', async () => {
    const editor = await renderTestEditorWithModel(Project, 'await-first-dom-report')

    expect('not implemented').toEqual('implemented')
  })
  // project with absolute positioned elements
  // project with flex layout
  // project with grid layout
  // project with custom colors in the tailwind config
  // project with custom plugin in the tailwind config
})
