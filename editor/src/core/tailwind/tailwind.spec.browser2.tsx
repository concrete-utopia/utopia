import { renderTestEditorWithModel } from '../../components/canvas/ui-jsx.test-utils'
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
`,
})

describe('rendering tailwind projects in the editor', () => {
  it('can render absolute positioning classes', async () => {
    const editor = await renderTestEditorWithModel(Project, 'await-first-dom-report')

    {
      const absolute = editor.renderedDOM.getByTestId('absolute')
      const { position, top, left, width, height, backgroundColor } = getComputedStyle(absolute)
      expect({ position, top, left, width, height, backgroundColor }).toEqual({
        backgroundColor: 'rgb(58, 183, 191)',
        height: '177px',
        left: '40px',
        position: 'absolute',
        top: '33px',
        width: '620px',
      })
    }
    {
      const absoluteChild = editor.renderedDOM.getByTestId('absolute-child')
      const { position, top, left, width, height, backgroundColor } =
        getComputedStyle(absoluteChild)
      expect({ position, top, left, width, height, backgroundColor }).toEqual({
        backgroundColor: 'rgb(18, 16, 99)',
        height: '40px',
        left: '16px',
        position: 'absolute',
        top: '16px',
        width: '40px',
      })
    }
  })

  it('can render flex positioning classes', async () => {
    const editor = await renderTestEditorWithModel(Project, 'await-first-dom-report')

    const flex = editor.renderedDOM.getByTestId('flex')
    const { display, flexDirection, justifyContent, alignItems, gap, padding } =
      getComputedStyle(flex)
    expect({ display, flexDirection, justifyContent, alignItems, gap, padding }).toEqual({
      alignItems: 'center',
      display: 'flex',
      flexDirection: 'row',
      gap: '40px',
      justifyContent: 'flex-start',
      padding: '30px 20px',
    })
  })

  it('can render grid positioning classes', async () => {
    const editor = await renderTestEditorWithModel(Project, 'await-first-dom-report')

    const grid = editor.renderedDOM.getByTestId('grid')
    const { display, gridTemplateColumns, gridTemplateRows, gap } = getComputedStyle(grid)
    expect({ display, gridTemplateColumns, gridTemplateRows, gap }).toEqual({
      display: 'grid',
      gap: '16px',
      gridTemplateColumns: '138px 138px 138px 138px',
      gridTemplateRows: '50.5px 50.5px 50.5px 50.5px',
    })

    {
      const gridChild2 = editor.renderedDOM.getByTestId('grid-child-2')
      const { gridColumnStart, gridColumnEnd, gridRowStart, gridRowEnd, backgroundColor } =
        getComputedStyle(gridChild2)
      expect({ gridColumnStart, gridColumnEnd, gridRowStart, gridRowEnd, backgroundColor }).toEqual(
        {
          backgroundColor: 'rgb(255, 119, 233)',
          gridColumnEnd: 'span 3',
          gridColumnStart: 'span 3',
          gridRowEnd: 'auto',
          gridRowStart: '2',
        },
      )
    }

    {
      const gridChild3 = editor.renderedDOM.getByTestId('grid-child-3')
      const { gridColumnStart, gridColumnEnd, gridRowStart, gridRowEnd, backgroundColor } =
        getComputedStyle(gridChild3)
      expect({ gridColumnStart, gridColumnEnd, gridRowStart, gridRowEnd, backgroundColor }).toEqual(
        {
          backgroundColor: 'rgb(255, 119, 233)',
          gridColumnEnd: 'auto',
          gridColumnStart: '4',
          gridRowEnd: 'auto',
          gridRowStart: '3',
        },
      )
    }
  })

  it('can render classes added from custom plugins', async () => {
    const editor = await renderTestEditorWithModel(Project, 'await-first-dom-report')

    {
      const textShadow1 = editor.renderedDOM.getByTestId('text-shadow-1')
      const { textShadow, fontSize, lineHeight } = getComputedStyle(textShadow1)
      expect({ textShadow, fontSize, lineHeight }).toEqual({
        fontSize: '36px',
        lineHeight: '40px',
        textShadow: 'rgba(0, 0, 0, 0.1) 2px 2px 4px',
      })
    }
    {
      const textShadow2 = editor.renderedDOM.getByTestId('text-shadow-2')
      const { textShadow, fontSize, lineHeight } = getComputedStyle(textShadow2)
      expect({ textShadow, fontSize, lineHeight }).toEqual({
        fontSize: '30px',
        lineHeight: '36px',
        textShadow: 'rgba(0, 0, 0, 0.2) 3px 3px 6px',
      })
    }
    {
      const textShadow3 = editor.renderedDOM.getByTestId('text-shadow-3')
      const { textShadow, fontSize, lineHeight } = getComputedStyle(textShadow3)
      expect({ textShadow, fontSize, lineHeight }).toEqual({
        fontSize: '24px',
        lineHeight: '32px',
        textShadow: 'rgba(0, 0, 0, 0.3) 4px 4px 8px',
      })
    }
    {
      const textShadow4 = editor.renderedDOM.getByTestId('text-shadow-4')
      const { textShadow, fontSize, lineHeight } = getComputedStyle(textShadow4)
      expect({ textShadow, fontSize, lineHeight }).toEqual({
        fontSize: '20px',
        lineHeight: '28px',
        textShadow: 'none',
      })
    }
  })
})
