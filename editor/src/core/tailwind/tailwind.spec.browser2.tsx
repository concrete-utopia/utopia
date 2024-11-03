import { mouseClickAtPoint } from '../../components/canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../../components/canvas/ui-jsx.test-utils'
import { renderTestEditorWithModel } from '../../components/canvas/ui-jsx.test-utils'
import { switchEditorMode } from '../../components/editor/actions/action-creators'
import { EditorModes } from '../../components/editor/editor-modes'
import { StoryboardFilePath } from '../../components/editor/store/editor-state'
import { createModifiedProject } from '../../sample-projects/sample-project-utils.test-utils'
import { setFeatureForBrowserTestsUseInDescribeBlockOnly } from '../../utils/utils.test-utils'
import { windowPoint } from '../shared/math-utils'
import { TailwindConfigPath } from './tailwind-config'
import { Project } from './tailwind.test-utils'

describe('rendering tailwind projects in the editor', () => {
  setFeatureForBrowserTestsUseInDescribeBlockOnly('Tailwind', true)
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

  describe('Remix', () => {
    const projectWithMultipleRoutes = createModifiedProject({
      [StoryboardFilePath]: `import * as React from 'react'
        import { RemixScene, Storyboard } from 'utopia-api'
        
        export var storyboard = (
          <Storyboard data-uid='storyboard'>
            <RemixScene
              className='absolute top-[100px] left-[200px] w-[700px] h-[700px]'
              data-label='Playground'
              data-uid='remix'
            />
          </Storyboard>
        )
        `,
      ['/app/root.js']: `import React from 'react'
        import { Outlet } from '@remix-run/react'
        
        export default function Root() {
          return (
            <div data-testid='root' className='flex flex-col gap-10 bg-red-200 text-2xl'>
              I am Root!
              <Outlet />
            </div>
          )
        }
        `,
      ['/app/routes/_index.js']: `import React from 'react'
        import { Link } from '@remix-run/react'
  
        export default function Index() {
          return (
            <div data-testid='index' className='flex flex-col gap-8'>
              Index page
              <Link to='/about' data-testid='remix-link'>About</Link>
            </div>
          )
        }
        `,
      ['/app/routes/about.js']: `import React from 'react'
  
        export default function About() {
          return (
            <div data-testid='about' className='flex flex-row gap-6 p-4'>
              <span data-testid='about-text' className='text-shadow-md'>About page</span>
            </div>
          )
        }
        `,
      '/src/app.css': `
        @tailwind base;
        @tailwind components;
        @tailwind utilities;
        `,
      [TailwindConfigPath]: `
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
  export default Tailwind`,
    })

    it('can render content in a RemixScene', async () => {
      const editor = await renderTestEditorWithModel(
        projectWithMultipleRoutes,
        'await-first-dom-report',
      )
      {
        const root = editor.renderedDOM.getByTestId('root')
        const { backgroundColor, display, flexDirection, gap, fontSize } = getComputedStyle(root)
        expect({ backgroundColor, display, flexDirection, gap, fontSize }).toEqual({
          backgroundColor: 'rgba(0, 0, 0, 0)',
          display: 'flex',
          flexDirection: 'column',
          fontSize: '24px',
          gap: '40px',
        })
      }
      {
        const index = editor.renderedDOM.getByTestId('index')
        const { display, flexDirection, gap } = getComputedStyle(index)
        expect({ display, flexDirection, gap }).toEqual({
          display: 'flex',
          flexDirection: 'column',
          gap: '32px',
        })
      }
    })
    it('can render content after navigating to a different page', async () => {
      const editor = await renderTestEditorWithModel(
        projectWithMultipleRoutes,
        'await-first-dom-report',
      )
      await switchToLiveMode(editor)
      await clickRemixLink(editor)

      {
        const about = editor.renderedDOM.getByTestId('about')
        const { display, flexDirection, gap, padding } = getComputedStyle(about)
        expect({ display, flexDirection, gap, padding }).toEqual({
          display: 'flex',
          flexDirection: 'row',
          gap: '24px',
          padding: '16px',
        })
      }
      {
        const aboutText = editor.renderedDOM.getByTestId('about-text')
        const { textShadow } = getComputedStyle(aboutText)
        expect(textShadow).toEqual('rgba(0, 0, 0, 0.2) 3px 3px 6px')
      }
    })
  })
})

const switchToLiveMode = (editor: EditorRenderResult) =>
  editor.dispatch([switchEditorMode(EditorModes.liveMode())], true)

async function clickLinkWithTestId(editor: EditorRenderResult, testId: string) {
  const targetElement = editor.renderedDOM.queryAllByTestId(testId)[0]
  const targetElementBounds = targetElement.getBoundingClientRect()

  const clickPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
  await mouseClickAtPoint(targetElement, clickPoint)
}

async function clickRemixLink(editor: EditorRenderResult) {
  await clickLinkWithTestId(editor, 'remix-link')
  await editor.getDispatchFollowUpActionsFinished()
}
