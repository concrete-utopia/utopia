import { renderTestEditorWithModel } from '../../components/canvas/ui-jsx.test-utils'
import { setFeatureForBrowserTestsUseInDescribeBlockOnly } from '../../utils/utils.test-utils'
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
})
