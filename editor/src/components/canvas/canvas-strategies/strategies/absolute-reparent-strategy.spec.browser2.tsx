import {
  EditorRenderResult,
  formatTestProjectCode,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../../ui-jsx.test-utils'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { windowPoint, WindowPoint } from '../../../../core/shared/math-utils'
import { cmdModifier, emptyModifiers, Modifiers } from '../../../../utils/modifiers'
import { PrettierConfig } from 'utopia-vscode-common'
import * as Prettier from 'prettier/standalone'
import {
  BakedInStoryboardVariableName,
  BakedInStoryboardUID,
} from '../../../../core/model/scene-utils'
import { getCursorFromEditor } from '../../controls/select-mode/cursor-component'
import { CSSCursor } from '../../canvas-types'
import { mouseClickAtPoint, mouseDragFromPointWithDelta } from '../../event-helpers.test-utils'
import { setFeatureForTests, wait } from '../../../../utils/utils.test-utils'

interface CheckCursor {
  cursor: CSSCursor | null
}

async function dragElement(
  renderResult: EditorRenderResult,
  targetTestId: string,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
  checkCursor: CheckCursor | null,
  midDragCallback: (() => Promise<void>) | null,
) {
  const targetElement = renderResult.renderedDOM.getByTestId(targetTestId)
  const targetElementBounds = targetElement.getBoundingClientRect()
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
  const combinedMidDragCallback = async () => {
    if (checkCursor != null) {
      expect(getCursorFromEditor(renderResult.getEditorState().editor)).toEqual(checkCursor.cursor)
    }
    if (midDragCallback != null) {
      await midDragCallback()
    }
  }

  await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
  await mouseDragFromPointWithDelta(canvasControlsLayer, startPoint, dragDelta, {
    modifiers: modifiers,
    midDragCallback: combinedMidDragCallback,
  })
}

function getChildrenHiderProjectCode(shouldHide: boolean): string {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
export const ChildrenHider = (props) => {
  return (
    <div data-uid='33d' style={{ ...props.style }}>
      {props.shouldHide ? null : props.children}
    </div>
  )
}
export var ${BakedInStoryboardVariableName} = (
  <Storyboard data-uid='${BakedInStoryboardUID}'>
    <Scene
      style={{
        backgroundColor: 'white',
        position: 'absolute',
        left: 0,
        top: 0,
        width: 400,
        height: 700,
      }}
      data-uid='${TestSceneUID}'
    >
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 0,
          width: 400,
          height: 700,
        }}
        data-uid='outer-div'
      >
        <ChildrenHider
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 41,
            top: 37,
            width: 338,
            height: 144,
            gap: 10,
          }}
          data-uid='children-hider'
          shouldHide={${shouldHide}}
        >
        </ChildrenHider>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            height: 65,
            width: 66,
            position: 'absolute',
            left: 190,
            top: 211,
          }}
          data-uid='child-to-reparent'
          data-testid='child-to-reparent'
        >
          drag me
        </div>
      </div>
    </Scene>
  </Storyboard>
)`
}

describe('Absolute Reparent Strategy', () => {
  it('reparents to the canvas root', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: -1000, y: -1000 })
    await dragElement(renderResult, 'bbb', dragDelta, cmdModifier, null, null)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(
        `
  import * as React from 'react'
  import { Scene, Storyboard, View } from 'utopia-api'

  export var App = (props) => {
    return (
      <div style={{width: '100%', height: '100%'}} data-uid='aaa' />
    )
  }

  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard data-uid='${BakedInStoryboardUID}'>
        <Scene
          style={{ left: 0, top: 0, width: 400, height: 400 }}
          data-uid='${TestSceneUID}'
        >
          <App
            data-uid='${TestAppUID}'
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
          />
        </Scene>
        <div
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: -960, top: -950, width: 200, height: 120 }}
          data-uid='bbb'
          data-testid='bbb'
        />
      </Storyboard>
    )
  }
`,
        PrettierConfig,
      ),
    )
  })
  it('reparents to the canvas root and converts width in percent to px', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: '30%', height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: -1000, y: -1000 })
    await dragElement(renderResult, 'bbb', dragDelta, cmdModifier, null, null)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(
        `
  import * as React from 'react'
  import { Scene, Storyboard, View } from 'utopia-api'

  export var App = (props) => {
    return (
      <div style={{width: '100%', height: '100%'}} data-uid='aaa' />
    )
  }

  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard data-uid='${BakedInStoryboardUID}'>
        <Scene
          style={{ left: 0, top: 0, width: 400, height: 400 }}
          data-uid='${TestSceneUID}'
        >
          <App
            data-uid='${TestAppUID}'
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
          />
        </Scene>
        <div
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: -960, top: -950, width: 120, height: 120 }}
          data-uid='bbb'
          data-testid='bbb'
        />
      </Storyboard>
    )
  }
`,
        PrettierConfig,
      ),
    )
  })
  it('reparents to the canvas root when target parent on the canvas is small', async () => {
    const renderResult = await renderTestEditorWithCode(
      formatTestProjectCode(`
import * as React from 'react'
import { Scene, Storyboard, View } from 'utopia-api'

export var App = (props) => {
  return (<div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
    <div
      style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
      data-uid='bbb'
      data-testid='bbb'
    />
  </div>)
}

export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid='${BakedInStoryboardUID}'>
      <Scene
        style={{ left: 0, top: 0, width: 400, height: 400 }}
        data-uid='${TestSceneUID}'
      >
        <App
          data-uid='${TestAppUID}'
          style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
        />
      </Scene>
      <div
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: -960, top: -950, width: 50, height: 50 }}
        data-uid='ccc'
        data-testid='ccc'
      />
    </Storyboard>
  )
}
`),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: -1000, y: -1000 })
    await dragElement(renderResult, 'bbb', dragDelta, emptyModifiers, null, null)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(
        `
    import * as React from 'react'
    import { Scene, Storyboard, View } from 'utopia-api'

    export var App = (props) => {
      return (<div style={{ width: '100%', height: '100%' }} data-uid='aaa' />)
    }

    export var ${BakedInStoryboardVariableName} = (props) => {
      return (
        <Storyboard data-uid='${BakedInStoryboardUID}'>
          <Scene
            style={{ left: 0, top: 0, width: 400, height: 400 }}
            data-uid='${TestSceneUID}'
          >
            <App
              data-uid='${TestAppUID}'
              style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
            />
          </Scene>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: -960, top: -950, width: 50, height: 50 }}
            data-uid='ccc'
            data-testid='ccc'
          />
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: -960, top: -950, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </Storyboard>
      )
    }
`,
        PrettierConfig,
      ),
    )
  })
  it('does not reparent to ancestor outside of the containing component when the mouse is inside the containing component bounds', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <>  
          <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
            <div
              style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
              data-uid='bbb'
              data-testid='bbb'
            />
          </div>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: -960, top: -950, width: 50, height: 50 }}
            data-uid='ccc'
            data-testid='ccc'
          />
        </>
      `),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: -1000, y: -1000 })
    await dragElement(renderResult, 'bbb', dragDelta, emptyModifiers, null, null)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(
        `
  import * as React from 'react'
  import { Scene, Storyboard, View } from 'utopia-api'

  export var App = (props) => {
    return (
      <>  
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: -960, top: -950, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
        <div
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: -960, top: -950, width: 50, height: 50 }}
          data-uid='ccc'
          data-testid='ccc'
        />
      </>
    )
  }

  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard data-uid='${BakedInStoryboardUID}'>
        <Scene
          style={{ left: 0, top: 0, width: 400, height: 400 }}
          data-uid='${TestSceneUID}'
        >
          <App
            data-uid='${TestAppUID}'
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
          />
        </Scene>
      </Storyboard>
    )
  }
`,
        PrettierConfig,
      ),
    )
  })
  it('reparents to small target parent when cmd is down', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <>  
          <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
            <div
              style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
              data-uid='bbb'
              data-testid='bbb'
            />
          </div>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: -960, top: -950, width: 50, height: 50 }}
            data-uid='ccc'
            data-testid='ccc'
          />
        </>
      `),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: -1000, y: -1000 })
    await dragElement(renderResult, 'bbb', dragDelta, cmdModifier, null, null)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(
        `
  import * as React from 'react'
  import { Scene, Storyboard, View } from 'utopia-api'

  export var App = (props) => {
    return (
      <>  
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa' />
        <div
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: -960, top: -950, width: 50, height: 50 }}
          data-uid='ccc'
          data-testid='ccc'
        >
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      </>
    )
  }

  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard data-uid='${BakedInStoryboardUID}'>
        <Scene
          style={{ left: 0, top: 0, width: 400, height: 400 }}
          data-uid='${TestSceneUID}'
        >
          <App
            data-uid='${TestAppUID}'
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
          />
        </Scene>
      </Storyboard>
    )
  }
`,
        PrettierConfig,
      ),
    )
  })
  it('reparents to target parent element', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <>  
          <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
            <div
              style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
              data-uid='bbb'
              data-testid='bbb'
            />
          </div>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: -960, top: -950, width: 300, height: 300 }}
            data-uid='ccc'
            data-testid='ccc'
          />
        </>
      `),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: -1000, y: -1000 })
    await dragElement(renderResult, 'bbb', dragDelta, emptyModifiers, null, null)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(
        `
  import * as React from 'react'
  import { Scene, Storyboard, View } from 'utopia-api'

  export var App = (props) => {
    return (
      <>  
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa' />
        <div
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: -960, top: -950, width: 300, height: 300 }}
          data-uid='ccc'
          data-testid='ccc'
        >
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      </>
    )
  }

  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard data-uid='${BakedInStoryboardUID}'>
        <Scene
          style={{ left: 0, top: 0, width: 400, height: 400 }}
          data-uid='${TestSceneUID}'
        >
          <App
            data-uid='${TestAppUID}'
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
          />
        </Scene>
      </Storyboard>
    )
  }
`,
        PrettierConfig,
      ),
    )
  })
  it('does not reparent to an element with only text children', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: 200, height: 100 }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 150, width: 200, height: 100 }}
            data-uid='ccc'
          >
            Can't drop here
          </div>
        </div>
      `),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: 0, y: 150 })
    await dragElement(renderResult, 'bbb', dragDelta, cmdModifier, null, null)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(
        `
  import * as React from 'react'
  import { Scene, Storyboard, View } from 'utopia-api'

  export var App = (props) => {
    return (
      <div style={{width: '100%', height: '100%'}} data-uid='aaa'>
        <div
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 200, width: 200, height: 100 }}
          data-uid='bbb'
          data-testid='bbb'
        />
        <div
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 150, width: 200, height: 100 }}
          data-uid='ccc'
        >
          Can't drop here
        </div>
      </div>
    )
  }

  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard data-uid='${BakedInStoryboardUID}'>
        <Scene
          style={{ left: 0, top: 0, width: 400, height: 400 }}
          data-uid='${TestSceneUID}'
        >
          <App
            data-uid='${TestAppUID}'
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
          />
        </Scene>
      </Storyboard>
    )
  }
`,
        PrettierConfig,
      ),
    )
  })
  it('referencing a value from outside the element prevents reparenting', async () => {
    function createCodeForProject(left: number, top: number) {
      return Prettier.format(
        `
  import * as React from 'react'
  import { Scene, Storyboard, View } from 'utopia-api'

  export var App = (props) => {
    const elementWidth = 200
    return (
      <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
        <div
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: ${left}, top: ${top}, width: elementWidth, height: 120 }}
          data-uid='bbb'
          data-testid='bbb'
        />
      </div>
    )
  }

  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard data-uid='${BakedInStoryboardUID}'>
        <Scene
          style={{ left: 0, top: 0, width: 400, height: 400 }}
          data-uid='${TestSceneUID}'
        >
          <App
            data-uid='${TestAppUID}'
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
          />
        </Scene>
      </Storyboard>
    )
  }
`,
        PrettierConfig,
      )
    }

    const renderResult = await renderTestEditorWithCode(
      createCodeForProject(40, 50),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: -1000, y: -1000 })
    await dragElement(renderResult, 'bbb', dragDelta, cmdModifier, null, null)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(createCodeForProject(40, 50))
  })

  describe('with fragments support enabled', () => {
    setFeatureForTests('Fragment support', true)

    it('reparents across from one fragment to within (not directly inside) another', async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(`
import * as React from 'react'
import { Scene, Storyboard, View } from 'utopia-api'

export var App = (props) => {
  return (<div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
    <>
      <div
        style={{ backgroundColor: 'grey', position: 'absolute', left: 50, top: 50, width: 200, height: 200 }}
        data-uid='bbb'
        data-testid='bbb'
      />
    </>
    <>
      <div
        style={{ backgroundColor: 'blue', position: 'absolute', left: 300, top: 300, width: 50, height: 50 }}
        data-uid='ccc'
        data-testid='ccc'
      />
    </>
  </div>)
}

export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid='${BakedInStoryboardUID}'>
      <Scene
        style={{ left: 500, top: 300, width: 400, height: 400 }}
        data-uid='${TestSceneUID}'
      >
        <App
          data-uid='${TestAppUID}'
          style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
        />
      </Scene>
    </Storyboard>
  )
}
`),
        'await-first-dom-report',
      )

      const bbbBounds = (await renderResult.renderedDOM.findByTestId('bbb')).getBoundingClientRect()
      const bbbCenter = {
        x: bbbBounds.x + bbbBounds.width / 2,
        y: bbbBounds.y + bbbBounds.height / 2,
      }

      const cccBounds = (await renderResult.renderedDOM.findByTestId('ccc')).getBoundingClientRect()
      const cccCenter = {
        x: cccBounds.x + cccBounds.width / 2,
        y: cccBounds.y + cccBounds.height / 2,
      }

      const dragDelta = windowPoint({ x: bbbCenter.x - cccCenter.x, y: bbbCenter.y - cccCenter.y })
      await dragElement(renderResult, 'ccc', dragDelta, emptyModifiers, null, async () => {
        const draggedElement = await renderResult.renderedDOM.findByTestId('ccc')
        const draggedElementBounds = draggedElement.getBoundingClientRect()
        expect(draggedElementBounds.x).toEqual(1014)
        expect(draggedElementBounds.y).toEqual(535)
        expect(draggedElementBounds.width).toEqual(50)
        expect(draggedElementBounds.height).toEqual(50)
      })

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        Prettier.format(
          `
import * as React from 'react'
import { Scene, Storyboard, View } from 'utopia-api'

export var App = (props) => {
  return (<div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
    <>
      <div
        style={{ backgroundColor: 'grey', position: 'absolute', left: 50, top: 50, width: 200, height: 200 }}
        data-uid='bbb'
        data-testid='bbb'
      >
        <div
          style={{ backgroundColor: 'blue', position: 'absolute', left: 75, top: 75, width: 50, height: 50 }}
          data-uid='ccc'
          data-testid='ccc'
        />
      </div>
    </>
    <>
    </>
  </div>)
}

export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid='${BakedInStoryboardUID}'>
      <Scene
        style={{ left: 500, top: 300, width: 400, height: 400 }}
        data-uid='${TestSceneUID}'
      >
        <App
          data-uid='${TestAppUID}'
          style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
        />
      </Scene>
    </Storyboard>
  )
}
`,
          PrettierConfig,
        ),
      )
    })
  })

  describe('with fragments support disabled', () => {
    setFeatureForTests('Fragment support', false)

    it('reparents across from one fragment to within (not directly inside) another', async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(`
import * as React from 'react'
import { Scene, Storyboard, View } from 'utopia-api'

export var App = (props) => {
  return (<div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
    <>
      <div
        style={{ backgroundColor: 'grey', position: 'absolute', left: 50, top: 50, width: 200, height: 200 }}
        data-uid='bbb'
        data-testid='bbb'
      />
    </>
    <>
      <div
        style={{ backgroundColor: 'blue', position: 'absolute', left: 300, top: 300, width: 50, height: 50 }}
        data-uid='ccc'
        data-testid='ccc'
      />
    </>
  </div>)
}

export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid='${BakedInStoryboardUID}'>
      <Scene
        style={{ left: 500, top: 300, width: 400, height: 400 }}
        data-uid='${TestSceneUID}'
      >
        <App
          data-uid='${TestAppUID}'
          style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
        />
      </Scene>
    </Storyboard>
  )
}
`),
        'await-first-dom-report',
      )

      const bbbBounds = (await renderResult.renderedDOM.findByTestId('bbb')).getBoundingClientRect()
      const bbbCenter = {
        x: bbbBounds.x + bbbBounds.width / 2,
        y: bbbBounds.y + bbbBounds.height / 2,
      }

      const cccBounds = (await renderResult.renderedDOM.findByTestId('ccc')).getBoundingClientRect()
      const cccCenter = {
        x: cccBounds.x + cccBounds.width / 2,
        y: cccBounds.y + cccBounds.height / 2,
      }

      const dragDelta = windowPoint({ x: bbbCenter.x - cccCenter.x, y: bbbCenter.y - cccCenter.y })
      await dragElement(renderResult, 'ccc', dragDelta, emptyModifiers, null, async () => {
        const draggedElement = await renderResult.renderedDOM.findByTestId('ccc')
        const draggedElementBounds = draggedElement.getBoundingClientRect()
        expect(draggedElementBounds.x).toEqual(1014)
        expect(draggedElementBounds.y).toEqual(535)
        expect(draggedElementBounds.width).toEqual(50)
        expect(draggedElementBounds.height).toEqual(50)
      })

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        Prettier.format(
          `
import * as React from 'react'
import { Scene, Storyboard, View } from 'utopia-api'

export var App = (props) => {
  return (<div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
    <>
      <div
        style={{ backgroundColor: 'grey', position: 'absolute', left: 50, top: 50, width: 200, height: 200 }}
        data-uid='bbb'
        data-testid='bbb'
      >
        <div
          style={{ backgroundColor: 'blue', position: 'absolute', left: 75, top: 75, width: 50, height: 50 }}
          data-uid='ccc'
          data-testid='ccc'
        />
      </div>
    </>
    <>
    </>
  </div>)
}

export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid='${BakedInStoryboardUID}'>
      <Scene
        style={{ left: 500, top: 300, width: 400, height: 400 }}
        data-uid='${TestSceneUID}'
      >
        <App
          data-uid='${TestAppUID}'
          style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
        />
      </Scene>
    </Storyboard>
  )
}
`,
          PrettierConfig,
        ),
      )
    })
  })

  it('renders correctly with ChildrenHider set to hide children', async () => {
    const renderResult = await renderTestEditorWithCode(
      getChildrenHiderProjectCode(true),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: 0, y: -150 })
    await dragElement(
      renderResult,
      'child-to-reparent',
      dragDelta,
      cmdModifier,
      {
        cursor: CSSCursor.NotPermitted,
      },
      null,
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(Object.keys(renderResult.getEditorState().editor.spyMetadata)).toEqual([
      'utopia-storyboard-uid',
      'utopia-storyboard-uid/scene-aaa',
      'utopia-storyboard-uid/scene-aaa/outer-div',
      'utopia-storyboard-uid/scene-aaa/outer-div/children-hider',
    ])
  })
  it('renders correctly with ChildrenHider set to show children', async () => {
    const renderResult = await renderTestEditorWithCode(
      getChildrenHiderProjectCode(false),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: 0, y: -150 })
    await dragElement(
      renderResult,
      'child-to-reparent',
      dragDelta,
      cmdModifier,
      {
        cursor: CSSCursor.Move,
      },
      null,
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(Object.keys(renderResult.getEditorState().editor.spyMetadata)).toEqual([
      'utopia-storyboard-uid',
      'utopia-storyboard-uid/scene-aaa',
      'utopia-storyboard-uid/scene-aaa/outer-div',
      'utopia-storyboard-uid/scene-aaa/outer-div/children-hider',
      'utopia-storyboard-uid/scene-aaa/outer-div/children-hider/child-to-reparent',
    ])
  })
})
;(['div', 'fragment'] as const).forEach((divOrFragment) => {
  describe(`Absolute reparent with children-affecting element ${divOrFragment} in the mix`, () => {
    it('cannot reparent into a children-affecting div', async () => {
      const renderResult = await renderTestEditorWithCode(
        testProjectWithUnstyledDivOrFragment(divOrFragment),
        'await-first-dom-report',
      )

      const dragDelta = windowPoint({ x: -50, y: 250 })
      await dragElement(
        renderResult,
        'ccc',
        dragDelta,
        cmdModifier,
        {
          cursor: CSSCursor.Move,
        },
        null,
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(Object.keys(renderResult.getEditorState().editor.spyMetadata)).toEqual([
        'utopia-storyboard-uid',
        'utopia-storyboard-uid/scene-aaa',
        'utopia-storyboard-uid/scene-aaa/app-entity',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/children-affecting',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/children-affecting/child-1',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/children-affecting/child-2',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/child-3',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/ccc', // <- ccc becomes a child of aaa/bbb, even though it was dragged over the globalFrame of children-affecting
      ])
    })

    it('drag-to-moving a child of a children-affecting element does not change the parent if the drag starts over the ancestor', async () => {
      const renderResult = await renderTestEditorWithCode(
        testProjectWithUnstyledDivOrFragment(divOrFragment),
        'await-first-dom-report',
      )

      const startingElementOrder = Object.keys(renderResult.getEditorState().editor.spyMetadata)

      const dragDelta = windowPoint({ x: 0, y: -50 })
      await dragElement(renderResult, 'child-2', dragDelta, cmdModifier, null, null)

      await renderResult.getDispatchFollowUpActionsFinished()

      // no reparent have happened
      expect(Object.keys(renderResult.getEditorState().editor.spyMetadata)).toEqual(
        startingElementOrder,
      )
    })

    it('drag-to-moving a child of a children-affecting element DOES change the parent if the drag leaves the ancestor', async () => {
      const renderResult = await renderTestEditorWithCode(
        testProjectWithUnstyledDivOrFragment(divOrFragment),
        'await-first-dom-report',
      )

      const dragDelta = windowPoint({ x: 100, y: -250 })
      await dragElement(renderResult, 'child-2', dragDelta, cmdModifier, null, null)

      await renderResult.getDispatchFollowUpActionsFinished()

      // no reparent have happened
      expect(Object.keys(renderResult.getEditorState().editor.spyMetadata)).toEqual([
        'utopia-storyboard-uid',
        'utopia-storyboard-uid/scene-aaa',
        'utopia-storyboard-uid/scene-aaa/app-entity',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/children-affecting',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/children-affecting/child-1',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/child-3',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/ccc',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/child-2', // <- child-2 is now a child of aaa
      ])
    })

    it('is possible to reparent a fragment-child into the parent of the fragment, if the drag starts out of the grandparent bounds', async () => {
      const renderResult = await renderTestEditorWithCode(
        testProjectWithUnstyledDivOrFragment(divOrFragment),
        'await-first-dom-report',
      )

      const dragDelta = windowPoint({ x: 50, y: 0 })
      await dragElement(renderResult, 'child-1', dragDelta, cmdModifier, null, null)

      await renderResult.getDispatchFollowUpActionsFinished()

      // no reparent have happened
      expect(Object.keys(renderResult.getEditorState().editor.spyMetadata)).toEqual([
        'utopia-storyboard-uid',
        'utopia-storyboard-uid/scene-aaa',
        'utopia-storyboard-uid/scene-aaa/app-entity',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/children-affecting',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/children-affecting/child-2',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/child-3',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/child-1', // <- child-1 is now a direct children of bbb
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/ccc',
      ])
    })
  })
})

function testProjectWithUnstyledDivOrFragment(divOrFragment: 'div' | 'fragment'): string {
  return makeTestProjectCodeWithSnippet(`
      <div
        style={{
          height: '100%',
          width: '100%',
          contain: 'layout',
        }}
        data-uid='aaa'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 70,
            top: 148,
            width: 450,
            height: 250,
          }}
          data-uid='bbb'
        >
          ${
            divOrFragment === 'div'
              ? `<div data-uid='children-affecting'>`
              : `<React.Fragment data-uid='children-affecting'>`
          }
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: -30,
                top: 120,
                width: 70,
                height: 50,
              }}
              data-uid='child-1'
              data-testid='child-1'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 150,
                top: 190,
                width: 100,
                height: 60,
              }}
              data-uid='child-2'
              data-testid='child-2'
            />
          ${divOrFragment === 'div' ? `</div>` : `</React.Fragment>`}
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              width: 90,
              height: 62,
              contain: 'layout',
              position: 'absolute',
              left: 210,
              top: 0,
            }}
            data-uid='child-3'
          />
        </div>
        <div
          style={{
            backgroundColor: '#0041B3A1',
            position: 'absolute',
            left: 170,
            top: 40,
            width: 100,
            height: 60,
          }}
          data-uid='ccc'
          data-testid='ccc'
        />
      </div>
  `)
}
