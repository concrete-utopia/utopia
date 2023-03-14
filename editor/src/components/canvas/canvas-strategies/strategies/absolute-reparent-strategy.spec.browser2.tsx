import * as Prettier from 'prettier/standalone'
import { PrettierConfig } from 'utopia-vscode-common'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { FOR_TESTS_setNextGeneratedUids } from '../../../../core/model/element-template-utils.test-utils'
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../../core/model/scene-utils'
import * as EP from '../../../../core/shared/element-path'
import {
  canvasVector,
  offsetRect,
  windowPoint,
  WindowPoint,
} from '../../../../core/shared/math-utils'
import { cmdModifier, emptyModifiers, Modifiers } from '../../../../utils/modifiers'
import { setFeatureForBrowserTests } from '../../../../utils/utils.test-utils'
import { selectComponents } from '../../../editor/actions/meta-actions'
import { NavigatorEntry } from '../../../editor/store/editor-state'
import { CSSCursor } from '../../canvas-types'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { getCursorFromEditor } from '../../controls/select-mode/cursor-component'
import { mouseClickAtPoint, mouseDragFromPointWithDelta } from '../../event-helpers.test-utils'
import {
  EditorRenderResult,
  formatTestProjectCode,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../../ui-jsx.test-utils'
import { AllContentAffectingTypes, ContentAffectingType } from './group-like-helpers'

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

async function dragAlreadySelectedElement(
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
    setFeatureForBrowserTests('Fragment support', true)

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
        expect(draggedElementBounds.y).toEqual(534)
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
    setFeatureForBrowserTests('Fragment support', false)

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
        expect(draggedElementBounds.y).toEqual(534)
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
  // TODO reenable this after conditionals work well with reparent
  xit('renders correctly with ChildrenHider set to hide children', async () => {
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
  xit('renders correctly with ChildrenHider set to show children', async () => {
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

function getRegularNavigatorTargets(entries: Array<NavigatorEntry>): Array<string> {
  return entries
    .filter((t) => t.type === 'REGULAR')
    .map((t) => t.elementPath)
    .map(EP.toString)
}

describe('children-affecting reparent tests', () => {
  setFeatureForBrowserTests('Fragment support', true)
  setFeatureForBrowserTests('Conditional support', true)
  AllContentAffectingTypes.forEach((type) => {
    describe(`Absolute reparent with children-affecting element ${type} in the mix`, () => {
      it('cannot reparent into a children-affecting div', async () => {
        const renderResult = await renderTestEditorWithCode(
          testProjectWithUnstyledDivOrFragment(type),
          'await-first-dom-report',
        )

        const dragDelta = windowPoint({ x: -75, y: 110 })
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

        expect(
          getRegularNavigatorTargets(renderResult.getEditorState().derived.navigatorTargets),
        ).toEqual([
          'utopia-storyboard-uid/scene-aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/children-affecting',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/children-affecting/inner-fragment',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/children-affecting/inner-fragment/child-1',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/children-affecting/inner-fragment/child-2',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/child-3',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/ccc', // <- ccc becomes a child of aaa/bbb, even though it was dragged over the globalFrame of children-affecting
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent',
        ])
      })

      it('drag-to-moving a child of a children-affecting element does not change the parent if the drag starts over the ancestor', async () => {
        const renderResult = await renderTestEditorWithCode(
          testProjectWithUnstyledDivOrFragment(type),
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
          testProjectWithUnstyledDivOrFragment(type),
          'await-first-dom-report',
        )

        const dragDelta = windowPoint({ x: 0, y: -100 })
        await dragElement(renderResult, 'child-2', dragDelta, cmdModifier, null, null)

        await renderResult.getDispatchFollowUpActionsFinished()

        // no reparent have happened
        expect(
          getRegularNavigatorTargets(renderResult.getEditorState().derived.navigatorTargets),
        ).toEqual([
          'utopia-storyboard-uid/scene-aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/children-affecting',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/children-affecting/inner-fragment',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/children-affecting/inner-fragment/child-1',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/child-3',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/ccc',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/child-2', // <- child-2 is now a child of aaa
        ])
      })

      it('is possible to reparent a fragment-child into the parent of the fragment, if the drag starts out of the grandparent bounds', async () => {
        const renderResult = await renderTestEditorWithCode(
          testProjectWithUnstyledDivOrFragment(type),
          'await-first-dom-report',
        )

        const dragDelta = windowPoint({ x: 50, y: 0 })
        await dragElement(renderResult, 'child-1', dragDelta, cmdModifier, null, null)

        await renderResult.getDispatchFollowUpActionsFinished()

        expect(
          getRegularNavigatorTargets(renderResult.getEditorState().derived.navigatorTargets),
        ).toEqual([
          'utopia-storyboard-uid/scene-aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/children-affecting',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/children-affecting/inner-fragment',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/children-affecting/inner-fragment/child-2',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/child-3',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/child-1', // <child-1 is not the direct child of bbb
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/ccc',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent',
        ])
      })
    })

    it(`reparenting the children-affecting ${type} to an absolute parent works`, async () => {
      const renderResult = await renderTestEditorWithCode(
        testProjectWithUnstyledDivOrFragment(type),
        'await-first-dom-report',
      )

      const child1GlobalFrameBefore = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
        EP.fromString(
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/children-affecting/inner-fragment/child-1',
        ),
        renderResult.getEditorState().editor.jsxMetadata,
      )
      const child2GlobalFrameBefore = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
        EP.fromString(
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/children-affecting/inner-fragment/child-2',
        ),
        renderResult.getEditorState().editor.jsxMetadata,
      )

      const targetElement = EP.fromString(
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/children-affecting',
      )
      // selecting the fragment-like parent manually, so that dragElement drags _it_ instead of child-2!
      await renderResult.dispatch(selectComponents([targetElement], false), true)
      const dragDelta = windowPoint({ x: 0, y: 140 })
      await dragAlreadySelectedElement(renderResult, 'child-2', dragDelta, cmdModifier, null, null)

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        getRegularNavigatorTargets(renderResult.getEditorState().derived.navigatorTargets),
      ).toEqual([
        'utopia-storyboard-uid/scene-aaa',
        'utopia-storyboard-uid/scene-aaa/app-entity',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/child-3',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/ccc',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/children-affecting', // <- the fragment-like children-affecting element has been reparented to otherparent, yay!
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/children-affecting/inner-fragment',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/children-affecting/inner-fragment/child-1',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/children-affecting/inner-fragment/child-2',
      ])

      const propsOfFragment =
        renderResult.getEditorState().editor.allElementProps[
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/children-affecting'
        ]
      // the fragment-like element continues to have no style prop
      expect(propsOfFragment?.style == null).toBeTruthy()
      const propsOfInnerFragment =
        renderResult.getEditorState().editor.allElementProps[
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/children-affecting/inner-fragment'
        ]
      // the inner fragment-like element continues to have no style prop
      expect(propsOfInnerFragment?.style == null).toBeTruthy()

      const child1GlobalFrameAfter = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
        EP.fromString(
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/children-affecting/inner-fragment/child-1',
        ),
        renderResult.getEditorState().editor.jsxMetadata,
      )
      const child2GlobalFrameAfter = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
        EP.fromString(
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/children-affecting/inner-fragment/child-2',
        ),
        renderResult.getEditorState().editor.jsxMetadata,
      )

      expect(child1GlobalFrameAfter).toEqual(
        offsetRect(child1GlobalFrameBefore, canvasVector(dragDelta)),
      )
      expect(child2GlobalFrameAfter).toEqual(
        offsetRect(child2GlobalFrameBefore, canvasVector(dragDelta)),
      )
    })
  })
})

function getOpeningTag(type: ContentAffectingType): string {
  switch (type) {
    case 'sizeless-div':
      return `<div data-uid='children-affecting' data-testid='children-affecting'><>`
    case 'fragment':
      return `<React.Fragment data-uid='children-affecting' data-testid='children-affecting'><>`
    case 'conditional':
      return `{ true ? ( <>`
    default:
      const _exhaustiveCheck: never = type
      throw new Error(`Unhandled ContentAffectingType ${JSON.stringify(type)}.`)
  }
}

function getClosingTag(type: ContentAffectingType): string {
  switch (type) {
    case 'sizeless-div':
      return `</></div>`
    case 'fragment':
      return `</></React.Fragment>`
    case 'conditional':
      return `</> ) : null }`
    default:
      const _exhaustiveCheck: never = type
      throw new Error(`Unhandled ContentAffectingType ${JSON.stringify(type)}.`)
  }
}

function testProjectWithUnstyledDivOrFragment(type: ContentAffectingType): string {
  if (type === 'conditional') {
    FOR_TESTS_setNextGeneratedUids([
      'skip1',
      'skip2',
      'skip3',
      'skip4',
      'skip5',
      'skip6',
      'inner-fragment',
      'skip8',
      'skip9',
      'skip10',
      'children-affecting',
    ])
  } else {
    FOR_TESTS_setNextGeneratedUids(['skip1', 'skip2', 'inner-fragment', 'children-affecting'])
  }

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
            left: 30,
            top: 75,
            width: 300,
            height: 100,
          }}
          data-uid='bbb'
        >
          ${getOpeningTag(type)}
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: -20,
                top: 20,
                width: 50,
                height: 30,
              }}
              data-uid='child-1'
              data-testid='child-1'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 100,
                top: 50,
                width: 75,
                height: 40,
              }}
              data-uid='child-2'
              data-testid='child-2'
            />
          ${getClosingTag(type)}
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              width: 30,
              height: 30,
              contain: 'layout',
              position: 'absolute',
              left: 270,
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
            top: 5,
            width: 50,
            height: 30,
          }}
          data-uid='ccc'
          data-testid='ccc'
        />
        <div
          style={{
            backgroundColor: '#0041B3A1',
            position: 'absolute',
            left: 20,
            top: 210,
            width: 250,
            height: 150,
          }}
          data-uid='otherparent'
          data-testid='otherparent'
        />
      </div>
  `)
}
