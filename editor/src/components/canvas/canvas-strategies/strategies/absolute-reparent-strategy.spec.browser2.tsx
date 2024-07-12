import * as Prettier from 'prettier/standalone'
import { setRightMenuTab } from '../../../../components/editor/actions/action-creators'
import { difference, getSingleValueOnly } from '../../../../core/shared/set-utils'
import { PrettierConfig } from 'utopia-vscode-common'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { FOR_TESTS_setNextGeneratedUids } from '../../../../core/model/element-template-utils.test-utils'
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../../core/model/scene-utils'
import * as EP from '../../../../core/shared/element-path'
import type { WindowPoint } from '../../../../core/shared/math-utils'
import {
  canvasVector,
  offsetPoint,
  offsetRect,
  windowPoint,
} from '../../../../core/shared/math-utils'
import type { Modifiers } from '../../../../utils/modifiers'
import { cmdModifier } from '../../../../utils/modifiers'
import { selectComponents } from '../../../editor/actions/meta-actions'
import { RightMenuTab, navigatorEntryToKey } from '../../../editor/store/editor-state'
import { CSSCursor } from '../../canvas-types'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { getCursorFromEditor } from '../../controls/select-mode/cursor-component'
import {
  mouseClickAtPoint,
  mouseDoubleClickAtPoint,
  mouseDownAtPoint,
  mouseDragFromPointToPoint,
  mouseDragFromPointToPointNoMouseDown,
  mouseDragFromPointWithDelta,
  mouseMoveToPoint,
  pressKey,
} from '../../event-helpers.test-utils'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import {
  formatTestProjectCode,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from '../../ui-jsx.test-utils'
import type { FragmentLikeType } from './fragment-like-helpers'
import { AllFragmentLikeTypes } from './fragment-like-helpers'
import {
  getClosingFragmentLikeTag,
  getOpeningFragmentLikeTag,
  getRegularNavigatorTargets,
} from './fragment-like-helpers.test-utils'
import { queryHelpers } from '@testing-library/react'
import { forceNotNull } from '../../../../core/shared/optional-utils'
import { getDomRectCenter } from '../../../../core/shared/dom-utils'
import {
  boundingClientRectToCanvasRectangle,
  selectComponentsForTest,
  wait,
} from '../../../../utils/utils.test-utils'
import CanvasActions from '../../canvas-actions'

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

  await mouseDownAtPoint(canvasControlsLayer, startPoint)

  await mouseDragFromPointToPointNoMouseDown(
    canvasControlsLayer,
    startPoint,
    offsetPoint(startPoint, dragDelta),
    {
      modifiers: modifiers,
      midDragCallback: combinedMidDragCallback,
    },
  )
}

function getElementByDataUID(renderResult: EditorRenderResult, dataUID: string): HTMLElement {
  const queryByDataUID = queryHelpers.queryByAttribute.bind(null, 'data-uid')
  return forceNotNull(
    `Could not find element with ${dataUID}`,
    queryByDataUID(renderResult.renderedDOM.container, dataUID),
  )
}

async function dragElementByDataUID(
  renderResult: EditorRenderResult,
  targetDataId: string,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
  checkCursor: CheckCursor | null,
  midDragCallback: (() => Promise<void>) | null,
) {
  const targetElement = getElementByDataUID(renderResult, targetDataId)
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

describe('Absolute Reparent Strategy', () => {
  it('reparents newly inserted elements', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa' data-testid='aaa' />
      `),
      'await-first-dom-report',
    )
    const targetElement = renderResult.renderedDOM.getByTestId('aaa')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startingMetadataKeys = new Set(
      Object.keys(renderResult.getEditorState().editor.jsxMetadata),
    )

    await renderResult.dispatch([setRightMenuTab(RightMenuTab.Insert)], false)

    // Insert first div.
    await pressKey('d')

    const firstInsertionPoint = {
      x: targetElementBounds.x + 50,
      y: targetElementBounds.y + 50,
    }

    await mouseMoveToPoint(canvasControlsLayer, firstInsertionPoint, { modifiers: cmdModifier })
    await mouseClickAtPoint(canvasControlsLayer, firstInsertionPoint, { modifiers: cmdModifier })

    await renderResult.getDispatchFollowUpActionsFinished()
    const afterFirstInsertMetadataKeys = new Set(
      Object.keys(renderResult.getEditorState().editor.jsxMetadata),
    )
    const pathOfFirstNewlyInsertedElement = getSingleValueOnly(
      difference(afterFirstInsertMetadataKeys, startingMetadataKeys),
    )
    const uidOfFirstNewlyInsertedElement = EP.toStaticUid(
      renderResult.getEditorState().editor.jsxMetadata[pathOfFirstNewlyInsertedElement].elementPath,
    )
    const firstInsertedElement = getElementByDataUID(renderResult, uidOfFirstNewlyInsertedElement)
    const firstElementBounds = firstInsertedElement.getBoundingClientRect()
    const firstElementCenter = getDomRectCenter(firstElementBounds)

    // Insert second div.
    await pressKey('d')

    const secondInsertionPoint = {
      x: targetElementBounds.x + 200,
      y: targetElementBounds.y + 200,
    }

    await mouseMoveToPoint(canvasControlsLayer, secondInsertionPoint, { modifiers: cmdModifier })
    await mouseClickAtPoint(canvasControlsLayer, secondInsertionPoint, { modifiers: cmdModifier })

    await renderResult.getDispatchFollowUpActionsFinished()
    const afterSecondInsertMetadataKeys = new Set(
      Object.keys(renderResult.getEditorState().editor.jsxMetadata),
    )
    const pathOfSecondNewlyInsertedElement = getSingleValueOnly(
      difference(afterSecondInsertMetadataKeys, afterFirstInsertMetadataKeys),
    )
    const uidOfSecondNewlyInsertedElement = EP.toStaticUid(
      renderResult.getEditorState().editor.jsxMetadata[pathOfSecondNewlyInsertedElement]
        .elementPath,
    )
    const secondInsertedElement = getElementByDataUID(renderResult, uidOfSecondNewlyInsertedElement)
    const secondElementBounds = secondInsertedElement.getBoundingClientRect()
    const secondElementCenter = getDomRectCenter(secondElementBounds)

    // Reparent second div into first div.
    const dragDelta = windowPoint({
      x: firstElementCenter.x - secondElementCenter.x,
      y: firstElementCenter.y - secondElementCenter.y,
    })
    await dragElementByDataUID(
      renderResult,
      uidOfSecondNewlyInsertedElement,
      dragDelta,
      cmdModifier,
      null,
      null,
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(
        `
  import * as React from 'react'
  import { Scene, Storyboard, View, Group } from 'utopia-api'

  export var App = (props) => {
    return (
      <div style={{width: '100%', height: '100%', contain: 'layout'}} data-uid='aaa' data-testid='aaa'>
        <div                                                                                                        
          style={{                                                                                                  
            backgroundColor: '#aaaaaa33',                                                                           
            position: 'absolute',                                                                                           
            left: 1,                                                                                                
            top: 1,                                                                                                 
            width: 100,                                                                                             
            height: 100,                                                                                            
          }}                                                                                                        
          data-uid='${uidOfFirstNewlyInsertedElement}'                                                                                            
        >
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 0,
              top: 0,
              width: 100,
              height: 100, 
            }}
            data-uid='${uidOfSecondNewlyInsertedElement}'
          />
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
    await dragElement(renderResult, 'bbb', dragDelta, cmdModifier, null, async () => {
      await renderResult.dispatch(
        [CanvasActions.setUsersPreferredStrategy('ABSOLUTE_REPARENT')],
        true,
      )

      expect(getRegularNavigatorTargets(renderResult)).toEqual([
        'utopia-storyboard-uid/scene-aaa',
        'utopia-storyboard-uid/scene-aaa/app-entity',
        'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
        'utopia-storyboard-uid/bbb', // only appears in the new target even mid-drag, no ghost element
      ])
    })

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(
        `
  import * as React from 'react'
  import { Scene, Storyboard, View, Group } from 'utopia-api'

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
  it('reparents when missing horizontal or vertical props without starting from zero', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ position: 'absolute', left: 100, top: 100, width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', top: 50 }}
            data-uid='bbb'
            data-testid='bbb'
          >
            I'm missing horizontal props
          </div>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50 }}
            data-uid='ccc'
            data-testid='ccc'
          >
            I'm missing vertical props
          </div>
        </div>
      `),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: 400, y: 400 })
    await dragElement(renderResult, 'bbb', dragDelta, cmdModifier, null, async () =>
      renderResult.dispatch([CanvasActions.setUsersPreferredStrategy('ABSOLUTE_REPARENT')], true),
    )
    await dragElement(renderResult, 'ccc', dragDelta, cmdModifier, null, async () =>
      renderResult.dispatch([CanvasActions.setUsersPreferredStrategy('ABSOLUTE_REPARENT')], true),
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(
        `
  import * as React from 'react'
  import { Scene, Storyboard, View, Group } from 'utopia-api'

  export var App = (props) => {
    return (
      <div style={{ position: 'absolute', left: 100, top: 100, width: '100%', height: '100%' }} data-uid='aaa' />
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
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', top: 550, left: 500 }}
          data-uid='bbb'
          data-testid='bbb'
        >
          I'm missing horizontal props
        </div>
        <div
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 550, top: 500 }}
          data-uid='ccc'
          data-testid='ccc'
        >
          I'm missing vertical props
        </div>
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
    await dragElement(renderResult, 'bbb', dragDelta, cmdModifier, null, async () =>
      renderResult.dispatch([CanvasActions.setUsersPreferredStrategy('ABSOLUTE_REPARENT')], true),
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(
        `
  import * as React from 'react'
  import { Scene, Storyboard, View, Group } from 'utopia-api'

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
  // this test is outdated, we only reparent with cmd pressed, and then we reparent to smaller parents too
  xit('reparents to the canvas root when target parent on the canvas is small', async () => {
    const renderResult = await renderTestEditorWithCode(
      formatTestProjectCode(`
import * as React from 'react'
import { Scene, Storyboard, View, Group } from 'utopia-api'

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
    await dragElement(renderResult, 'bbb', dragDelta, cmdModifier, null, async () => {
      void renderResult.dispatch(
        [CanvasActions.setUsersPreferredStrategy('ABSOLUTE_REPARENT')],
        true,
      )
    })

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(
        `
    import * as React from 'react'
    import { Scene, Storyboard, View, Group } from 'utopia-api'

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
  // Outdated test, cmd is necessary for reparenting, and it forces reparenting to ancestor outside of the containing component
  xit('does not reparent to ancestor outside of the containing component when the mouse is inside the containing component bounds', async () => {
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
  import { Scene, Storyboard, View, Group } from 'utopia-api'

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
  import { Scene, Storyboard, View, Group } from 'utopia-api'

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
    await dragElement(renderResult, 'bbb', dragDelta, cmdModifier, null, null)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(
        `
  import * as React from 'react'
  import { Scene, Storyboard, View, Group } from 'utopia-api'

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
  import { Scene, Storyboard, View, Group } from 'utopia-api'

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
  it('does not reparent scene into other component in scene', async () => {
    const editor = await renderTestEditorWithCode(
      ProjectWithNestedComponents,
      'await-first-dom-report',
    )

    const dragMeBB = editor.renderedDOM.getByTestId('drag-me').getBoundingClientRect()
    const dragMeCenter = {
      x: dragMeBB.left + dragMeBB.width / 2,
      y: dragMeBB.top + dragMeBB.height / 2,
    }
    const dragHereBB = editor.renderedDOM.getByTestId('drag-here').getBoundingClientRect()
    const dragHereCenter = {
      x: dragHereBB.left + dragHereBB.width / 2,
      y: dragHereBB.top + dragHereBB.height / 2,
    }
    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    await mouseDoubleClickAtPoint(canvasControlsLayer, dragHereCenter)

    // check that `drag-here` is expanded in the navigator
    expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
      'regular-sb/scene1',
      'regular-sb/scene1/container1',
      'regular-sb/scene1/container1:container-root-div',
      'regular-sb/scene1/container1:container-root-div/135',
      'regular-sb/scene1/container1/hello',
      'regular-sb/container2',
      'regular-sb/container2:container-root-div',
      'regular-sb/container2:container-root-div/135',
      'regular-sb/container2/hi',
    ])

    const dragDelta = windowPoint({
      x: dragHereCenter.x - dragMeCenter.x,
      y: dragHereCenter.y - dragMeCenter.y,
    })

    await dragElement(editor, 'drag-me', dragDelta, cmdModifier, null, async () => {
      await editor.dispatch([CanvasActions.setUsersPreferredStrategy('ABSOLUTE_REPARENT')], true)
      expect(getCursorFromEditor(await editor.getEditorState().editor)).toEqual(
        CSSCursor.NotPermitted, // checks that we show that it's not permitted
      )
    })

    // the drag is prevented, nothing changes
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(ProjectWithNestedComponents)
  })
  it('referencing a value from outside the element prevents reparenting', async () => {
    function createCodeForProject(left: number, top: number) {
      return Prettier.format(
        `
  import * as React from 'react'
  import { Scene, Storyboard, View, Group } from 'utopia-api'

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
    await dragElement(renderResult, 'bbb', dragDelta, cmdModifier, null, async () =>
      renderResult.dispatch([CanvasActions.setUsersPreferredStrategy('ABSOLUTE_REPARENT')], true),
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(createCodeForProject(40, 50))
  })

  describe('with fragments support enabled', () => {
    it('reparents across from one fragment to within (not directly inside) another', async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(`
import * as React from 'react'
import { Scene, Storyboard, View, Group } from 'utopia-api'

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
      await dragElement(renderResult, 'ccc', dragDelta, cmdModifier, null, async () => {
        const draggedElement = await renderResult.renderedDOM.findByTestId('ccc')
        const draggedElementBounds = draggedElement.getBoundingClientRect()
        const draggedElementCanvasBounds = boundingClientRectToCanvasRectangle(
          renderResult,
          draggedElementBounds,
        )
        expect(draggedElementCanvasBounds.x).toEqual(625)
        expect(draggedElementCanvasBounds.y).toEqual(425)
        expect(draggedElementCanvasBounds.width).toEqual(50)
        expect(draggedElementCanvasBounds.height).toEqual(50)
      })

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        Prettier.format(
          `
import * as React from 'react'
import { Scene, Storyboard, View, Group } from 'utopia-api'

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
    it('reparents across from one fragment to within (not directly inside) another', async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(`
import * as React from 'react'
import { Scene, Storyboard, View, Group } from 'utopia-api'

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
      await dragElement(renderResult, 'ccc', dragDelta, cmdModifier, null, async () => {
        const draggedElement = await renderResult.renderedDOM.findByTestId('ccc')
        const draggedElementBounds = draggedElement.getBoundingClientRect()
        const draggedElementCanvasBounds = boundingClientRectToCanvasRectangle(
          renderResult,
          draggedElementBounds,
        )
        expect(draggedElementCanvasBounds.x).toEqual(625)
        expect(draggedElementCanvasBounds.y).toEqual(425)
        expect(draggedElementCanvasBounds.width).toEqual(50)
        expect(draggedElementCanvasBounds.height).toEqual(50)
      })

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        Prettier.format(
          `
import * as React from 'react'
import { Scene, Storyboard, View, Group } from 'utopia-api'

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

  describe('fragment-like reparent tests', () => {
    AllFragmentLikeTypes.forEach((type) => {
      describe(`Absolute reparent with fragment-like element ${type} in the mix`, () => {
        it('cannot reparent into a fragment-like div', async () => {
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
              cursor: CSSCursor.Reparent,
            },
            null,
          )

          await renderResult.getDispatchFollowUpActionsFinished()

          expect(getRegularNavigatorTargets(renderResult)).toEqual([
            'utopia-storyboard-uid/scene-aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/fragment-like',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/fragment-like/inner-fragment',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/fragment-like/inner-fragment/child-1',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/fragment-like/inner-fragment/child-2',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/child-3',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/ccc', // <- ccc becomes a child of aaa/bbb, even though it was dragged over the globalFrame of fragment-like
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent',
          ])
        })

        it('drag-to-moving a child of a fragment-like element does not change the parent if the drag starts over the ancestor', async () => {
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

        it('drag-to-moving a child of a fragment-like element DOES change the parent if the drag leaves the ancestor', async () => {
          const renderResult = await renderTestEditorWithCode(
            testProjectWithUnstyledDivOrFragment(type),
            'await-first-dom-report',
          )

          const dragDelta = windowPoint({ x: 0, y: -100 })
          await dragElement(renderResult, 'child-2', dragDelta, cmdModifier, null, null)

          await renderResult.getDispatchFollowUpActionsFinished()

          // no reparent have happened
          expect(getRegularNavigatorTargets(renderResult)).toEqual([
            'utopia-storyboard-uid/scene-aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/fragment-like',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/fragment-like/inner-fragment',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/fragment-like/inner-fragment/child-1',
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

          expect(getRegularNavigatorTargets(renderResult)).toEqual([
            'utopia-storyboard-uid/scene-aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/fragment-like',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/fragment-like/inner-fragment',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/fragment-like/inner-fragment/child-2',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/child-3',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/child-1', // <child-1 is not the direct child of bbb
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/ccc',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent',
          ])
        })
      })

      it(`reparenting the fragment-like ${type} to an absolute parent works`, async () => {
        const renderResult = await renderTestEditorWithCode(
          testProjectWithUnstyledDivOrFragment(type),
          'await-first-dom-report',
        )

        const child1GlobalFrameBefore = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
          EP.fromString(
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/fragment-like/inner-fragment/child-1',
          ),
          renderResult.getEditorState().editor.jsxMetadata,
        )
        const child2GlobalFrameBefore = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
          EP.fromString(
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/fragment-like/inner-fragment/child-2',
          ),
          renderResult.getEditorState().editor.jsxMetadata,
        )

        const targetElement = EP.fromString(
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/fragment-like',
        )
        // selecting the fragment-like parent manually, so that dragElement drags _it_ instead of child-2!
        await renderResult.dispatch(selectComponents([targetElement], false), true)
        const dragDelta = windowPoint({ x: 0, y: 140 })
        await dragAlreadySelectedElement(
          renderResult,
          'child-2',
          dragDelta,
          cmdModifier,
          null,
          async () =>
            renderResult.dispatch(
              [CanvasActions.setUsersPreferredStrategy('ABSOLUTE_REPARENT')],
              true,
            ),
        )

        await renderResult.getDispatchFollowUpActionsFinished()

        expect(getRegularNavigatorTargets(renderResult)).toEqual([
          'utopia-storyboard-uid/scene-aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/child-3',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/ccc',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/fragment-like', // <- the fragment-like fragment-like element has been reparented to otherparent, yay!
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/fragment-like/inner-fragment',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/fragment-like/inner-fragment/child-1',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/fragment-like/inner-fragment/child-2',
        ])

        const propsOfFragment =
          renderResult.getEditorState().editor.allElementProps[
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/fragment-like'
          ]
        // the fragment-like element continues to have no style prop
        expect(propsOfFragment?.style == null).toBeTruthy()
        const propsOfInnerFragment =
          renderResult.getEditorState().editor.allElementProps[
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/fragment-like/inner-fragment'
          ]
        // the inner fragment-like element continues to have no style prop
        expect(propsOfInnerFragment?.style == null).toBeTruthy()

        const child1GlobalFrameAfter = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
          EP.fromString(
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/fragment-like/inner-fragment/child-1',
          ),
          renderResult.getEditorState().editor.jsxMetadata,
        )
        const child2GlobalFrameAfter = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
          EP.fromString(
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/fragment-like/inner-fragment/child-2',
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

      it(`reparenting the fragment-like ${type} from the canvas to an absolute parent works`, async () => {
        const renderResult = await renderTestEditorWithCode(
          testProjectWithUnstyledDivOrFragmentOnCanvas(type),
          'await-first-dom-report',
        )

        const child1GlobalFrameBefore = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
          EP.fromString('utopia-storyboard-uid/fragment-like/inner-fragment/child-1'),
          renderResult.getEditorState().editor.jsxMetadata,
        )
        const child2GlobalFrameBefore = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
          EP.fromString('utopia-storyboard-uid/fragment-like/inner-fragment/child-2'),
          renderResult.getEditorState().editor.jsxMetadata,
        )

        const targetElement = EP.fromString('utopia-storyboard-uid/fragment-like')
        // selecting the fragment-like parent manually, so that dragElement drags _it_ instead of child-2!
        await renderResult.dispatch(selectComponents([targetElement], false), true)
        const dragDelta = windowPoint({ x: 300, y: 150 })
        await dragAlreadySelectedElement(
          renderResult,
          'child-2',
          dragDelta,
          cmdModifier,
          null,
          async () =>
            renderResult.dispatch(
              [CanvasActions.setUsersPreferredStrategy('ABSOLUTE_REPARENT')],
              true,
            ),
        )

        await renderResult.getDispatchFollowUpActionsFinished()

        expect(getRegularNavigatorTargets(renderResult)).toEqual([
          'utopia-storyboard-uid/scene-aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/child-3',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/ccc',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/fragment-like', // <- the fragment-like fragment-like element has been reparented to otherparent, yay!
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/fragment-like/inner-fragment',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/fragment-like/inner-fragment/child-1',
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/fragment-like/inner-fragment/child-2',
        ])

        const propsOfFragment =
          renderResult.getEditorState().editor.allElementProps[
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/fragment-like'
          ]
        // the fragment-like element continues to have no style prop
        expect(propsOfFragment?.style == null).toBeTruthy()
        const propsOfInnerFragment =
          renderResult.getEditorState().editor.allElementProps[
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/fragment-like/inner-fragment'
          ]
        // the inner fragment-like element continues to have no style prop
        expect(propsOfInnerFragment?.style == null).toBeTruthy()

        const child1GlobalFrameAfter = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
          EP.fromString(
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/fragment-like/inner-fragment/child-1',
          ),
          renderResult.getEditorState().editor.jsxMetadata,
        )
        const child2GlobalFrameAfter = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
          EP.fromString(
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/fragment-like/inner-fragment/child-2',
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

  describe('conditional slots', () => {
    it('reparents into a conditional slot if empty', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root' style={{background: "#0ff"}}>
          <div data-uid='aaa' style={{ width: 200, height: 200, position: "absolute", top: 0, left: 0, background: "#ccc" }}>
            { true ? null : <div data-uid='false-branch' /> }
          </div>
          <div
            style={{ backgroundColor: '#f0f', position: 'absolute', width: 50, height: 50, top: 250, left: 250 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
        'await-first-dom-report',
      )

      const dragDelta = windowPoint({ x: -150, y: -150 })
      await dragElement(renderResult, 'bbb', dragDelta, cmdModifier, null, null)

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root' style={{background: "#0ff"}}>
          <div data-uid='aaa' style={{ width: 200, height: 200, position: "absolute", top: 0, left: 0, background: "#ccc" }}>
            {
              true ? (
                <div
                  style={{
                    backgroundColor: '#f0f',
                    position: 'absolute',
                    width: 50,
                    height: 50,
                    top: 100,
                    left: 100
                  }}
                  data-uid='bbb'
                  data-testid='bbb'
                />
              ) : (
                <div data-uid='false-branch' />
              )
            }
          </div>
        </div>
      `),
      )
    })
    it('reparents into the parent if slot is not empty', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root' style={{background: "#0ff"}}>
          <div data-uid='aaa' style={{ width: 200, height: 200, position: "absolute", top: 0, left: 0, background: "#ccc" }}>
            { true ? <div data-uid='false-branch' /> : null }
          </div>
          <div
            style={{ backgroundColor: '#f0f', position: 'absolute', width: 50, height: 50, top: 250, left: 250 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
        'await-first-dom-report',
      )

      const dragDelta = windowPoint({ x: -150, y: -150 })
      await dragElement(renderResult, 'bbb', dragDelta, cmdModifier, null, null)

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root' style={{background: "#0ff"}}>
          <div data-uid='aaa' style={{ width: 200, height: 200, position: "absolute", top: 0, left: 0, background: "#ccc" }}>
            { true ? <div data-uid='false-branch' /> : null }
            <div
              style={{
                backgroundColor: '#f0f',
                position: 'absolute',
                width: 50,
                height: 50,
                top: 100,
                left: 100
              }}
              data-uid='bbb'
              data-testid='bbb'
            />
          </div>
        </div>
      `),
      )
    })
    it('supports reparenting into nested conditionals', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root' style={{background: "#0ff"}}>
          <div data-uid='aaa' style={{ width: 200, height: 200, position: "absolute", top: 0, left: 0, background: "#ccc" }}>
            {true ? (
              true ? (
                true ? null : (
                  <div data-uid='false-branch1' />
                )
              ) : (
                <div data-uid='false-branch2' />
              )
            ) : (
              <div data-uid='false-branch3' />
            )}
          </div>
          <div
            style={{ backgroundColor: '#f0f', position: 'absolute', width: 50, height: 50, top: 250, left: 250 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
        'await-first-dom-report',
      )

      const dragDelta = windowPoint({ x: -150, y: -150 })
      await dragElement(renderResult, 'bbb', dragDelta, cmdModifier, null, null)

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='root' style={{background: "#0ff"}}>
            <div data-uid='aaa' style={{ width: 200, height: 200, position: "absolute", top: 0, left: 0, background: "#ccc" }}>
              {true ? (
                true ? (
                  true ? (
                    <div
                      style={{
                        backgroundColor: '#f0f',
                        position: 'absolute',
                        width: 50,
                        height: 50,
                        top: 100,
                        left: 100
                      }}
                      data-uid='bbb'
                      data-testid='bbb'
                    />
                  ) : (
                    <div data-uid='false-branch1' />
                  )
                ) : (
                  <div data-uid='false-branch2' />
                )
              ) : (
                <div data-uid='false-branch3' />
              )}
            </div>
          </div>
        `),
      )
    })
  })

  describe('groups', () => {
    it("does not reparent outside of a group (if it's not inside a scene)", async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='sb'>
              <div data-uid='div' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', width: 300, height: 250 }}>
                <Group data-uid='group' style={{ position: 'absolute', left: 25, top: 25, width: 175, height: 120, backgroundColor: 'white' }}>
                  <div data-uid='child1' data-testid='child1' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 50, height: 43 }} />
                  <div data-uid='child2' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 125, top: 77, width: 50, height: 43 }} />
                </Group>
              </div>
            </Storyboard>
          )
        `),
        'await-first-dom-report',
      )
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      await renderResult.dispatch(
        selectComponents([EP.fromString('sb/div/group/child1')], true),
        true,
      )

      const dragme = (await renderResult.renderedDOM.findByTestId('child1')).getBoundingClientRect()

      await mouseDragFromPointWithDelta(
        canvasControlsLayer,
        { x: dragme.x + 10, y: dragme.y + 10 },
        { x: -100, y: 0 },
      )

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='sb'>
              <div data-uid='div' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', width: 300, height: 250 }}>
                <Group data-uid='group' style={{ position: 'absolute', left: -75, top: 25, width: 275, height: 120, backgroundColor: 'white' }}>
                  <div data-uid='child1' data-testid='child1' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 50, height: 43 }} />
                  <div data-uid='child2' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 225, top: 77, width: 50, height: 43 }} />
                </Group>
              </div>
            </Storyboard>
          )
        `),
      )
    })
    it("does reparent outside of a group (if it's inside a scene)", async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Scene, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='sb'>
              <Scene data-uid='scene' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', width: 300, height: 250 }}>
                <Group data-uid='group' style={{ position: 'absolute', left: 25, top: 25, width: 175, height: 120, backgroundColor: 'white' }}>
                  <div data-uid='child1' data-testid='child1' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 50, height: 43 }} />
                  <div data-uid='child2' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 125, top: 77, width: 50, height: 43 }} />
                </Group>
              </Scene>
            </Storyboard>
          )
        `),
        'await-first-dom-report',
      )
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      await renderResult.dispatch(
        selectComponents([EP.fromString('sb/scene/group/child1')], true),
        true,
      )

      const dragme = (await renderResult.renderedDOM.findByTestId('child1')).getBoundingClientRect()

      await mouseDragFromPointWithDelta(
        canvasControlsLayer,
        { x: dragme.x + 10, y: dragme.y + 10 },
        { x: -100, y: 0 },
        {
          modifiers: cmdModifier,
          midDragCallback: async () =>
            renderResult.dispatch(
              [CanvasActions.setUsersPreferredStrategy('ABSOLUTE_REPARENT')],
              true,
            ),
        },
      )

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Scene, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='sb'>
              <Scene data-uid='scene' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', width: 300, height: 250 }}>
                <Group data-uid='group' style={{ position: 'absolute', left: 150, top: 102, width: 50, height: 43, backgroundColor: 'white' }}>
                  <div data-uid='child2' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 50, height: 43 }} />
                </Group>
              </Scene>
              <div data-uid='child1' data-testid='child1' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: -75, top: 25, width: 50, height: 43 }} />
            </Storyboard>
          )
        `),
      )
    })
    it("does reparent outside of a group (if it's inside a scene app)", async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(
          makeTestProjectCodeWithSnippet(`
            <Group data-uid='group' style={{ position: 'absolute', left: 25, top: 25, width: 175, height: 120, backgroundColor: 'white' }}>
              <div data-uid='child1' data-testid='child1' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 50, height: 43 }} />
              <div data-uid='child2' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 125, top: 77, width: 50, height: 43 }} />
            </Group>
        `),
        ),
        'await-first-dom-report',
      )
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      await renderResult.dispatch(
        selectComponents(
          [EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:group/child1')],
          true,
        ),
        true,
      )

      const dragme = (await renderResult.renderedDOM.findByTestId('child1')).getBoundingClientRect()

      await mouseDragFromPointWithDelta(
        canvasControlsLayer,
        { x: dragme.x + 10, y: dragme.y + 10 },
        { x: -100, y: 0 },
        {
          modifiers: cmdModifier,
          midDragCallback: async () =>
            renderResult.dispatch(
              [CanvasActions.setUsersPreferredStrategy('ABSOLUTE_REPARENT')],
              true,
            ),
        },
      )

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        formatTestProjectCode(`
          import * as React from 'react'
          import { Scene, Storyboard, View, Group } from 'utopia-api'

          export var App = (props) => {
            return (
              <Group data-uid='group' style={{ position: 'absolute', left: 150, top: 102, width: 50, height: 43, backgroundColor: 'white' }}>
                <div data-uid='child2' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 50, height: 43 }} />
              </Group>
            )
          }

          export var storyboard = (props) => {
            return (
              <Storyboard data-uid='utopia-storyboard-uid'>
                <Scene style={{ left: 0, top: 0, width: 400, height: 400 }} data-uid='scene-aaa'>
                  <App data-uid='app-entity' style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }} />
                </Scene>
                <div data-uid='child1' data-testid='child1' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: -75, top: 25, width: 50, height: 43 }} />
              </Storyboard>
            )
          }
        `),
      )
    })
  })
  // we don't have snapping with reparenting anymore, because cmd enables reparenting and disables snapping at the same time
  xdescribe('snapping', () => {
    const NewParentTestId = 'new-parent'
    const NewSiblingTestId = 'new-sibling'
    const project = (innards: string) => `<div
    style={{
      height: '100%',
      width: '100%',
      contain: 'layout',
    }}
    data-uid='root'
  >
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 28,
        top: 67,
        width: 244,
        height: 141,
        padding: '37px 60px',
      }}
      data-testid='${NewParentTestId}'
      data-uid='new-parent'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 59,
          height: 67,
          contain: 'layout',
          position: 'absolute',
          left: 22,
          top: 0,
        }}
        data-testid='${NewSiblingTestId}'
        data-uid='new-sibling'
      />
    </div>
    ${innards}
  </div>`

    it('when reparented from absolute, it snaps to new parent and children of new parent', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          project(`
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 130,
            top: 242,
            width: 70,
            height: 80,
          }}
          data-uid='container'
        >
          <img
            src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.jpg?raw=true'
            alt='Utopia logo'
            style={{
              width: 46,
              height: 58,
              position: 'absolute',
              left: 12,
              top: 11,
            }}
            data-testid='drag-me'
            data-uid='drag-me'
          />
        </div>
    `),
        ),
        'await-first-dom-report',
      )

      const newParentCenterX = getElementCenterCoords(renderResult, NewParentTestId).x
      const newSiblingCenterY = getElementCenterCoords(renderResult, NewSiblingTestId).y
      const elementToDragCenter = getElementCenterCoords(renderResult, 'drag-me')
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      await selectComponentsForTest(renderResult, [
        EP.appendNewElementPath(TestScenePath, ['root', 'container', 'drag-me']),
      ])

      await mouseDragFromPointToPoint(
        canvasControlsLayer,
        elementToDragCenter,
        windowPoint({ x: newParentCenterX, y: newSiblingCenterY }),
        {
          modifiers: cmdModifier,
          midDragCallback: async () => {
            const guidelines =
              renderResult.getEditorState().editor.canvas.controls.snappingGuidelines
            expect(guidelines).toHaveLength(2)
          },
        },
      )
    })
    it('when reparented from flex, it snaps to new parent and children of new parent', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          project(`
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 130,
            top: 242,
            width: 'max-content',
            height: 'max-content',
            display: 'flex',
            flexDirection: 'column',
            padding: '11px 12px',
          }}
          data-uid='container'
        >
          <img
            src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.jpg?raw=true'
            alt='Utopia logo'
            style={{
              width: 46,
              height: 58,
              position: 'absolute',
              left: 12,
              top: 11,
            }}
            data-testid='drag-me'
            data-uid='drag-me'
          />
        </div>
    `),
        ),
        'await-first-dom-report',
      )

      const newParentCenterX = getElementCenterCoords(renderResult, NewParentTestId).x
      const newSiblingCenterY = getElementCenterCoords(renderResult, NewSiblingTestId).y
      const elementToDragCenter = getElementCenterCoords(renderResult, 'drag-me')
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      await selectComponentsForTest(renderResult, [
        EP.appendNewElementPath(TestScenePath, ['root', 'container', 'drag-me']),
      ])

      await mouseDragFromPointToPoint(
        canvasControlsLayer,
        elementToDragCenter,
        windowPoint({ x: newParentCenterX, y: newSiblingCenterY }),
        {
          modifiers: cmdModifier,
          midDragCallback: async () => {
            const guidelines =
              renderResult.getEditorState().editor.canvas.controls.snappingGuidelines
            expect(guidelines).toHaveLength(2)
          },
          moveBeforeMouseDown: true,
          staggerMoveEvents: true,
        },
      )
    })
    it('when reparented from flow, it snaps to new parent and children of new parent', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          project(`
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 130,
              top: 242,
              width: 70,
              height: 80,
              padding: '11px 12px',
            }}
            data-uid='container'
          >
            <img
              src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.jpg?raw=true'
              alt='Utopia logo'
              style={{
                width: 46,
                height: 58,
                position: 'absolute',
                left: 12,
                top: 11,
              }}
              data-uid='drag-me'
              data-testid='drag-me'
            />
          </div>
    `),
        ),
        'await-first-dom-report',
      )

      const newParentCenterX = getElementCenterCoords(renderResult, NewParentTestId).x
      const newSiblingCenterY = getElementCenterCoords(renderResult, NewSiblingTestId).y
      const elementToDragCenter = getElementCenterCoords(renderResult, 'drag-me')
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      await selectComponentsForTest(renderResult, [
        EP.appendNewElementPath(TestScenePath, ['root', 'container', 'drag-me']),
      ])

      await mouseDragFromPointToPoint(
        canvasControlsLayer,
        elementToDragCenter,
        windowPoint({ x: newParentCenterX, y: newSiblingCenterY }),
        {
          modifiers: cmdModifier,
          midDragCallback: async () => {
            const guidelines =
              renderResult.getEditorState().editor.canvas.controls.snappingGuidelines
            expect(guidelines).toHaveLength(2)
          },
        },
      )
    })
  })
})

function testProjectWithUnstyledDivOrFragment(type: FragmentLikeType): string {
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
      'fragment-like',
    ])
  } else {
    FOR_TESTS_setNextGeneratedUids(['skip1', 'skip2', 'inner-fragment', 'fragment-like'])
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
          ${getOpeningFragmentLikeTag(type)}
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
          ${getClosingFragmentLikeTag(type)}
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

function testProjectWithUnstyledDivOrFragmentOnCanvas(type: FragmentLikeType): string {
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
      'fragment-like',
    ])
  } else {
    FOR_TESTS_setNextGeneratedUids(['skip1', 'skip2', 'inner-fragment', 'fragment-like'])
  }

  const code = `
  import * as React from 'react'
  import { Scene, Storyboard, View, Group } from 'utopia-api'

  export var App = (props) => {
    return (<div
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
    </div>)
  }

  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard data-uid='${BakedInStoryboardUID}'>
        <Scene
          style={{ left: 100, top: -50, width: 400, height: 400 }}
          data-uid='${TestSceneUID}'
        >
          <App
            data-uid='${TestAppUID}'
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
          />
        </Scene>
        ${getOpeningFragmentLikeTag(type)}
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: -200,
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
              left: -100,
              top: 50,
              width: 75,
              height: 40,
            }}
            data-uid='child-2'
            data-testid='child-2'
          />
        ${getClosingFragmentLikeTag(type)}
      </Storyboard>
    )
  }
`
  return formatTestProjectCode(code)
}

function getElementCenterCoords(editor: EditorRenderResult, testId: string): WindowPoint {
  const element = editor.renderedDOM.getByTestId(testId)
  const bounds = element.getBoundingClientRect()
  const center = windowPoint({ x: bounds.x + bounds.width / 2, y: bounds.y + bounds.height / 2 })
  return center
}

const ProjectWithNestedComponents = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

function Container({ children, ...props }) {
  return (
    <div data-uid='container-root-div' {...props}>
      {children}
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      commentId='scene'
      data-testid='drag-me'
      data-label='Scene 2'
      style={{
        position: 'absolute',
        left: 1126,
        top: 667,
        width: 405,
        height: 522,
      }}
      data-uid='scene1'
    >
      <Container
        data-uid='container1'
        style={{
          backgroundColor: '#ff4500',
          width: 700,
          height: 759,
          position: 'absolute',
          left: 31,
          top: 143,
        }}
      >
        <h2 data-uid='hello'>Hello</h2>
      </Container>
    </Scene>
    <Container
      data-testid='drag-here'
      data-uid='container2'
      style={{
        backgroundColor: '#0074ff',
        width: 600,
        height: 659,
        position: 'absolute',
        left: 331,
        top: -130,
      }}
    >
      <h2 data-uid='hi'>Hi</h2>
    </Container>
  </Storyboard>
)
`
