import {
  EditorRenderResult,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
  TestSceneUID,
} from '../canvas/ui-jsx.test-utils'
import { act, fireEvent } from '@testing-library/react'
import { offsetPoint, windowPoint, WindowPoint } from '../../core/shared/math-utils'
import { PrettierConfig } from 'utopia-vscode-common'
import * as Prettier from 'prettier/standalone'
import { BakedInStoryboardVariableName, BakedInStoryboardUID } from '../../core/model/scene-utils'
import { getDomRectCenter } from '../../core/shared/dom-utils'
import { selectComponents } from '../editor/actions/action-creators'
import * as EP from '../../core/shared/element-path'

function dragElement(
  renderResult: EditorRenderResult,
  dragTargetID: string,
  dropTargetID: string,
  startPoint: WindowPoint,
  dragDelta: WindowPoint,
  hoverEvents: 'apply-hover-events' | 'do-not-apply-hover-events',
): void {
  const dragTarget = renderResult.renderedDOM.getByTestId(dragTargetID)
  const dropTarget = renderResult.renderedDOM.getByTestId(dropTargetID)

  const endPoint = offsetPoint(startPoint, dragDelta)

  fireEvent(
    dragTarget,
    new MouseEvent('dragstart', {
      bubbles: true,
      cancelable: true,
      clientX: startPoint.x,
      clientY: startPoint.y,
      buttons: 1,
    }),
  )

  fireEvent(
    dragTarget,
    new MouseEvent('drag', {
      bubbles: true,
      cancelable: true,
      clientX: endPoint.x,
      clientY: endPoint.y,
      movementX: dragDelta.x,
      movementY: dragDelta.y,
      buttons: 1,
    }),
  )

  if (hoverEvents === 'apply-hover-events') {
    fireEvent(
      dropTarget,
      new MouseEvent('dragenter', {
        bubbles: true,
        cancelable: true,
        clientX: endPoint.x,
        clientY: endPoint.y,
        movementX: dragDelta.x,
        movementY: dragDelta.y,
        buttons: 1,
      }),
    )

    fireEvent(
      dropTarget,
      new MouseEvent('dragover', {
        bubbles: true,
        cancelable: true,
        clientX: endPoint.x,
        clientY: endPoint.y,
        movementX: dragDelta.x,
        movementY: dragDelta.y,
        buttons: 1,
      }),
    )

    fireEvent(
      dropTarget,
      new MouseEvent('drop', {
        bubbles: true,
        cancelable: true,
        clientX: endPoint.x,
        clientY: endPoint.y,
        buttons: 1,
      }),
    )
  }
}

function getProjectCode(): string {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const unmoveableColour = 'orange'

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
          height: 500,
        }}
        data-uid='sceneroot'
        data-testid='sceneroot'
        data-label='sceneroot'
      >
        <div
          style={{
            backgroundColor: 'teal',
            position: 'absolute',
            left: 255,
            top: 35,
            width: 109,
            height: 123,
          }}
          data-uid='firstdiv'
          data-testid='firstdiv'
          data-label='firstdiv'
        />
        <div
          style={{
            backgroundColor: 'purple',
            position: 'absolute',
            left: 21,
            top: 215.5,
            width: 123,
            height: 100,
          }}
          data-uid='seconddiv'
          data-testid='seconddiv'
          data-label='seconddiv'
        />
        <div
          style={{
            backgroundColor: 'green',
            position: 'absolute',
            left: 26,
            top: 35,
            width: 118,
            height: 123,
          }}
          data-uid='thirddiv'
          data-testid='thirddiv'
          data-label='thirddiv'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            height: 65,
            width: 66,
            position: 'absolute',
            left: 265,
            top: 233,
          }}
          data-uid='dragme'
          data-testid='dragme'
          data-label='dragme'
        >
          drag me
        </div>
        <div
          style={{
            backgroundColor: unmoveableColour,
            height: 65,
            width: 66,
            position: 'absolute',
            left: 265,
            top: 300,
          }}
          data-uid='notdrag'
          data-testid='notdrag'
          data-label='notdrag'
        >
          not drag
        </div>
      </div>
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 500,
          width: 400,
          height: 200,
        }}
        data-uid='parentsibling'
        data-testid='parentsibling'
        data-label='parentsibling'
      />
    </Scene>
  </Storyboard>
)
`
}

function getProjectCodeDraggedToBeforeEverything(): string {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const unmoveableColour = 'orange'

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
          height: 500,
        }}
        data-uid='sceneroot'
        data-testid='sceneroot'
        data-label='sceneroot'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            height: 65,
            width: 66,
            position: 'absolute',
            left: 265,
            top: 233,
          }}
          data-uid='dragme'
          data-testid='dragme'
          data-label='dragme'
        >
          drag me
        </div>
        <div
          style={{
            backgroundColor: 'teal',
            position: 'absolute',
            left: 255,
            top: 35,
            width: 109,
            height: 123,
          }}
          data-uid='firstdiv'
          data-testid='firstdiv'
          data-label='firstdiv'
        />
        <div
          style={{
            backgroundColor: 'purple',
            position: 'absolute',
            left: 21,
            top: 215.5,
            width: 123,
            height: 100,
          }}
          data-uid='seconddiv'
          data-testid='seconddiv'
          data-label='seconddiv'
        />
        <div
          style={{
            backgroundColor: 'green',
            position: 'absolute',
            left: 26,
            top: 35,
            width: 118,
            height: 123,
          }}
          data-uid='thirddiv'
          data-testid='thirddiv'
          data-label='thirddiv'
        />
        <div
          style={{
            backgroundColor: unmoveableColour,
            height: 65,
            width: 66,
            position: 'absolute',
            left: 265,
            top: 300,
          }}
          data-uid='notdrag'
          data-testid='notdrag'
          data-label='notdrag'
        >
          not drag
        </div>
      </div>
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 500,
          width: 400,
          height: 200,
        }}
        data-uid='parentsibling'
        data-testid='parentsibling'
        data-label='parentsibling'
      />
    </Scene>
  </Storyboard>
)
`
}

function getProjectCodeDraggedToAfterFirstSibling(): string {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const unmoveableColour = 'orange'

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
          height: 500,
        }}
        data-uid='sceneroot'
        data-testid='sceneroot'
        data-label='sceneroot'
      >
        <div
          style={{
            backgroundColor: 'teal',
            position: 'absolute',
            left: 255,
            top: 35,
            width: 109,
            height: 123,
          }}
          data-uid='firstdiv'
          data-testid='firstdiv'
          data-label='firstdiv'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            height: 65,
            width: 66,
            position: 'absolute',
            left: 265,
            top: 233,
          }}
          data-uid='dragme'
          data-testid='dragme'
          data-label='dragme'
        >
          drag me
        </div>
        <div
          style={{
            backgroundColor: 'purple',
            position: 'absolute',
            left: 21,
            top: 215.5,
            width: 123,
            height: 100,
          }}
          data-uid='seconddiv'
          data-testid='seconddiv'
          data-label='seconddiv'
        />
        <div
          style={{
            backgroundColor: 'green',
            position: 'absolute',
            left: 26,
            top: 35,
            width: 118,
            height: 123,
          }}
          data-uid='thirddiv'
          data-testid='thirddiv'
          data-label='thirddiv'
        />
        <div
          style={{
            backgroundColor: unmoveableColour,
            height: 65,
            width: 66,
            position: 'absolute',
            left: 265,
            top: 300,
          }}
          data-uid='notdrag'
          data-testid='notdrag'
          data-label='notdrag'
        >
          not drag
        </div>
      </div>
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 500,
          width: 400,
          height: 200,
        }}
        data-uid='parentsibling'
        data-testid='parentsibling'
        data-label='parentsibling'
      />
    </Scene>
  </Storyboard>
)
`
}

function getProjectCodeDraggedToAfterLastSibling(): string {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const unmoveableColour = 'orange'

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
          height: 500,
        }}
        data-uid='sceneroot'
        data-testid='sceneroot'
        data-label='sceneroot'
      >
        <div
          style={{
            backgroundColor: 'teal',
            position: 'absolute',
            left: 255,
            top: 35,
            width: 109,
            height: 123,
          }}
          data-uid='firstdiv'
          data-testid='firstdiv'
          data-label='firstdiv'
        />
        <div
          style={{
            backgroundColor: 'purple',
            position: 'absolute',
            left: 21,
            top: 215.5,
            width: 123,
            height: 100,
          }}
          data-uid='seconddiv'
          data-testid='seconddiv'
          data-label='seconddiv'
        />
        <div
          style={{
            backgroundColor: 'green',
            position: 'absolute',
            left: 26,
            top: 35,
            width: 118,
            height: 123,
          }}
          data-uid='thirddiv'
          data-testid='thirddiv'
          data-label='thirddiv'
        />
        <div
          style={{
            backgroundColor: unmoveableColour,
            height: 65,
            width: 66,
            position: 'absolute',
            left: 265,
            top: 300,
          }}
          data-uid='notdrag'
          data-testid='notdrag'
          data-label='notdrag'
        >
          not drag
        </div>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            height: 65,
            width: 66,
            position: 'absolute',
            left: 265,
            top: 233,
          }}
          data-uid='dragme'
          data-testid='dragme'
          data-label='dragme'
        >
          drag me
        </div>
      </div>
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 500,
          width: 400,
          height: 200,
        }}
        data-uid='parentsibling'
        data-testid='parentsibling'
        data-label='parentsibling'
      />
    </Scene>
  </Storyboard>
)
`
}

function getProjectCodeReparentedUnderCousin(): string {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const unmoveableColour = 'orange'

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
          height: 500,
        }}
        data-uid='sceneroot'
        data-testid='sceneroot'
        data-label='sceneroot'
      >
        <div
          style={{
            backgroundColor: 'teal',
            position: 'absolute',
            left: 255,
            top: 35,
            width: 109,
            height: 123,
          }}
          data-uid='firstdiv'
          data-testid='firstdiv'
          data-label='firstdiv'
        />
        <div
          style={{
            backgroundColor: 'purple',
            position: 'absolute',
            left: 21,
            top: 215.5,
            width: 123,
            height: 100,
          }}
          data-uid='seconddiv'
          data-testid='seconddiv'
          data-label='seconddiv'
        />
        <div
          style={{
            backgroundColor: 'green',
            position: 'absolute',
            left: 26,
            top: 35,
            width: 118,
            height: 123,
          }}
          data-uid='thirddiv'
          data-testid='thirddiv'
          data-label='thirddiv'
        />
        <div
          style={{
            backgroundColor: unmoveableColour,
            height: 65,
            width: 66,
            position: 'absolute',
            left: 265,
            top: 300,
          }}
          data-uid='notdrag'
          data-testid='notdrag'
          data-label='notdrag'
        >
          not drag
        </div>
      </div>
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 500,
          width: 400,
          height: 200,
        }}
        data-uid='parentsibling'
        data-testid='parentsibling'
        data-label='parentsibling'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            height: 65,
            width: 66,
            position: 'absolute',
            left: 265,
            top: 233,
          }}
          data-uid='dragme'
          data-testid='dragme'
          data-label='dragme'
        >
          drag me
        </div>
      </div>
    </Scene>
  </Storyboard>
)
`
}
function getProjectCodeReparentedUnderFirstSibling(): string {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const unmoveableColour = 'orange'

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
          height: 500,
        }}
        data-uid='sceneroot'
        data-testid='sceneroot'
        data-label='sceneroot'
      >
        <div
          style={{
            backgroundColor: 'teal',
            position: 'absolute',
            left: 255,
            top: 35,
            width: 109,
            height: 123,
          }}
          data-uid='firstdiv'
          data-testid='firstdiv'
          data-label='firstdiv'
        >
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              height: 65,
              width: 66,
              position: 'absolute',
              left: 265,
              top: 233,
            }}
            data-uid='dragme'
            data-testid='dragme'
            data-label='dragme'
          >
            drag me
          </div>
        </div>
        <div
          style={{
            backgroundColor: 'purple',
            position: 'absolute',
            left: 21,
            top: 215.5,
            width: 123,
            height: 100,
          }}
          data-uid='seconddiv'
          data-testid='seconddiv'
          data-label='seconddiv'
        />
        <div
          style={{
            backgroundColor: 'green',
            position: 'absolute',
            left: 26,
            top: 35,
            width: 118,
            height: 123,
          }}
          data-uid='thirddiv'
          data-testid='thirddiv'
          data-label='thirddiv'
        />
        <div
          style={{
            backgroundColor: unmoveableColour,
            height: 65,
            width: 66,
            position: 'absolute',
            left: 265,
            top: 300,
          }}
          data-uid='notdrag'
          data-testid='notdrag'
          data-label='notdrag'
        >
          not drag
        </div>
      </div>
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 500,
          width: 400,
          height: 200,
        }}
        data-uid='parentsibling'
        data-testid='parentsibling'
        data-label='parentsibling'
      />
    </Scene>
  </Storyboard>
)
`
}

function getProjectCodeReparentedUnderScene(): string {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const unmoveableColour = 'orange'

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
          height: 500,
        }}
        data-uid='sceneroot'
        data-testid='sceneroot'
        data-label='sceneroot'
      >
        <div
          style={{
            backgroundColor: 'teal',
            position: 'absolute',
            left: 255,
            top: 35,
            width: 109,
            height: 123,
          }}
          data-uid='firstdiv'
          data-testid='firstdiv'
          data-label='firstdiv'
        />
        <div
          style={{
            backgroundColor: 'purple',
            position: 'absolute',
            left: 21,
            top: 215.5,
            width: 123,
            height: 100,
          }}
          data-uid='seconddiv'
          data-testid='seconddiv'
          data-label='seconddiv'
        />
        <div
          style={{
            backgroundColor: 'green',
            position: 'absolute',
            left: 26,
            top: 35,
            width: 118,
            height: 123,
          }}
          data-uid='thirddiv'
          data-testid='thirddiv'
          data-label='thirddiv'
        />
        <div
          style={{
            backgroundColor: unmoveableColour,
            height: 65,
            width: 66,
            position: 'absolute',
            left: 265,
            top: 300,
          }}
          data-uid='notdrag'
          data-testid='notdrag'
          data-label='notdrag'
        >
          not drag
        </div>
      </div>
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 500,
          width: 400,
          height: 200,
        }}
        data-uid='parentsibling'
        data-testid='parentsibling'
        data-label='parentsibling'
      />
    </Scene>
    <div
        style={{
          backgroundColor: '#aaaaaa33',
          height: 65,
          width: 66,
          position: 'absolute',
          left: 265,
          top: 233,
        }}
        data-uid='dragme'
        data-testid='dragme'
        data-label='dragme'
      >
        drag me
      </div>
  </Storyboard>
)
`
}

describe('Navigator', () => {
  it('reorders to before the first sibling', async () => {
    const renderResult = await renderTestEditorWithCode(getProjectCode(), 'await-first-dom-report')

    const dragMeElement = await renderResult.renderedDOM.findByTestId(
      `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
    )
    const dragMeElementRect = dragMeElement.getBoundingClientRect()
    const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
    const firstDivElement = await renderResult.renderedDOM.findByTestId(
      `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
    )
    const firstDivElementRect = firstDivElement.getBoundingClientRect()
    const firstDivElementCenter = getDomRectCenter(firstDivElementRect)
    const dragTo = {
      x: firstDivElementCenter.x,
      y: firstDivElementRect.y + 3,
    }

    const dragDelta = windowPoint({
      x: dragTo.x - dragMeElementCenter.x,
      y: dragTo.y - dragMeElementCenter.y,
    })

    const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetElement], false)], false)
      await dispatchDone
    })

    act(() =>
      dragElement(
        renderResult,
        `navigator-item-drag-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
        `navigator-item-drop-before-utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
        windowPoint(dragMeElementCenter),
        dragDelta,
        'apply-hover-events',
      ),
    )

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(getProjectCodeDraggedToBeforeEverything(), PrettierConfig),
    )
  })

  it('reorders to after the first sibling', async () => {
    const renderResult = await renderTestEditorWithCode(getProjectCode(), 'await-first-dom-report')

    const dragMeElement = await renderResult.renderedDOM.findByTestId(
      `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
    )
    const dragMeElementRect = dragMeElement.getBoundingClientRect()
    const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
    const firstDivElement = await renderResult.renderedDOM.findByTestId(
      `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
    )
    const firstDivElementRect = firstDivElement.getBoundingClientRect()
    const firstDivElementCenter = getDomRectCenter(firstDivElementRect)
    const dragTo = {
      x: firstDivElementCenter.x,
      y: firstDivElementRect.y + firstDivElementRect.height,
    }

    const dragDelta = windowPoint({
      x: dragTo.x - dragMeElementCenter.x,
      y: dragTo.y - dragMeElementCenter.y,
    })

    const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetElement], false)], false)
      await dispatchDone
    })

    act(() =>
      dragElement(
        renderResult,
        `navigator-item-drag-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
        `navigator-item-drop-after-utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
        windowPoint(dragMeElementCenter),
        dragDelta,
        'apply-hover-events',
      ),
    )

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(getProjectCodeDraggedToAfterFirstSibling(), PrettierConfig),
    )
  })

  it('reorders to after the last sibling', async () => {
    const renderResult = await renderTestEditorWithCode(getProjectCode(), 'await-first-dom-report')

    const dragMeElement = await renderResult.renderedDOM.findByTestId(
      `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
    )
    const dragMeElementRect = dragMeElement.getBoundingClientRect()
    const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
    const notDraggableDivElement = await renderResult.renderedDOM.findByTestId(
      `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
    )
    const notDraggableDivElementRect = notDraggableDivElement.getBoundingClientRect()
    const notDraggableDivElementCenter = getDomRectCenter(notDraggableDivElementRect)
    const dragTo = {
      x: notDraggableDivElementCenter.x,
      y: notDraggableDivElementRect.y + notDraggableDivElementRect.height - 3,
    }

    const dragDelta = windowPoint({
      x: dragTo.x - dragMeElementCenter.x,
      y: dragTo.y - dragMeElementCenter.y,
    })

    const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetElement], false)], false)
      await dispatchDone
    })

    act(() =>
      dragElement(
        renderResult,
        `navigator-item-drag-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
        `navigator-item-drop-after-utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
        windowPoint(dragMeElementCenter),
        dragDelta,
        'apply-hover-events',
      ),
    )

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(getProjectCodeDraggedToAfterLastSibling(), PrettierConfig),
    )
  })

  it('reparents under the first sibling', async () => {
    const renderResult = await renderTestEditorWithCode(getProjectCode(), 'await-first-dom-report')

    const dragMeElement = await renderResult.renderedDOM.findByTestId(
      `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
    )
    const dragMeElementRect = dragMeElement.getBoundingClientRect()
    const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
    const firstDivElement = await renderResult.renderedDOM.findByTestId(
      `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
    )
    const firstDivElementRect = firstDivElement.getBoundingClientRect()
    const firstDivElementCenter = getDomRectCenter(firstDivElementRect)
    const dragTo = {
      x: firstDivElementCenter.x,
      y: firstDivElementRect.y + 15,
    }

    const dragDelta = windowPoint({
      x: dragTo.x - dragMeElementCenter.x,
      y: dragTo.y - dragMeElementCenter.y,
    })

    const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetElement], false)], false)
      await dispatchDone
    })

    act(() =>
      dragElement(
        renderResult,
        `navigator-item-drag-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
        `navigator-item-drop-utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
        windowPoint(dragMeElementCenter),
        dragDelta,
        'apply-hover-events',
      ),
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(getProjectCodeReparentedUnderFirstSibling(), PrettierConfig),
    )
  })

  it('reparents under grandparent', async () => {
    const renderResult = await renderTestEditorWithCode(getProjectCode(), 'await-first-dom-report')

    const dragMeElement = await renderResult.renderedDOM.findByTestId(
      `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
    )
    const dragMeElementRect = dragMeElement.getBoundingClientRect()
    const dragMeElementCenter = getDomRectCenter(dragMeElementRect)

    const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetElement], false)], false)
      await dispatchDone
    })

    act(() => {
      dragElement(
        renderResult,
        `navigator-item-drag-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
        `navigator-item-drop-after-utopia_storyboard_uid/scene_aaa/sceneroot/thirddiv`,
        windowPoint(dragMeElementCenter),
        windowPoint({ x: -65, y: 0 }),
        'apply-hover-events',
      )
    })

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(getProjectCodeReparentedUnderScene(), PrettierConfig),
    )
  })

  it('reparents under cousin element', async () => {
    const renderResult = await renderTestEditorWithCode(getProjectCode(), 'await-first-dom-report')

    const dragMeElement = await renderResult.renderedDOM.findByTestId(
      `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
    )
    const dragMeElementRect = dragMeElement.getBoundingClientRect()
    const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
    const cousinDivElement = await renderResult.renderedDOM.findByTestId(
      `navigator-item-utopia_storyboard_uid/scene_aaa/parentsibling`,
    )
    const cousinDivElementRect = cousinDivElement.getBoundingClientRect()
    const cousinDivElementCenter = getDomRectCenter(cousinDivElementRect)
    const dragTo = {
      x: cousinDivElementCenter.x,
      y: cousinDivElementRect.y + 14,
    }

    const dragDelta = windowPoint({
      x: dragTo.x - dragMeElementCenter.x,
      y: dragTo.y - dragMeElementCenter.y,
    })

    const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetElement], false)], false)
      await dispatchDone
    })

    act(() =>
      dragElement(
        renderResult,
        `navigator-item-drag-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
        `navigator-item-drop-utopia_storyboard_uid/scene_aaa/parentsibling`,
        windowPoint(dragMeElementCenter),
        dragDelta,
        'apply-hover-events',
      ),
    )

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(getProjectCodeReparentedUnderCousin(), PrettierConfig),
    )
  })

  it('attempt to reparent non-reparentable item', async () => {
    const renderResult = await renderTestEditorWithCode(getProjectCode(), 'await-first-dom-report')

    const notDragElement = await renderResult.renderedDOM.findByTestId(
      `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
    )
    const notDragElementRect = notDragElement.getBoundingClientRect()
    const notDragElementCenter = getDomRectCenter(notDragElementRect)
    const cousinDivElement = await renderResult.renderedDOM.findByTestId(
      `navigator-item-utopia_storyboard_uid/scene_aaa/parentsibling`,
    )
    const cousinDivElementRect = cousinDivElement.getBoundingClientRect()
    const cousinDivElementCenter = getDomRectCenter(cousinDivElementRect)
    const dragTo = {
      x: cousinDivElementCenter.x,
      y: cousinDivElementRect.y + 10,
    }

    const dragDelta = windowPoint({
      x: dragTo.x - notDragElementCenter.x,
      y: dragTo.y - notDragElementCenter.y,
    })

    const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/notdrag')
    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetElement], false)], false)
      await dispatchDone
    })

    act(() =>
      dragElement(
        renderResult,
        `navigator-item-drag-utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
        `navigator-item-drop-utopia_storyboard_uid/scene_aaa/parentsibling`,
        windowPoint(notDragElementCenter),
        dragDelta,
        'do-not-apply-hover-events',
      ),
    )

    expect(renderResult.getEditorState().editor.navigator.dropTargetHint.type).toEqual(null)
    expect(
      renderResult.getEditorState().editor.navigator.dropTargetHint.displayAtElementPath,
    ).toEqual(null)

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(getProjectCode(), PrettierConfig),
    )
  })

  it('does not reparent to invalid target', async () => {
    const renderResult = await renderTestEditorWithCode(getProjectCode(), 'await-first-dom-report')

    const dragMeElement = await renderResult.renderedDOM.findByTestId(
      `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
    )
    const dragMeElementRect = dragMeElement.getBoundingClientRect()
    const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
    const notDraggableDivElement = await renderResult.renderedDOM.findByTestId(
      `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
    )
    const notDraggableDivElementRect = notDraggableDivElement.getBoundingClientRect()
    const notDraggableDivElementCenter = getDomRectCenter(notDraggableDivElementRect)
    const dragTo = {
      x: notDraggableDivElementCenter.x,
      y: notDraggableDivElementRect.y,
    }

    const dragDelta = windowPoint({
      x: dragTo.x - dragMeElementCenter.x,
      y: dragTo.y - dragMeElementCenter.y,
    })

    const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetElement], false)], false)
      await dispatchDone
    })

    act(() =>
      dragElement(
        renderResult,
        `navigator-item-drag-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
        `navigator-item-drop-utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
        windowPoint(dragMeElementCenter),
        dragDelta,
        'apply-hover-events',
      ),
    )

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(getProjectCode(), PrettierConfig),
    )
  })
})
