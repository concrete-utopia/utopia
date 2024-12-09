import * as EP from '../../../../core/shared/element-path'
import type { CanvasVector } from '../../../../core/shared/math-utils'
import { windowPoint, zeroCanvasPoint } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { Modifiers } from '../../../../utils/modifiers'
import { emptyModifiers } from '../../../../utils/modifiers'
import { selectComponentsForTest } from '../../../../utils/utils.test-utils'
import { selectComponents } from '../../../editor/actions/action-creators'
import CanvasActions from '../../canvas-actions'
import type { EdgePosition } from '../../canvas-types'
import { EdgePositionBottomRight } from '../../canvas-types'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import {
  mouseClickAtPoint,
  mouseDoubleClickAtPoint,
  mouseDragFromPointWithDelta,
} from '../../event-helpers.test-utils'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import {
  formatTestProjectCode,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../../ui-jsx.test-utils'
import { createInteractionViaMouse, updateInteractionViaMouse } from '../interaction-state'

// no mouseup here! it starts the interaction and resizes with drag delta
async function startDragUsingActions(
  renderResult: EditorRenderResult,
  target: ElementPath,
  edgePosition: EdgePosition,
  dragDelta: CanvasVector,
) {
  await renderResult.dispatch([selectComponents([target], false)], true)
  const startInteractionSession = createInteractionViaMouse(
    zeroCanvasPoint,
    emptyModifiers,
    {
      type: 'RESIZE_HANDLE',
      edgePosition: edgePosition,
    },
    'zero-drag-not-permitted',
  )
  await renderResult.dispatch(
    [CanvasActions.createInteractionSession(startInteractionSession)],
    false,
  )
  await renderResult.getDispatchFollowUpActionsFinished()
  await renderResult.dispatch(
    [
      CanvasActions.updateInteractionSession(
        updateInteractionViaMouse(startInteractionSession, 'DRAG', dragDelta, emptyModifiers, {
          type: 'RESIZE_HANDLE',
          edgePosition: edgePosition,
        }),
      ),
    ],
    false,
  )
  await renderResult.getDispatchFollowUpActionsFinished()
}

async function doDblClickTest(
  editor: EditorRenderResult,
  testId: string,
  verticalOffset: number = 30,
): Promise<HTMLElement> {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.renderedDOM.getByTestId('mydiv')
  const divBounds = div.getBoundingClientRect()
  const divCorner = {
    x: divBounds.x + 50,
    y: divBounds.y + 40,
  }

  await mouseClickAtPoint(canvasControlsLayer, divCorner)

  const nineBlockControlSegment = editor.renderedDOM.getByTestId(testId)

  await mouseDoubleClickAtPoint(nineBlockControlSegment, { x: 2, y: verticalOffset })

  return div
}

async function doSnapDrag(
  editor: EditorRenderResult,
  delta: { x: number; y: number },
  edgePosition: EdgePosition,
  modifiers: Modifiers = emptyModifiers,
) {
  const canvasControl = editor.renderedDOM.getByTestId(
    `resize-control-${edgePosition.x}-${edgePosition.y}`,
  )

  const resizeCornerBounds = canvasControl.getBoundingClientRect()
  const startPoint = windowPoint({
    x: resizeCornerBounds.x + 2,
    y: resizeCornerBounds.y + 2,
  })

  await mouseDragFromPointWithDelta(canvasControl, startPoint, delta, {
    modifiers: modifiers,
  })
}

const RegularMultiChildProject = `
  <div
    data-uid='root'
    data-testid='root'
    style={{
      height: '100%',
      width: '100%',
      contain: 'layout',
      backgroundColor: '#ffffff',
    }}
  >
    <div
      data-uid='grid'
      data-testid='grid'
      style={{
        height: 150,
        position: 'absolute',
        left: 168,
        top: 305,
        width: 364,
        display: 'grid',
        gap: 5,
        padding: 10,
        gridTemplateColumns: '1fr 1fr 1fr',
        gridTemplateRows: '1fr 1fr',
        backgroundColor: '#36AA15',
      }}
    >
      <div
        data-uid='child1'
        data-testid='child1'
        style={{
          backgroundColor: 'red',
          gridRow: '1 / 2',
          gridColumn: 1,
          width: 50,
          contain: 'layout',
          justifySelf: 'stretch',
        }}
      />
      <div
        data-uid='child2'
        data-testid='child2'
        style={{
          position: 'absolute',
          backgroundColor: 'red',
          gridRow: '1 / 2',
          gridColumn: '2 / 3',
          width: '100%',
          height: '100%',
        }}
      />
      <div
        data-uid='child3'
        data-testid='child3'
        style={{
          position: 'absolute',
          backgroundColor: 'red',
          gridRow: '1 / 2',
          gridColumn: '3 / 4',
          width: '100%',
          height: '100%',
        }}
      />
    </div>
  </div>
`

/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "expectElementWithTestIdNotToBeRendered", "expectElementWithTestIdToBeRendered"] }] */

describe('Grid Child Corner Resize Strategy', () => {
  it('dragging corner of fixed and stretched child updates properties as expected', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(RegularMultiChildProject),
      'await-first-dom-report',
    )
    await selectComponentsForTest(renderResult, [
      EP.appendNewElementPath(TestScenePath, ['root', 'grid', 'child1']),
    ])

    await doSnapDrag(renderResult, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      formatTestProjectCode(
        makeTestProjectCodeWithSnippet(`
  <div
    data-uid='root'
    data-testid='root'
    style={{
      height: '100%',
      width: '100%',
      contain: 'layout',
      backgroundColor: '#ffffff',
    }}
  >
    <div
      data-uid='grid'
      data-testid='grid'
      style={{
        height: 150,
        position: 'absolute',
        left: 168,
        top: 305,
        width: 364,
        display: 'grid',
        gap: 5,
        padding: 10,
        gridTemplateColumns: '1fr 1fr 1fr',
        gridTemplateRows: '1fr 1fr',
        backgroundColor: '#36AA15',
      }}
    >
      <div
        data-uid='child1'
        data-testid='child1'
        style={{
          backgroundColor: 'red',
          width: 100,
          contain: 'layout',
          justifySelf: 'stretch',
          gridColumn: 1,
          gridRow: '1 / 3',
        }}
      />
      <div
        data-uid='child2'
        data-testid='child2'
        style={{
          position: 'absolute',
          backgroundColor: 'red',
          gridRow: '1 / 2',
          gridColumn: '2 / 3',
          width: '100%',
          height: '100%',
        }}
      />
      <div
        data-uid='child3'
        data-testid='child3'
        style={{
          position: 'absolute',
          backgroundColor: 'red',
          gridRow: '1 / 2',
          gridColumn: '3 / 4',
          width: '100%',
          height: '100%',
        }}
      />
    </div>
  </div>
`),
      ),
    )
  })
})
