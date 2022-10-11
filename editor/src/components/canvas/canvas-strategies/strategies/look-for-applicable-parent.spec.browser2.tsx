import * as EP from '../../../../core/shared/element-path'
import {
  canvasPoint,
  CanvasVector,
  windowPoint,
  zeroCanvasPoint,
} from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { emptyModifiers } from '../../../../utils/modifiers'
import { selectComponents } from '../../../editor/actions/meta-actions'
import CanvasActions from '../../canvas-actions'
import { mouseDownAtPoint, mouseMoveToPoint } from '../../event-helpers.test-utils'
import {
  EditorRenderResult,
  formatTestProjectCode,
  renderTestEditorWithCode,
} from '../../ui-jsx.test-utils'
import {
  boundingArea,
  createInteractionViaMouse,
  updateInteractionViaMouse,
} from '../interaction-state'

const middle = (rect: DOMRect): { x: number; y: number } => ({
  x: rect.x + rect.width / 2,
  y: rect.y + rect.height / 2,
})

const DraggedDivId = 'dragme-pls'

const codeElementWithNoSiblings = formatTestProjectCode(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
import { View, Rectangle } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <Scene
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 207,
        top: 126,
      }}
      data-label='Playground'
      data-uid='3fc'
    >
      <View
        style={{
          backgroundColor: '#7D94A7AB',
          display: 'flex',
          flexDirection: 'row',
          width: '100%',
          height: 404,
          left: 87,
          top: 281,
        }}
        data-uid='932'
      >
        <View
          style={{
            backgroundColor: '#7D94B7AB',
            width: '100%',
            height: 104,
            left: 86.5,
            top: 281,
          }}
          data-uid='fe1'
        />
        <View
          style={{
            backgroundColor: '#B37DB7AB',
            width: '100%',
            height: 104,
          }}
          data-uid='a50'
        >
          <div
            data-testid='${DraggedDivId}'
            data-uid='a75'
            style={{
              height: 54,
              width: 50,
              backgroundColor: '#EB0A0A',
              left: 245,
              top: 196,
            }}
          />
        </View>
        <View
          style={{
            backgroundColor: '#90B77DAB',
            width: '100%',
            height: 104,
            left: 87,
            top: 281,
          }}
          data-uid='ed3'
        />
      </View>
    </Scene>
  </Storyboard>
)
`)

const codeElementWithSibling = formatTestProjectCode(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
import { View, Rectangle } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <Scene
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 207,
        top: 126,
      }}
      data-label='Playground'
      data-uid='3fc'
    >
      <View
        style={{
          backgroundColor: '#7D94A7AB',
          display: 'flex',
          flexDirection: 'row',
          width: '100%',
          height: 404,
          left: 87,
          top: 281,
        }}
        data-uid='932'
      >
        <View
          style={{
            backgroundColor: '#7D94B7AB',
            width: '100%',
            height: 104,
            left: 86.5,
            top: 281,
          }}
          data-uid='fe1'
        />
        <View
          style={{
            backgroundColor: '#B37DB7AB',
            width: '100%',
            height: 104,
          }}
          data-uid='a50'
        >
          <Rectangle
            style={{
              backgroundColor: '#0091FFAA',
              left: 92.5,
              top: 15,
              width: 63,
              height: 56,
            }}
            data-uid='045'
          />
          <div
            data-uid='a75'
            data-testid='${DraggedDivId}'
            style={{
              height: 54,
              width: 50,
              backgroundColor: '#EB0A0A',
              left: 245,
              top: 196,
            }}
          />
        </View>
        <View
          style={{
            backgroundColor: '#90B77DAB',
            width: '100%',
            height: 104,
            left: 87,
            top: 281,
          }}
          data-uid='ed3'
        />
      </View>
    </Scene>
  </Storyboard>
)
`)

const codeDeeplyNestedElement = formatTestProjectCode(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
import { View, Rectangle } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <Scene
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 207,
        top: 126,
      }}
      data-label='Playground'
      data-uid='3fc'
    >
      <View
        style={{
          backgroundColor: '#7D94A7AB',
          display: 'flex',
          flexDirection: 'row',
          width: '100%',
          height: 404,
          left: 87,
          top: 281,
        }}
        data-uid='932'
      >
        <View
          style={{
            backgroundColor: '#7D94B7AB',
            width: '100%',
            height: 104,
            left: 86.5,
            top: 281,
          }}
          data-uid='fe1'
        />
        <View
          style={{
            backgroundColor: '#B98731E8',
            width: '100%',
            height: 114,
          }}
          data-uid='bb3'
        >
          <View
            style={{
              backgroundColor: '#B37DB7AB',
              width: '100%',
              height: 104,
            }}
            data-uid='a50'
          >
            <div
              data-uid='a75'
              data-testid='${DraggedDivId}'
              style={{
                height: 54,
                width: 50,
                backgroundColor: '#EB0A0A',
                left: 245,
                top: 196,
              }}
            />
          </View>
        </View>
        <View
          style={{
            backgroundColor: '#90B77DAB',
            width: '100%',
            height: 104,
            left: 87,
            top: 281,
          }}
          data-uid='ed3'
        />
      </View>
    </Scene>
  </Storyboard>
)
`)

async function startDragUsingActions(
  renderResult: EditorRenderResult,
  target: ElementPath,
  dragDelta: CanvasVector,
) {
  await renderResult.dispatch(selectComponents([target], false), true)
  const startInteractionSession = createInteractionViaMouse(
    zeroCanvasPoint,
    emptyModifiers,
    boundingArea(),
  )
  await renderResult.dispatch(
    [CanvasActions.createInteractionSession(startInteractionSession)],
    false,
  )
  await renderResult.getDispatchFollowUpActionsFinished()
  await renderResult.dispatch(
    [
      CanvasActions.updateInteractionSession(
        updateInteractionViaMouse(
          startInteractionSession,
          dragDelta,
          emptyModifiers,
          boundingArea(),
        ),
      ),
    ],
    false,
  )
  await renderResult.getDispatchFollowUpActionsFinished()
}

const pathForShallowNestedElement = EP.elementPath([['0cd', '3fc', '932', 'a50', 'a75']])
const pathForDeeplyNestedElement = EP.elementPath([['0cd', '3fc', '932', 'bb3', 'a50', 'a75']])

async function runTest(
  code: string,
  targetPath: ElementPath,
  check: (editor: EditorRenderResult) => void,
) {
  const editor = await renderTestEditorWithCode(code, 'await-first-dom-report')

  await editor.dispatch(selectComponents([targetPath], false), true)

  const divToBeDragged = editor.renderedDOM.queryByTestId(DraggedDivId)
  if (divToBeDragged == null) {
    throw new Error(`div with id ${DraggedDivId} should be there`)
  }

  const { x, y } = windowPoint(middle(divToBeDragged.getBoundingClientRect()))

  await startDragUsingActions(editor, targetPath, canvasPoint({ x: 20, y: 0 }))
  mouseDownAtPoint(divToBeDragged, { x: x, y: y })
  mouseMoveToPoint(divToBeDragged, { x: x + 20, y: y }, { eventOptions: { buttons: 1 } })

  await editor.getDispatchFollowUpActionsFinished()
  check(editor)
}

describe('finds an applicable strategy for the nearest parent', () => {
  it('element with no siblings', () =>
    runTest(codeElementWithNoSiblings, pathForShallowNestedElement, (editor) => {
      const strategies = editor.getEditorState().strategyState.sortedApplicableStrategies

      expect(strategies?.length).toEqual(2)
      if (strategies == null) {
        // here for type assertion
        throw new Error('`strategies` should not be null')
      }
      expect(strategies[0].strategy.id).toEqual('LOOK_FOR_APPLICABLE_PARENT_ID')
    }))

  it('deeply nested element with no siblings', () =>
    runTest(codeDeeplyNestedElement, pathForDeeplyNestedElement, (editor) => {
      const strategies = editor.getEditorState().strategyState.sortedApplicableStrategies

      expect(strategies?.length).toEqual(2)
      if (strategies == null) {
        // here for type assertion
        throw new Error('`strategies` should not be null')
      }
      expect(strategies[0].strategy.id).toEqual('LOOK_FOR_APPLICABLE_PARENT_ID')
    }))

  it('eleement with siblings', () =>
    runTest(codeElementWithSibling, pathForShallowNestedElement, (editor) => {
      const strategies = editor.getEditorState().strategyState.sortedApplicableStrategies

      expect(strategies?.length).toEqual(2)
      if (strategies == null) {
        // here for type assertion
        throw new Error('`strategies` should not be null')
      }
      expect(strategies[0].strategy.id).toEqual('FLOW_REORDER')
    }))
})
