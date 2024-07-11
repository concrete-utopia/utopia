import * as EP from '../../../../core/shared/element-path'
import type { WindowPoint } from '../../../../core/shared/math-utils'
import { windowPoint } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { Modifiers } from '../../../../utils/modifiers'
import { ctrlModifier, shiftModifier } from '../../../../utils/modifiers'
import { selectComponents } from '../../../editor/actions/meta-actions'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import {
  keyDown,
  mouseDownAtPoint,
  mouseDragFromPointWithDelta,
  mouseMoveToPoint,
} from '../../event-helpers.test-utils'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import {
  formatTestProjectCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../ui-jsx.test-utils'

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
          data-uid='ed9'
        >
          <div
            data-testid='${DraggedDivId}'
            data-uid='a75'
            style={{
              height: '100%',
              width: '100%',
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

const codeElementWithNoSiblingsAndMatchingOnlyWidth =
  formatTestProjectCode(`import * as React from 'react'
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
          backgroundColor: '#B37DB7AB',
          width: 100,
          height: 100,
        }}
        data-uid='ed9'
      >
        <div
          data-testid='${DraggedDivId}'
          data-uid='a75'
          style={{
            height: 50,
            width: '100%',
            backgroundColor: '#EB0A0A',
            left: 245,
            top: 196,
          }}
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
          data-uid='ed9'
        >
          <Rectangle
            style={{
              backgroundColor: '#aaaaaa33',
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
            height: 104,
          }}
          data-uid='bb3'
        >
          <View
            style={{
              backgroundColor: '#B37DB7AB',
              width: '100%',
              height: 104,
            }}
            data-uid='ed9'
          >
            <div
              data-uid='a75'
              data-testid='${DraggedDivId}'
              style={{
                height: '100%',
                width: '100%',
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

const TestProjectAbsoluteAndFlow = `
<div style={{ width: '100%', height: '100%', position: 'absolute' }} data-uid='container'>
  <div
    style={{
      width: 200,
      height: 200,
      backgroundColor: '#CA1E4C80',
      position: 'absolute'
    }}
    data-uid='aaa'
  >
    <div
      style={{
        width: '100%',
        height: '100%',
        backgroundColor: '#297374',
      }}
      data-uid='bbb'
      data-testid='bbb'
    />
  </div>
  <div
    style={{
      width: 200,
      height: 200,
      backgroundColor: '#FF2200AB',
    }}
    data-uid='ccc'
  >
    <div
      style={{
        width: '100%',
        height: '100%',
        backgroundColor: '#FFE600AB',
      }}
      data-uid='ddd'
      data-testid='ddd'
    />
  </div>
  <div
    style={{
      width: 200,
      height: 200,
      backgroundColor: '#00FFA2AB',
    }}
    data-uid='eee'
  />
</div>
`

const TestProjectFragmentWithAbsoluteChildren = `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
import { View, Rectangle } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 207,
        top: 126,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <div
        style={{
          height: '100%',
          width: '100%',
          contain: 'layout',
        }}
        data-uid='container'
      >
        <div
          style={{
            position: 'absolute',
            left: 359,
            top: 57,
            width: 196,
            height: 82,
          }}
          data-uid='outer'
        >
          <React.Fragment data-uid='fragment'>
            <div
              data-testid='${DraggedDivId}'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                flexBasis: 80,
                height: 28,
                width: 40,
                left: 27,
                top: 37,
              }}
              data-uid='child1'
            />
            <div
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                flexBasis: 80,
                height: 28,
                width: 40,
                left: 27,
                top: 37,
              }}
              data-uid='child2'
            />
          </React.Fragment>
        </div>
      </div>
    </Scene>
  </Storyboard>
)
`

const TestProjectFragmentWithNoSiblings = `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
import { View, Rectangle } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 207,
        top: 126,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <div
        style={{
          height: '100%',
          width: '100%',
          contain: 'layout',
        }}
        data-uid='container'
      >
        <div
          style={{
            top: 73,
            left: 63,
            width: 200,
            height: 200,
            position: 'absolute',
          }}
          data-uid='outer'
        >
          <React.Fragment data-uid='fragment'>
            <div
              data-testid='${DraggedDivId}'
              style={{
                backgroundColor: 'red',
                flexBasis: 80,
                height: '100%',
                width: '100%',
              }}
              data-uid='child'
            />
          </React.Fragment>
        </div>
      </div>
    </Scene>
  </Storyboard>
)
`

async function dragElement(
  canvasControlsLayer: HTMLElement,
  startPoint: WindowPoint,
  dragDelta: WindowPoint,
  midDragCallback?: () => Promise<void>,
  modifiers?: Modifiers,
): Promise<void> {
  await mouseDownAtPoint(canvasControlsLayer, startPoint)
  await mouseDragFromPointWithDelta(canvasControlsLayer, startPoint, dragDelta, {
    midDragCallback: midDragCallback,
  })
}

const pathForShallowNestedElement = EP.elementPath([['0cd', '3fc', '932', 'ed9', 'a75']])
const pathForDeeplyNestedElement = EP.elementPath([['0cd', '3fc', '932', 'bb3', 'ed9', 'a75']])

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

  const startPoint = windowPoint(middle(divToBeDragged.getBoundingClientRect()))
  const { x, y } = startPoint

  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  await dragElement(
    canvasControlsLayer,
    startPoint,
    windowPoint({ x: 20, y: 0 }),
    async () => {
      await mouseDownAtPoint(divToBeDragged, { x: x, y: y }, { modifiers: ctrlModifier })
      await mouseMoveToPoint(
        divToBeDragged,
        { x: x + 20, y: y },
        { eventOptions: { buttons: 1 }, modifiers: ctrlModifier },
      )

      check(editor)
    },
    ctrlModifier,
  )
}

describe('finds an applicable strategy for the nearest ancestor', () => {
  it('element with no siblings', () =>
    runTest(codeElementWithNoSiblings, pathForShallowNestedElement, (editor) => {
      const strategies = editor.getEditorState().strategyState.sortedApplicableStrategies

      expect(strategies?.length).toBeGreaterThan(1)
      if (strategies == null) {
        // here for type assertion
        throw new Error('`strategies` should not be null')
      }
      expect(strategies[0].strategy.id.endsWith('_ANCESTOR_1')).toBeTruthy()
    }))

  it('element with no siblings but only one matching dimension does not find ancestor strategy', () =>
    runTest(
      codeElementWithNoSiblingsAndMatchingOnlyWidth,
      pathForShallowNestedElement,
      (editor) => {
        const strategies = editor.getEditorState().strategyState.sortedApplicableStrategies

        expect(strategies?.length).toBeGreaterThan(0)
        if (strategies == null) {
          // here for type assertion
          throw new Error('`strategies` should not be null')
        }
        expect(strategies[0].strategy.id.includes('_ANCESTOR_')).toBeFalsy()
      },
    ))

  it('deeply nested element with no siblings', () =>
    runTest(codeDeeplyNestedElement, pathForDeeplyNestedElement, (editor) => {
      const strategies = editor.getEditorState().strategyState.sortedApplicableStrategies

      expect(strategies?.length).toBeGreaterThan(1)
      if (strategies == null) {
        // here for type assertion
        throw new Error('`strategies` should not be null')
      }
      expect(strategies[0].strategy.id.endsWith('_ANCESTOR_2')).toBeTruthy()
    }))

  it('element with siblings', () =>
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

describe('Fragments are transparent for ancestor metastrategy', () => {
  it('dragging a fragment with multiple absolute children trigger absolute move', () =>
    runTest(
      TestProjectFragmentWithAbsoluteChildren,
      EP.fromString('storyboard/scene/container/outer/fragment'),
      (editor) => {
        const strategies = editor.getEditorState().strategyState.sortedApplicableStrategies

        expect(strategies?.length).toBeGreaterThan(0)
        if (strategies == null) {
          // here for type assertion
          throw new Error('`strategies` should not be null')
        }
        expect(strategies[0].strategy.id).toEqual('ABSOLUTE_MOVE')
      },
    ))
  it('dragging a fragment with no siblings and single child triggers ancestor metastrategy', () =>
    runTest(
      TestProjectFragmentWithNoSiblings,
      EP.fromString('storyboard/scene/container/outer/fragment'),
      (editor) => {
        const strategies = editor.getEditorState().strategyState.sortedApplicableStrategies

        expect(strategies?.length).toBeGreaterThan(0)
        if (strategies == null) {
          // here for type assertion
          throw new Error('`strategies` should not be null')
        }
        expect(strategies[0].strategy.id.endsWith('_ANCESTOR_1')).toBeTruthy()
      },
    ))
})

describe('finds keyboard strategy for absolute ancestor', () => {
  it('element with absolute parent should trigger ancestor move', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectAbsoluteAndFlow),
      'await-first-dom-report',
    )
    await renderResult.dispatch(
      selectComponents(
        [EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/aaa/bbb')],
        false,
      ),
      true,
    )
    await renderResult.getDispatchFollowUpActionsFinished()

    keyDown('ArrowRight', { modifiers: shiftModifier })
    const strategies = renderResult.getEditorState().strategyState.sortedApplicableStrategies

    expect(strategies?.length).toBeGreaterThan(0)
    if (strategies == null) {
      // here for type assertion
      throw new Error('`strategies` should not be null')
    }
    expect(strategies[0].strategy.id.endsWith('_ANCESTOR_1')).toBeTruthy()
    expect(strategies[0].strategy.name).toContain('Move')
  })
  it('element with flow parent should trigger ancestor reorder', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectAbsoluteAndFlow),
      'await-first-dom-report',
    )
    await renderResult.dispatch(
      selectComponents(
        [EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/ccc/ddd')],
        false,
      ),
      true,
    )
    await renderResult.getDispatchFollowUpActionsFinished()

    keyDown('ArrowDown')
    const strategies = renderResult.getEditorState().strategyState.sortedApplicableStrategies

    expect(strategies?.length).toBeGreaterThan(0)
    if (strategies == null) {
      // here for type assertion
      throw new Error('`strategies` should not be null')
    }
    expect(strategies[0].strategy.id.endsWith('_ANCESTOR_1')).toBeTruthy()
    expect(strategies[0].strategy.name).toContain('Reorder')
  })
})
