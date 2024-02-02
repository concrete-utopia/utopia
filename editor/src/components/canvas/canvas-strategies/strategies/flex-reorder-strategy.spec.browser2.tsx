import { cmdModifier, emptyModifiers } from '../../../../utils/modifiers'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../ui-jsx.test-utils'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import {
  mouseClickAtPoint,
  mouseDownAtPoint,
  mouseDragFromPointWithDelta,
  pressKey,
} from '../../event-helpers.test-utils'
import { canvasPoint, windowPoint } from '../../../../core/shared/math-utils'
import {
  getClosingFragmentLikeTag,
  getOpeningFragmentLikeTag,
  getRegularNavigatorTargets,
} from './fragment-like-helpers.test-utils'
import type { FragmentLikeType } from './fragment-like-helpers'
import { AllFragmentLikeNonDomElementTypes, AllFragmentLikeTypes } from './fragment-like-helpers'
import { fromString } from '../../../../core/shared/element-path'
import { MoveReorderReparentIndicatorID } from '../../controls/select-mode/strategy-indicator'
import { CanvasToolbarEditButtonID } from '../../../../components/editor/canvas-toolbar'

const TestProject = (direction: string) => `
<div
  data-uid='aaa'
  style={{ display: 'flex', gap: 10, flexDirection: '${direction}' }}
>
  <div
    data-uid='child-0'
    style={{
      width: 50,
      height: 50,
      backgroundColor: 'green',
    }}
  />
  <div
    data-uid='child-1'
    data-testid='child-1'
    style={{
      width: 50,
      height: 50,
      backgroundColor: 'blue',
    }}
  />
  <div
    data-uid='child-2'
    style={{
      width: 50,
      height: 50,
      backgroundColor: 'purple',
    }}
  />
</div>`

function TestProjectWithFragment(type: FragmentLikeType, direction: string) {
  return `
<div
  data-uid='aaa'
  style={{ display: 'flex', gap: 10, flexDirection: '${direction}' }}
>
  <div
    data-uid='child-0'
      data-testid='child-0'
    style={{
      width: 50,
      height: 50,
      backgroundColor: 'green',
    }}
  />
  ${getOpeningFragmentLikeTag(type)}
    <div
      data-uid='fragment-child-1'
      data-testid='fragment-child-1'
      style={{
        width: 50,
        height: 50,
        backgroundColor: 'blue',
      }}
    />
    <div
      data-uid='fragment-child-2'
      data-testid='fragment-child-2'
      style={{
        width: 50,
        height: 50,
        backgroundColor: 'purple',
      }}
    />
    ${getClosingFragmentLikeTag(type)}
  <div
    data-uid='child-3'
    data-testid='child-3'
    style={{
      width: 50,
      height: 50,
      backgroundColor: 'yellow',
    }}
  />
</div>`
}

const TestProjectAbsoluteSibling = `
<div
  data-uid='aaa'
  style={{ display: 'flex', gap: 10 }}
>
  <div
    data-uid='absolute-child'
    style={{
      position: 'absolute',
      top: 100,
      left: 50,
      width: 50,
      height: 50,
      backgroundColor: 'yellow',
    }}
  />
  <div
    data-uid='child-0'
    style={{
      width: 50,
      height: 50,
      backgroundColor: 'green',
    }}
  />
  <div
    data-uid='child-1'
    data-testid='child-1'
    style={{
      width: 50,
      height: 50,
      backgroundColor: 'blue',
    }}
  />
  <div
    data-uid='child-2'
    style={{
      width: 50,
      height: 50,
      backgroundColor: 'purple',
    }}
  />
</div>
`

function TestProjectWithFragmentAbsoluteSibling(type: FragmentLikeType): string {
  return `
<div
  data-uid='aaa'
  style={{ display: 'flex', gap: 10 }}
>
  <div
    data-uid='absolute-child'
    style={{
      position: 'absolute',
      top: 100,
      left: 50,
      width: 50,
      height: 50,
      backgroundColor: 'yellow',
    }}
  />
  ${getOpeningFragmentLikeTag(type)}
    <div
      data-uid='fragment-child-1'
      data-testid='fragment-child-1'
      style={{
        width: 50,
        height: 50,
        backgroundColor: 'blue',
      }}
    />
    <div
      data-uid='fragment-child-2'
      data-testid='fragment-child-2'
      style={{
        width: 50,
        height: 50,
        backgroundColor: 'purple',
      }}
    />
  ${getClosingFragmentLikeTag(type)}
  <div
    data-uid='child-3'
      data-testid='child-3'
    style={{
      width: 50,
      height: 50,
      backgroundColor: 'purple',
    }}
  />
</div>
`
}

describe('Flex Reorder Strategy', () => {
  describe('normal reorder scenarios', () => {
    it('does not activate when drag threshold is not reached', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(TestProject('row')),
        'await-first-dom-report',
      )

      const targetElement = renderResult.renderedDOM.getByTestId('child-1')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
      await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
      await mouseDownAtPoint(canvasControlsLayer, startPoint, { modifiers: emptyModifiers })
      await mouseDragFromPointWithDelta(
        canvasControlsLayer,
        startPoint,
        canvasPoint({ x: 1, y: 1 }),
        {
          modifiers: emptyModifiers,
        },
      )

      await renderResult.getDispatchFollowUpActionsFinished()
      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(TestProject('row')),
      )
    })
    it('works with normal direction', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(TestProject('row')),
        'await-first-dom-report',
      )

      const targetElement = renderResult.renderedDOM.getByTestId('child-1')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
      await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
      await mouseDragFromPointWithDelta(
        canvasControlsLayer,
        startPoint,
        canvasPoint({ x: 62, y: 0 }),
        {
          modifiers: emptyModifiers,
          midDragCallback: async () => {
            const moveReorderReparentIndicator = renderResult.renderedDOM.getByTestId(
              MoveReorderReparentIndicatorID,
            )
            expect(moveReorderReparentIndicator.innerText).toEqual('Reordering Flex Elements')
            const toolbarEditButtonImage = renderResult.renderedDOM.getByTestId(
              `${CanvasToolbarEditButtonID}-icon`,
            )
            expect(toolbarEditButtonImage.getAttribute('src')).toEqual(
              expect.stringContaining('reorder'), // check if the toolbar shows the Reorder icon
            )
            expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
              'FLEX_REORDER',
            )
            expect(
              renderResult.getEditorState().strategyState.customStrategyState?.lastReorderIdx,
            ).toEqual(2)
          },
        },
      )

      await renderResult.getDispatchFollowUpActionsFinished()
      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{ display: 'flex', gap: 10, flexDirection: 'row' }}
      >
        <div
          data-uid='child-0'
          style={{
            width: 50,
            height: 50,
            backgroundColor: 'green',
          }}
        />
        <div
          data-uid='child-2'
          style={{
            width: 50,
            height: 50,
            backgroundColor: 'purple',
          }}
        />
        <div
          data-uid='child-1'
          data-testid='child-1'
          style={{
            width: 50,
            height: 50,
            backgroundColor: 'blue',
          }}
        />
      </div>`),
      )
    })
    it('excludes absolute siblings', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(TestProjectAbsoluteSibling),
        'await-first-dom-report',
      )

      const targetElement = renderResult.renderedDOM.getByTestId('child-1')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
      await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
      await mouseDragFromPointWithDelta(
        canvasControlsLayer,
        startPoint,
        canvasPoint({ x: 62, y: 0 }),
        {
          modifiers: emptyModifiers,
          midDragCallback: async () => {
            expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
              'FLEX_REORDER',
            )
            expect(
              renderResult.getEditorState().strategyState.customStrategyState?.lastReorderIdx,
            ).toEqual(3)
          },
        },
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{ display: 'flex', gap: 10 }}
      >
        <div
          data-uid='absolute-child'
          style={{
            position: 'absolute',
            top: 100,
            left: 50,
            width: 50,
            height: 50,
            backgroundColor: 'yellow',
          }}
        />
        <div
          data-uid='child-0'
          style={{
            width: 50,
            height: 50,
            backgroundColor: 'green',
          }}
        />
        <div
          data-uid='child-2'
          style={{
            width: 50,
            height: 50,
            backgroundColor: 'purple',
          }}
        />
        <div
          data-uid='child-1'
          data-testid='child-1'
          style={{
            width: 50,
            height: 50,
            backgroundColor: 'blue',
          }}
        />
      </div>`),
      )
    })
    it('works with reverse direction', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(TestProject('row-reverse')),
        'await-first-dom-report',
      )

      const targetElement = renderResult.renderedDOM.getByTestId('child-1')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
      await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
      await mouseDragFromPointWithDelta(
        canvasControlsLayer,
        startPoint,
        canvasPoint({ x: 62, y: 0 }),
        {
          modifiers: emptyModifiers,
          midDragCallback: async () => {
            expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
              'FLEX_REORDER',
            )
            expect(
              renderResult.getEditorState().strategyState.customStrategyState?.lastReorderIdx,
            ).toEqual(0)
          },
        },
      )

      await renderResult.getDispatchFollowUpActionsFinished()
      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{
          display: 'flex',
          gap: 10,
          flexDirection: 'row-reverse',
        }}
      >
        <div
          data-uid='child-1'
          data-testid='child-1'
          style={{
            width: 50,
            height: 50,
            backgroundColor: 'blue',
          }}
        />
        <div
          data-uid='child-0'
          style={{
            width: 50,
            height: 50,
            backgroundColor: 'green',
          }}
        />
        <div
          data-uid='child-2'
          style={{
            width: 50,
            height: 50,
            backgroundColor: 'purple',
          }}
        />
      </div>`),
      )
    })
  })

  describe('flex reorder, with a group-like element as a sibling', () => {
    AllFragmentLikeTypes.forEach((type) => {
      describe(`– group-like element ${type} –`, () => {
        it('works with normal direction', async () => {
          const renderResult = await renderTestEditorWithCode(
            makeTestProjectCodeWithSnippet(TestProjectWithFragment(type, 'row')),
            'await-first-dom-report',
          )

          await renderResult.getDispatchFollowUpActionsFinished()
          expect(getRegularNavigatorTargets(renderResult)).toEqual([
            'utopia-storyboard-uid/scene-aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/child-0',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment/fragment-child-1',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment/fragment-child-2',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/child-3', // <- child-3 starts out as the last element
          ])

          const targetElement = renderResult.renderedDOM.getByTestId('child-3')
          const targetElementBounds = targetElement.getBoundingClientRect()
          const canvasControlsLayer =
            renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

          const startPoint = windowPoint({
            x: targetElementBounds.x + 5,
            y: targetElementBounds.y + 5,
          })
          await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
          await mouseDragFromPointWithDelta(
            canvasControlsLayer,
            startPoint,
            canvasPoint({ x: -20, y: 0 }),
            {
              modifiers: emptyModifiers,
              midDragCallback: async () => {
                expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
                  'FLEX_REORDER',
                )
              },
            },
          )

          await renderResult.getDispatchFollowUpActionsFinished()
          expect(getRegularNavigatorTargets(renderResult)).toEqual([
            'utopia-storyboard-uid/scene-aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/child-0',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/child-3', // <- child-3 moves to the left of the fragment
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment/fragment-child-1',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment/fragment-child-2',
          ])
        })

        it('excludes absolute siblings', async () => {
          const renderResult = await renderTestEditorWithCode(
            makeTestProjectCodeWithSnippet(TestProjectWithFragmentAbsoluteSibling(type)),
            'await-first-dom-report',
          )

          const targetElement = renderResult.renderedDOM.getByTestId('child-3')
          const targetElementBounds = targetElement.getBoundingClientRect()
          const canvasControlsLayer =
            renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

          const startPoint = windowPoint({
            x: targetElementBounds.x + 5,
            y: targetElementBounds.y + 5,
          })
          await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })

          await mouseDragFromPointWithDelta(
            canvasControlsLayer,
            startPoint,
            canvasPoint({ x: -20, y: 0 }),
            {
              modifiers: emptyModifiers,
              midDragCallback: async () => {
                expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
                  'FLEX_REORDER',
                )
              },
            },
          )

          await renderResult.getDispatchFollowUpActionsFinished()

          expect(getRegularNavigatorTargets(renderResult)).toEqual([
            'utopia-storyboard-uid/scene-aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/absolute-child',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/child-3', // <- child-3 moves to the left of the fragment
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment/fragment-child-1',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment/fragment-child-2',
          ])
        })

        it('works with reverse direction', async () => {
          const renderResult = await renderTestEditorWithCode(
            makeTestProjectCodeWithSnippet(TestProjectWithFragment(type, 'row-reverse')),
            'await-first-dom-report',
          )

          const targetElement = renderResult.renderedDOM.getByTestId('child-3')
          const targetElementBounds = targetElement.getBoundingClientRect()
          const canvasControlsLayer =
            renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

          const startPoint = windowPoint({
            x: targetElementBounds.x + 5,
            y: targetElementBounds.y + 5,
          })
          await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })

          await mouseDragFromPointWithDelta(
            canvasControlsLayer,
            startPoint,
            canvasPoint({ x: 100, y: 0 }),
            {
              modifiers: emptyModifiers,
              midDragCallback: async () => {
                expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
                  'FLEX_REORDER',
                )
              },
            },
          )

          await renderResult.getDispatchFollowUpActionsFinished()
          expect(getRegularNavigatorTargets(renderResult)).toEqual([
            'utopia-storyboard-uid/scene-aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/child-0',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/child-3', // <- child-3 moves to the left of the fragment, despite the drag delta pointing to the right
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment/fragment-child-1',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment/fragment-child-2',
          ])
        })
      })
    })
  })

  describe('projects with fragments, with fragments support enabled', () => {
    // we only run this test for non-dom elements, as a sizeless div in flex layout acts weird
    AllFragmentLikeNonDomElementTypes.forEach((type) => {
      describe(`– group-like element ${type} – `, () => {
        it('does not activate when drag threshold is not reached', async () => {
          const renderResult = await renderTestEditorWithCode(
            makeTestProjectCodeWithSnippet(TestProjectWithFragment(type, 'row')),
            'await-first-dom-report',
          )

          const targetElement = renderResult.renderedDOM.getByTestId('fragment-child-2')
          const targetElementBounds = targetElement.getBoundingClientRect()
          const canvasControlsLayer =
            renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

          const startPoint = windowPoint({
            x: targetElementBounds.x + 5,
            y: targetElementBounds.y + 5,
          })
          await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
          await pressKey('Escape')
          await mouseDownAtPoint(canvasControlsLayer, startPoint, { modifiers: emptyModifiers })
          await mouseDragFromPointWithDelta(
            canvasControlsLayer,
            startPoint,
            canvasPoint({ x: 1, y: 1 }),
            {
              modifiers: emptyModifiers,
            },
          )

          await renderResult.getDispatchFollowUpActionsFinished()
          expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
            makeTestProjectCodeWithSnippet(TestProjectWithFragment(type, 'row')),
          )
        })

        it('dragging the group-like element reorders the entire element', async () => {
          const renderResult = await renderTestEditorWithCode(
            makeTestProjectCodeWithSnippet(TestProjectWithFragment(type, 'row')),
            'await-first-dom-report',
          )

          const targetElement = renderResult.renderedDOM.getByTestId('fragment-child-2')
          const targetElementBounds = targetElement.getBoundingClientRect()
          const canvasControlsLayer =
            renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

          const startPoint = windowPoint({
            x: targetElementBounds.x + 5,
            y: targetElementBounds.y + 5,
          })

          await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
          await pressKey('Escape')
          await pressKey('Escape')
          await renderResult.getDispatchFollowUpActionsFinished()

          // make sure we reall selected the fragment-like element
          expect(renderResult.getEditorState().editor.selectedViews).toEqual([
            fromString('utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like'),
          ])

          await mouseDragFromPointWithDelta(
            canvasControlsLayer,
            startPoint,
            canvasPoint({ x: 62, y: 0 }),
            {
              modifiers: emptyModifiers,
              midDragCallback: async () => {
                expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
                  'FLEX_REORDER',
                )
                expect(
                  renderResult.getEditorState().strategyState.customStrategyState?.lastReorderIdx,
                ).toEqual(2)
              },
            },
          )

          await renderResult.getDispatchFollowUpActionsFinished()
          expect(getRegularNavigatorTargets(renderResult)).toEqual([
            'utopia-storyboard-uid/scene-aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/child-0',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/child-3',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like', // <- fragment-like moves right of child-3
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment/fragment-child-1',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment/fragment-child-2',
          ])
        })

        it('dragging root fragment of conditional active branch reparents the conditional itself', async () => {
          const renderResult = await renderTestEditorWithCode(
            makeTestProjectCodeWithSnippet(TestProjectWithFragment(type, 'row')),
            'await-first-dom-report',
          )

          const targetElement = renderResult.renderedDOM.getByTestId('fragment-child-2')
          const targetElementBounds = targetElement.getBoundingClientRect()
          const canvasControlsLayer =
            renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

          const startPoint = windowPoint({
            x: targetElementBounds.x + 5,
            y: targetElementBounds.y + 5,
          })

          await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
          await pressKey('Escape')
          await renderResult.getDispatchFollowUpActionsFinished()

          // make sure we reall selected the fragment-like element
          expect(renderResult.getEditorState().editor.selectedViews).toEqual([
            fromString(
              'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment',
            ),
          ])

          await mouseDragFromPointWithDelta(
            canvasControlsLayer,
            startPoint,
            canvasPoint({ x: 62, y: 0 }),
            {
              modifiers: emptyModifiers,
              midDragCallback: async () => {
                expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
                  'FLEX_REORDER',
                )
                expect(
                  renderResult.getEditorState().strategyState.customStrategyState?.lastReorderIdx,
                ).toEqual(2)
              },
            },
          )

          await renderResult.getDispatchFollowUpActionsFinished()
          expect(getRegularNavigatorTargets(renderResult)).toEqual([
            'utopia-storyboard-uid/scene-aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/child-0',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/child-3',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like', // <- fragment-like moves right of child-3
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment/fragment-child-1',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment/fragment-child-2',
          ])
        })

        it('excludes absolute siblings', async () => {
          const renderResult = await renderTestEditorWithCode(
            makeTestProjectCodeWithSnippet(TestProjectWithFragmentAbsoluteSibling(type)),
            'await-first-dom-report',
          )

          const targetElement = renderResult.renderedDOM.getByTestId('fragment-child-2')
          const targetElementBounds = targetElement.getBoundingClientRect()
          const canvasControlsLayer =
            renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

          const startPoint = windowPoint({
            x: targetElementBounds.x + 5,
            y: targetElementBounds.y + 5,
          })
          await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
          await pressKey('Escape')
          await pressKey('Escape')
          await renderResult.getDispatchFollowUpActionsFinished()

          // make sure we reall selected the fragment-like element
          expect(renderResult.getEditorState().editor.selectedViews).toEqual([
            fromString('utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like'),
          ])

          await mouseDragFromPointWithDelta(
            canvasControlsLayer,
            startPoint,
            canvasPoint({ x: 62, y: 0 }),
            {
              modifiers: emptyModifiers,
              midDragCallback: async () => {
                expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
                  'FLEX_REORDER',
                )
                expect(
                  renderResult.getEditorState().strategyState.customStrategyState?.lastReorderIdx,
                ).toEqual(2)
              },
            },
          )

          await renderResult.getDispatchFollowUpActionsFinished()

          expect(getRegularNavigatorTargets(renderResult)).toEqual([
            'utopia-storyboard-uid/scene-aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/absolute-child',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/child-3', // <- child-3 moves to the left of the fragment
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment/fragment-child-1',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment/fragment-child-2',
          ])
        })

        it('works with reverse direction', async () => {
          const renderResult = await renderTestEditorWithCode(
            makeTestProjectCodeWithSnippet(TestProjectWithFragment(type, 'row-reverse')),
            'await-first-dom-report',
          )

          const targetElement = renderResult.renderedDOM.getByTestId('fragment-child-2')
          const targetElementBounds = targetElement.getBoundingClientRect()
          const canvasControlsLayer =
            renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

          const startPoint = windowPoint({
            x: targetElementBounds.x + 5,
            y: targetElementBounds.y + 5,
          })
          await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
          await pressKey('Escape')
          await pressKey('Escape')
          await renderResult.getDispatchFollowUpActionsFinished()

          // make sure we reall selected the fragment-like element
          expect(renderResult.getEditorState().editor.selectedViews).toEqual([
            fromString('utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like'),
          ])

          await mouseDragFromPointWithDelta(
            canvasControlsLayer,
            startPoint,
            canvasPoint({ x: 120, y: 0 }),
            {
              modifiers: emptyModifiers,
              midDragCallback: async () => {
                expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
                  'FLEX_REORDER',
                )
                expect(
                  renderResult.getEditorState().strategyState.customStrategyState?.lastReorderIdx,
                ).toEqual(0)
              },
            },
          )

          await renderResult.getDispatchFollowUpActionsFinished()
          expect(getRegularNavigatorTargets(renderResult)).toEqual([
            'utopia-storyboard-uid/scene-aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like', // <- succesfully reparented to zero position
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment/fragment-child-1',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/fragment-like/inner-fragment/fragment-child-2',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/child-0',
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/child-3',
          ])
        })
      })
    })
  })
})
