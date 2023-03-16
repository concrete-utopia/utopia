import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import {
  TestSceneUID,
  renderTestEditorWithCode,
  EditorRenderResult,
  getPrintedUiJsCode,
} from '../../components/canvas/ui-jsx.test-utils'
import { offsetPoint, windowPoint, WindowPoint } from '../../core/shared/math-utils'
import { fireEvent } from '@testing-library/react'
import { selectComponents } from '../editor/actions/meta-actions'
import * as EP from '../../core/shared/element-path'
import { act } from 'react-dom/test-utils'
import {
  conditionalClauseNavigatorEntry,
  DerivedState,
  EditorState,
  navigatorEntryToKey,
  regularNavigatorEntry,
  syntheticNavigatorEntry,
  varSafeNavigatorEntryToKey,
} from '../editor/store/editor-state'
import { getDomRectCenter } from '../../core/shared/dom-utils'
import { setFeatureForBrowserTests } from '../../utils/utils.test-utils'
import { navigatorDepth } from './navigator-utils'
import { compose3Optics, Optic } from '../../core/shared/optics/optics'
import { ChildOrAttribute } from '../../core/shared/element-template'
import { forElementOptic } from '../../core/model/common-optics'
import { unsafeGet } from '../../core/shared/optics/optic-utilities'
import {
  conditionalWhenFalseOptic,
  jsxConditionalExpressionOptic,
} from '../../core/model/conditionals'

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
      data-testid='${TestSceneUID}'
    >
      <div
        style={{
          height: '100%',
          width: '100%',
          contain: 'layout',
        }}
        data-uid='containing-div'
        data-testid='containing-div'
      >
        {
          // @utopia/uid=conditional1
          [].length === 0 ? (
            // @utopia/uid=conditional2
            [].length === 0 ? (
              <div
                style={{
                  height: 150,
                  width: 150,
                  position: 'absolute',
                  left: 154,
                  top: 134,
                  backgroundColor: 'lightblue',
                }}
                data-uid='then-then-div'
                data-testid='then-then-div'
              />
            ) : null
          ) : (
            <div
              style={{
                height: 150,
                position: 'absolute',
                left: 154,
                top: 134,
              }}
              data-uid='else-div'
              data-testid='else-div'
            />
          )}
        <div
          style={{
            height: 150,
            width: 150,
            position: 'absolute',
            left: 300,
            top: 300,
            backgroundColor: 'darkblue',
          }}
          data-uid='sibling-div'
          data-testid='sibling-div'
        />
      </div>
    </Scene>
  </Storyboard>
)
`
}

function getProjectCodeEmptyActive(): string {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

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
      data-testid='${TestSceneUID}'
    >
      <div
        style={{
          height: '100%',
          width: '100%',
          contain: 'layout',
        }}
        data-uid='containing-div'
        data-testid='containing-div'
      >
        {
          // @utopia/uid=conditional1
          [].length === 0 ? (
            // @utopia/uid=conditional2
            [].length === 0 ? null : null
          ) : (
            <div
              style={{
                height: 150,
                position: 'absolute',
                left: 154,
                top: 134,
              }}
              data-uid='else-div'
              data-testid='else-div'
            />
          )
        }
        <div
          style={{
            height: 150,
            width: 150,
            position: 'absolute',
            left: 300,
            top: 300,
            backgroundColor: 'darkblue',
          }}
          data-uid='sibling-div'
          data-testid='sibling-div'
        />
      </div>
    </Scene>
  </Storyboard>
)
`
}

function navigatorStructure(editorState: EditorState, deriveState: DerivedState): string {
  const lines = deriveState.visibleNavigatorTargets.map((target) => {
    const targetAsText = navigatorEntryToKey(target)
    let prefix: string = ''
    const depth = navigatorDepth(target, editorState.jsxMetadata)
    for (let index = 0; index < depth; index++) {
      prefix = prefix.concat('  ')
    }
    return `${prefix}${targetAsText}`
  })
  return lines.join('\n')
}

describe('conditionals in the navigator', () => {
  setFeatureForBrowserTests('Conditional support', true)
  setFeatureForBrowserTests('Fragment support', true)
  xit('dragging into a non-empty clause, creates a fragment wrapper', async () => {
    // TODO: Fill this out.
  })
  it('dragging into an empty active clause, takes the place of the empty value', async () => {
    const renderResult = await renderTestEditorWithCode(
      getProjectCodeEmptyActive(),
      'await-first-dom-report',
    )

    expect(
      navigatorStructure(
        renderResult.getEditorState().editor,
        renderResult.getEditorState().derived,
      ),
    ).toEqual(`  regular-utopia-storyboard-uid/scene-aaa
    regular-utopia-storyboard-uid/scene-aaa/containing-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-true-case
          regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-true-case
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/true-case-attribute
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-false-case
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/false-case-attribute
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/else-div-element-else-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div`)

    // Select the entry we plan to drag.
    const elementPathToDrag = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/containing-div/sibling-div`,
    )
    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch(selectComponents([elementPathToDrag], false), false)
      await dispatchDone
    })

    // Getting info relating to what element will be dragged.
    const navigatorEntryToDrag = await renderResult.renderedDOM.findByTestId(
      `navigator-item-${varSafeNavigatorEntryToKey(regularNavigatorEntry(elementPathToDrag))}`,
    )
    const navigatorEntryToDragRect = navigatorEntryToDrag.getBoundingClientRect()
    const navigatorEntryToDragCenter = getDomRectCenter(navigatorEntryToDragRect)

    // Getting info relating to where the element will be dragged to.
    const elementPathToTarget = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/containing-div/conditional1/conditional2`,
    )
    const navigatorEntryToTarget = await renderResult.renderedDOM.findByTestId(
      `navigator-item-${varSafeNavigatorEntryToKey(
        conditionalClauseNavigatorEntry(elementPathToTarget, 'false-case'),
      )}`,
    )
    const navigatorEntryToTargetRect = navigatorEntryToTarget.getBoundingClientRect()
    const navigatorEntryToTargetCenter = getDomRectCenter(navigatorEntryToTargetRect)

    const dragDelta = {
      x: navigatorEntryToTargetCenter.x - navigatorEntryToDragCenter.x,
      y: navigatorEntryToTargetCenter.y - navigatorEntryToDragCenter.y,
    }

    await act(async () =>
      dragElement(
        renderResult,
        `navigator-item-${varSafeNavigatorEntryToKey(regularNavigatorEntry(elementPathToDrag))}`,
        `navigator-item-${varSafeNavigatorEntryToKey(
          conditionalClauseNavigatorEntry(elementPathToTarget, 'false-case'),
        )}`,
        windowPoint(navigatorEntryToDragCenter),
        windowPoint(dragDelta),
        'apply-hover-events',
      ),
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    if (getPrintedUiJsCode(renderResult.getEditorState()) === getProjectCodeEmptyActive()) {
      throw new Error(`Code is unchanged.`)
    }

    expect(
      navigatorStructure(
        renderResult.getEditorState().editor,
        renderResult.getEditorState().derived,
      ),
    ).toEqual(`  regular-utopia-storyboard-uid/scene-aaa
    regular-utopia-storyboard-uid/scene-aaa/containing-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-true-case
          regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-true-case
              regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/sibling-div
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-false-case
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/false-case-attribute
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/else-div-element-else-div`)
  })
  it('dragging into an empty inactive clause, takes the place of the empty value', async () => {
    const renderResult = await renderTestEditorWithCode(getProjectCode(), 'await-first-dom-report')

    expect(
      navigatorStructure(
        renderResult.getEditorState().editor,
        renderResult.getEditorState().derived,
      ),
    ).toEqual(`  regular-utopia-storyboard-uid/scene-aaa
    regular-utopia-storyboard-uid/scene-aaa/containing-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-true-case
          regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-true-case
              regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/then-then-div
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-false-case
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/false-case-attribute
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/else-div-element-else-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div`)

    // Select the entry we plan to drag.
    const elementPathToDrag = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/containing-div/sibling-div`,
    )
    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch(selectComponents([elementPathToDrag], false), false)
      await dispatchDone
    })

    // Getting info relating to what element will be dragged.
    const navigatorEntryToDrag = await renderResult.renderedDOM.findByTestId(
      `navigator-item-${varSafeNavigatorEntryToKey(regularNavigatorEntry(elementPathToDrag))}`,
    )
    const navigatorEntryToDragRect = navigatorEntryToDrag.getBoundingClientRect()
    const navigatorEntryToDragCenter = getDomRectCenter(navigatorEntryToDragRect)

    // Getting info relating to where the element will be dragged to.
    const elementPathToTarget = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/containing-div/conditional1/conditional2`,
    )
    const navigatorEntryToTarget = await renderResult.renderedDOM.findByTestId(
      `navigator-item-${varSafeNavigatorEntryToKey(
        conditionalClauseNavigatorEntry(elementPathToTarget, 'false-case'),
      )}`,
    )
    const navigatorEntryToTargetRect = navigatorEntryToTarget.getBoundingClientRect()
    const navigatorEntryToTargetCenter = getDomRectCenter(navigatorEntryToTargetRect)

    const dragDelta = {
      x: navigatorEntryToTargetCenter.x - navigatorEntryToDragCenter.x,
      y: navigatorEntryToTargetCenter.y - navigatorEntryToDragCenter.y,
    }

    await act(async () =>
      dragElement(
        renderResult,
        `navigator-item-${varSafeNavigatorEntryToKey(regularNavigatorEntry(elementPathToDrag))}`,
        `navigator-item-${varSafeNavigatorEntryToKey(
          conditionalClauseNavigatorEntry(elementPathToTarget, 'false-case'),
        )}`,
        windowPoint(navigatorEntryToDragCenter),
        windowPoint(dragDelta),
        'apply-hover-events',
      ),
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    if (getPrintedUiJsCode(renderResult.getEditorState()) === getProjectCode()) {
      throw new Error(`Code is unchanged.`)
    }

    expect(
      navigatorStructure(
        renderResult.getEditorState().editor,
        renderResult.getEditorState().derived,
      ),
    ).toEqual(`  regular-utopia-storyboard-uid/scene-aaa
    regular-utopia-storyboard-uid/scene-aaa/containing-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-true-case
          regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-true-case
              regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/then-then-div
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-false-case
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/false-case-attribute
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/else-div-element-else-div`)
  })
  it('dragging out of an inactive clause, replaces with null', async () => {
    const renderResult = await renderTestEditorWithCode(getProjectCode(), 'await-first-dom-report')

    expect(
      navigatorStructure(
        renderResult.getEditorState().editor,
        renderResult.getEditorState().derived,
      ),
    ).toEqual(`  regular-utopia-storyboard-uid/scene-aaa
    regular-utopia-storyboard-uid/scene-aaa/containing-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-true-case
          regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-true-case
              regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/then-then-div
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-false-case
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/false-case-attribute
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/else-div-element-else-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div`)

    // Select the entry we plan to drag.
    const elementPathToDrag = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/containing-div/conditional1/else-div`,
    )

    // Need the underlying value in the clause to be able to construct the navigator entry.
    const inactiveElementOptic: Optic<EditorState, ChildOrAttribute> = compose3Optics(
      forElementOptic(EP.parentPath(elementPathToDrag)),
      jsxConditionalExpressionOptic,
      conditionalWhenFalseOptic,
    )
    const inactiveElement = unsafeGet(inactiveElementOptic, renderResult.getEditorState().editor)

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch(selectComponents([elementPathToDrag], false), false)
      await dispatchDone
    })

    // Getting info relating to what element will be dragged.
    const navigatorEntryToDrag = await renderResult.renderedDOM.findByTestId(
      `navigator-item-${varSafeNavigatorEntryToKey(
        syntheticNavigatorEntry(elementPathToDrag, inactiveElement),
      )}`,
    )
    const navigatorEntryToDragRect = navigatorEntryToDrag.getBoundingClientRect()
    const navigatorEntryToDragCenter = getDomRectCenter(navigatorEntryToDragRect)

    // Getting info relating to where the element will be dragged to.
    const elementPathToTarget = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/containing-div`,
    )
    const navigatorEntryToTarget = await renderResult.renderedDOM.findByTestId(
      `navigator-item-${varSafeNavigatorEntryToKey(regularNavigatorEntry(elementPathToTarget))}`,
    )
    const navigatorEntryToTargetRect = navigatorEntryToTarget.getBoundingClientRect()
    const navigatorEntryToTargetCenter = getDomRectCenter(navigatorEntryToTargetRect)

    const dragDelta = {
      x: navigatorEntryToTargetCenter.x - navigatorEntryToDragCenter.x,
      y: navigatorEntryToTargetCenter.y - navigatorEntryToDragCenter.y,
    }

    await act(async () =>
      dragElement(
        renderResult,
        `navigator-item-${varSafeNavigatorEntryToKey(
          syntheticNavigatorEntry(elementPathToDrag, inactiveElement),
        )}`,
        `navigator-item-${varSafeNavigatorEntryToKey(regularNavigatorEntry(elementPathToTarget))}`,
        windowPoint(navigatorEntryToDragCenter),
        windowPoint(dragDelta),
        'apply-hover-events',
      ),
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    if (getPrintedUiJsCode(renderResult.getEditorState()) === getProjectCode()) {
      throw new Error(`Code is unchanged.`)
    }

    expect(
      navigatorStructure(
        renderResult.getEditorState().editor,
        renderResult.getEditorState().derived,
      ),
    ).toEqual(`  regular-utopia-storyboard-uid/scene-aaa
    regular-utopia-storyboard-uid/scene-aaa/containing-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-true-case
          regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-true-case
              regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/then-then-div
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-false-case
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/false-case-attribute
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/false-case-attribute
      regular-utopia-storyboard-uid/scene-aaa/containing-div/else-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div`)
  })
  it('dragging out of an active clause, replaces with null', async () => {
    const renderResult = await renderTestEditorWithCode(getProjectCode(), 'await-first-dom-report')

    expect(
      navigatorStructure(
        renderResult.getEditorState().editor,
        renderResult.getEditorState().derived,
      ),
    ).toEqual(`  regular-utopia-storyboard-uid/scene-aaa
    regular-utopia-storyboard-uid/scene-aaa/containing-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-true-case
          regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-true-case
              regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/then-then-div
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-false-case
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/false-case-attribute
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/else-div-element-else-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div`)

    // Select the entry we plan to drag.
    const elementPathToDrag = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/containing-div/conditional1/conditional2/then-then-div`,
    )
    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch(selectComponents([elementPathToDrag], false), false)
      await dispatchDone
    })

    // Getting info relating to what element will be dragged.
    const navigatorEntryToDrag = await renderResult.renderedDOM.findByTestId(
      `navigator-item-${varSafeNavigatorEntryToKey(regularNavigatorEntry(elementPathToDrag))}`,
    )
    const navigatorEntryToDragRect = navigatorEntryToDrag.getBoundingClientRect()
    const navigatorEntryToDragCenter = getDomRectCenter(navigatorEntryToDragRect)

    // Getting info relating to where the element will be dragged to.
    const elementPathToTarget = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/containing-div`,
    )
    const navigatorEntryToTarget = await renderResult.renderedDOM.findByTestId(
      `navigator-item-${varSafeNavigatorEntryToKey(regularNavigatorEntry(elementPathToTarget))}`,
    )
    const navigatorEntryToTargetRect = navigatorEntryToTarget.getBoundingClientRect()
    const navigatorEntryToTargetCenter = getDomRectCenter(navigatorEntryToTargetRect)

    const dragDelta = {
      x: navigatorEntryToTargetCenter.x - navigatorEntryToDragCenter.x,
      y: navigatorEntryToTargetCenter.y - navigatorEntryToDragCenter.y,
    }

    await act(async () =>
      dragElement(
        renderResult,
        `navigator-item-${varSafeNavigatorEntryToKey(regularNavigatorEntry(elementPathToDrag))}`,
        `navigator-item-${varSafeNavigatorEntryToKey(regularNavigatorEntry(elementPathToTarget))}`,
        windowPoint(navigatorEntryToDragCenter),
        windowPoint(dragDelta),
        'apply-hover-events',
      ),
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    if (getPrintedUiJsCode(renderResult.getEditorState()) === getProjectCode()) {
      throw new Error(`Code is unchanged.`)
    }

    expect(
      navigatorStructure(
        renderResult.getEditorState().editor,
        renderResult.getEditorState().derived,
      ),
    ).toEqual(`  regular-utopia-storyboard-uid/scene-aaa
    regular-utopia-storyboard-uid/scene-aaa/containing-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-true-case
          regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-true-case
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/true-case-attribute
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-false-case
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/false-case-attribute
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/else-div-element-else-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/then-then-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div`)
  })
  xit('dragging into child of an active clause, works as it would without the conditional', () => {
    // TODO: Fill this out.
  })
})
