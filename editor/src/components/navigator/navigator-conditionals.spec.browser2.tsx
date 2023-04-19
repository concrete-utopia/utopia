import { fireEvent, screen } from '@testing-library/react'
import { act } from 'react-dom/test-utils'
import {
  EditorRenderResult,
  TestSceneUID,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
} from '../../components/canvas/ui-jsx.test-utils'
import { forElementOptic } from '../../core/model/common-optics'
import {
  conditionalWhenFalseOptic,
  conditionalWhenTrueOptic,
  jsxConditionalExpressionOptic,
} from '../../core/model/conditionals'
import { FOR_TESTS_setNextGeneratedUids } from '../../core/model/element-template-utils.test-utils'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import { getDomRectCenter } from '../../core/shared/dom-utils'
import * as EP from '../../core/shared/element-path'
import { JSXElementChild, isJSXConditionalExpression } from '../../core/shared/element-template'
import { WindowPoint, offsetPoint, windowPoint } from '../../core/shared/math-utils'
import { fromTypeGuard } from '../../core/shared/optics/optic-creators'
import { unsafeGet } from '../../core/shared/optics/optic-utilities'
import { Optic, compose3Optics } from '../../core/shared/optics/optics'
import { ElementPath } from '../../core/shared/project-file-types'
import { getUtopiaID } from '../../core/shared/uid-utils'
import { emptyImports } from '../../core/workers/common/project-file-utils'
import { getTargetParentForPaste } from '../../utils/clipboard'
import { mouseClickAtPoint, pressKey } from '../canvas/event-helpers.test-utils'
import { elementPaste, pasteJSXElements } from '../editor/actions/action-creators'
import { selectComponents } from '../editor/actions/meta-actions'
import {
  DerivedState,
  EditorState,
  conditionalClauseNavigatorEntry,
  navigatorEntryToKey,
  regularNavigatorEntry,
  syntheticNavigatorEntry,
  varSafeNavigatorEntryToKey,
} from '../editor/store/editor-state'
import { InsertionPath, conditionalClauseInsertionPath } from '../editor/store/insertion-path'
import { NavigatorItemTestId } from './navigator-item/navigator-item'
import { navigatorDepth } from './navigator-utils'
import { TopDropTargetLineTestId } from './navigator-item/navigator-item-dnd-container'

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

function getProjectCodeTree(): string {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var ${BakedInStoryboardVariableName} = (
  <Storyboard data-uid='${BakedInStoryboardUID}'>
    <div data-uid='aaa'>foo</div>
    {
      // @utopia/uid=cond1
      true ? <div data-uid='bbb'>bbb</div> : null
    }
    <div data-uid='ccc'>
      <div data-uid='ddd'>ddd</div>
      {
        // @utopia/uid=cond2
        true ? (
          <div data-uid='eee'>
            {
              // @utopia/uid=cond3
              true ? <div data-uid='fff'>fff</div> : null
            }
            <div data-uid='ggg'>ggg</div>
          </div>
        ) : null
      }
    </div>
    {
      // @utopia/uid=cond4
      true ? <div data-uid='hhh'>hhh</div> : null
    }
    {
      // @utopia/uid=cond5
      true ? <div data-uid='iii'>iii</div> : null
    }
    <div data-uid='jjj'>jjj</div>
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

function getProjectCodeNotEmpty(): string {
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
            <div data-uid='then-div' data-testid='then-div'>foo</div>
          ) : (
            <div data-uid='else-div' data-testid='else-div'>bar</div>
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

function getProjectCodeEmptySingleConditional(): string {
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
          // @utopia/uid=conditional
          true ? null : null
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

async function ensureNoopDrag({
  projectCode,
  pathToDrag,
  dropTargetID,
}: {
  projectCode: string
  pathToDrag: ElementPath
  dropTargetID: string
}) {
  const renderResult = await renderTestEditorWithCode(projectCode, 'await-first-dom-report')

  await act(async () => {
    const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
    await renderResult.dispatch(selectComponents([pathToDrag], false), false)
    await dispatchDone
  })

  // Getting info relating to what element will be dragged.
  const navigatorEntryToDrag = await renderResult.renderedDOM.findByTestId(
    `navigator-item-${varSafeNavigatorEntryToKey(regularNavigatorEntry(pathToDrag))}`,
  )
  const navigatorEntryToDragRect = navigatorEntryToDrag.getBoundingClientRect()
  const navigatorEntryToDragCenter = getDomRectCenter(navigatorEntryToDragRect)

  // Getting info relating to where the element will be dragged to.
  const navigatorEntryToTarget = await renderResult.renderedDOM.findByTestId(dropTargetID)

  const navigatorEntryToTargetRect = navigatorEntryToTarget.getBoundingClientRect()
  const navigatorEntryToTargetCenter = getDomRectCenter(navigatorEntryToTargetRect)

  const dragDelta = {
    x: navigatorEntryToTargetCenter.x - navigatorEntryToDragCenter.x,
    y: navigatorEntryToTargetCenter.y - navigatorEntryToDragCenter.y,
  }

  await act(async () =>
    dragElement(
      renderResult,
      `navigator-item-${varSafeNavigatorEntryToKey(regularNavigatorEntry(pathToDrag))}`,
      dropTargetID,
      windowPoint(navigatorEntryToDragCenter),
      windowPoint(dragDelta),
      'apply-hover-events',
    ),
  )

  await renderResult.getDispatchFollowUpActionsFinished()

  return getPrintedUiJsCode(renderResult.getEditorState())
}

describe('conditionals in the navigator', () => {
  it('keeps conditionals position', async () => {
    const renderResult = await renderTestEditorWithCode(
      getProjectCodeTree(),
      'await-first-dom-report',
    )
    const want = `  regular-utopia-storyboard-uid/aaa
  regular-utopia-storyboard-uid/cond1
    conditional-clause-utopia-storyboard-uid/cond1-true-case
      regular-utopia-storyboard-uid/cond1/bbb
    conditional-clause-utopia-storyboard-uid/cond1-false-case
      synthetic-utopia-storyboard-uid/cond1/a25-attribute
  regular-utopia-storyboard-uid/ccc
    regular-utopia-storyboard-uid/ccc/ddd
    regular-utopia-storyboard-uid/ccc/cond2
      conditional-clause-utopia-storyboard-uid/ccc/cond2-true-case
        regular-utopia-storyboard-uid/ccc/cond2/eee
          regular-utopia-storyboard-uid/ccc/cond2/eee/cond3
            conditional-clause-utopia-storyboard-uid/ccc/cond2/eee/cond3-true-case
              regular-utopia-storyboard-uid/ccc/cond2/eee/cond3/fff
            conditional-clause-utopia-storyboard-uid/ccc/cond2/eee/cond3-false-case
              synthetic-utopia-storyboard-uid/ccc/cond2/eee/cond3/129-attribute
          regular-utopia-storyboard-uid/ccc/cond2/eee/ggg
      conditional-clause-utopia-storyboard-uid/ccc/cond2-false-case
        synthetic-utopia-storyboard-uid/ccc/cond2/328-attribute
  regular-utopia-storyboard-uid/cond4
    conditional-clause-utopia-storyboard-uid/cond4-true-case
      regular-utopia-storyboard-uid/cond4/hhh
    conditional-clause-utopia-storyboard-uid/cond4-false-case
      synthetic-utopia-storyboard-uid/cond4/5ea-attribute
  regular-utopia-storyboard-uid/cond5
    conditional-clause-utopia-storyboard-uid/cond5-true-case
      regular-utopia-storyboard-uid/cond5/iii
    conditional-clause-utopia-storyboard-uid/cond5-false-case
      synthetic-utopia-storyboard-uid/cond5/658-attribute
  regular-utopia-storyboard-uid/jjj`
    expect(
      navigatorStructure(
        renderResult.getEditorState().editor,
        renderResult.getEditorState().derived,
      ),
    ).toEqual(want)
  })
  it('can not drag into a conditional', async () => {
    const renderResult = await renderTestEditorWithCode(getProjectCode(), 'await-first-dom-report')

    const expectedNavigatorStructure = `  regular-utopia-storyboard-uid/scene-aaa
    regular-utopia-storyboard-uid/scene-aaa/containing-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-true-case
          regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-true-case
              regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/then-then-div
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-false-case
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/a25-attribute
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/else-div-element-else-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div`

    expect(
      navigatorStructure(
        renderResult.getEditorState().editor,
        renderResult.getEditorState().derived,
      ),
    ).toEqual(expectedNavigatorStructure)

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
      `${BakedInStoryboardUID}/${TestSceneUID}/containing-div/conditional1`,
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

    FOR_TESTS_setNextGeneratedUids(['fragment'])

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

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(getProjectCode())

    expect(
      navigatorStructure(
        renderResult.getEditorState().editor,
        renderResult.getEditorState().derived,
      ),
    ).toEqual(expectedNavigatorStructure)
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
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/a25-attribute
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-false-case
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/129-attribute
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
        conditionalClauseNavigatorEntry(elementPathToTarget, 'true-case'),
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
          conditionalClauseNavigatorEntry(elementPathToTarget, 'true-case'),
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
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/129-attribute
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/else-div-element-else-div`)
  })
  it('can reorder to before the conditional', async () => {
    const renderResult = await renderTestEditorWithCode(getProjectCode(), 'await-first-dom-report')

    const expectedNavigatorStructure = `  regular-utopia-storyboard-uid/scene-aaa
    regular-utopia-storyboard-uid/scene-aaa/containing-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-true-case
          regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-true-case
              regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/then-then-div
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-false-case
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/a25-attribute
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/else-div-element-else-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div`

    expect(
      navigatorStructure(
        renderResult.getEditorState().editor,
        renderResult.getEditorState().derived,
      ),
    ).toEqual(expectedNavigatorStructure)

    // Select the entry we plan to drag.
    const elementPathToDrag = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/containing-div/sibling-div`,
    )

    await act(async () => {
      await renderResult.dispatch(selectComponents([elementPathToDrag], false), false)
      await renderResult.getDispatchFollowUpActionsFinished()
    })

    // Getting info relating to what element will be dragged.
    const navigatorEntryToDrag = await renderResult.renderedDOM.findByTestId(
      `navigator-item-${varSafeNavigatorEntryToKey(regularNavigatorEntry(elementPathToDrag))}`,
    )
    const navigatorEntryToDragRect = navigatorEntryToDrag.getBoundingClientRect()
    const navigatorEntryToDragCenter = getDomRectCenter(navigatorEntryToDragRect)

    // Getting info relating to where the element will be dragged to.
    const elementPathToTarget = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/containing-div/conditional1`,
    )
    const navigatorEntryToTarget = await renderResult.renderedDOM.findByTestId(
      `navigator-item-${varSafeNavigatorEntryToKey(regularNavigatorEntry(elementPathToTarget))}`,
    )
    const navigatorEntryToTargetRect = navigatorEntryToTarget.getBoundingClientRect()
    const navigatorEntryToTargetCenter = getDomRectCenter(navigatorEntryToTargetRect)
    const dragTo = {
      x: navigatorEntryToTargetCenter.x,
      y: navigatorEntryToTargetCenter.y + 3,
    }

    const dragDelta = {
      x: dragTo.x - navigatorEntryToDragCenter.x,
      y: dragTo.y - navigatorEntryToDragCenter.y,
    }

    FOR_TESTS_setNextGeneratedUids(['fragment'])

    await act(async () =>
      dragElement(
        renderResult,
        `navigator-item-${varSafeNavigatorEntryToKey(regularNavigatorEntry(elementPathToDrag))}`,
        TopDropTargetLineTestId(
          varSafeNavigatorEntryToKey(regularNavigatorEntry(elementPathToTarget)),
        ),
        windowPoint(navigatorEntryToDragCenter),
        windowPoint(dragDelta),
        'apply-hover-events',
      ),
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(
      navigatorStructure(
        renderResult.getEditorState().editor,
        renderResult.getEditorState().derived,
      ),
    ).toEqual(`  regular-utopia-storyboard-uid/scene-aaa
    regular-utopia-storyboard-uid/scene-aaa/containing-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-true-case
          regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-true-case
              regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/then-then-div
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-false-case
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/a25-attribute
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
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/a25-attribute
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
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/sibling-div-element-sibling-div
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/else-div-element-else-div`)
  })
  it('cannot drag into a non-empty active clause', async () => {
    const projectCode = getProjectCodeNotEmpty()
    const got = await ensureNoopDrag({
      projectCode: projectCode,
      pathToDrag: EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/containing-div/sibling-div`,
      ),
      dropTargetID: `navigator-item-${varSafeNavigatorEntryToKey(
        conditionalClauseNavigatorEntry(
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/containing-div/conditional1`),
          'true-case',
        ),
      )}`,
    })
    expect(got).toEqual(projectCode)
  })
  it('cannot drag into a non-empty inactive clause', async () => {
    const projectCode = getProjectCodeNotEmpty()
    const got = await ensureNoopDrag({
      projectCode: projectCode,
      pathToDrag: EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/containing-div/sibling-div`,
      ),
      dropTargetID: `navigator-item-${varSafeNavigatorEntryToKey(
        conditionalClauseNavigatorEntry(
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/containing-div/conditional1`),
          'false-case',
        ),
      )}`,
    })
    expect(got).toEqual(projectCode)
  })
  it('dragging in between clauses does nothing', async () => {
    const renderResult = await renderTestEditorWithCode(
      getProjectCodeEmptySingleConditional(),
      'await-first-dom-report',
    )

    const dragMeElement = await renderResult.renderedDOM.findByTestId(
      `navigator-item-regular_utopia_storyboard_uid/scene_aaa/containing_div/sibling_div`,
    )

    const dragMeElementRect = dragMeElement.getBoundingClientRect()
    const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
    const firstDivElement = await renderResult.renderedDOM.findByTestId(
      `navigator-item-drag-synthetic_utopia_storyboard_uid/scene_aaa/containing_div/conditional/a25_attribute`,
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

    const targetElement = EP.fromString(
      'regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div',
    )
    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch(selectComponents([targetElement], false), false)
      await dispatchDone
    })

    act(() =>
      dragElement(
        renderResult,
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/containing_div/sibling_div`,
        `navigator-item-drop-after-conditional_clause_utopia_storyboard_uid/scene_aaa/containing_div/conditional_true-case`,
        windowPoint(dragMeElementCenter),
        dragDelta,
        'apply-hover-events',
      ),
    )

    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/containing-div',
        'regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional',
        'conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional-true-case',
        'synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional/a25-attribute',
        'conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional-false-case',
        'synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional/129-attribute',
        'regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div',
      ],
    )
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
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/a25-attribute
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/else-div-element-else-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div`)

    // Select the entry we plan to drag.
    const elementPathToDrag = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/containing-div/conditional1/else-div`,
    )

    // Need the underlying value in the clause to be able to construct the navigator entry.
    const inactiveElementOptic: Optic<EditorState, JSXElementChild> = compose3Optics(
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

    // Need the underlying value in the clause to be able to construct the navigator entry.
    const removedOriginalLocationOptic: Optic<EditorState, JSXElementChild> = compose3Optics(
      forElementOptic(EP.parentPath(elementPathToDrag)),
      jsxConditionalExpressionOptic,
      conditionalWhenFalseOptic,
    )
    const removedOriginalLocation = unsafeGet(
      removedOriginalLocationOptic,
      renderResult.getEditorState().editor,
    )
    const removedOriginalUID = getUtopiaID(removedOriginalLocation)

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
      regular-utopia-storyboard-uid/scene-aaa/containing-div/else-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-true-case
          regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-true-case
              regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/then-then-div
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-false-case
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/a25-attribute
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/${removedOriginalUID}-attribute
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
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/a25-attribute
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

    // Need the underlying value in the clause to be able to construct the navigator entry.
    const removedOriginalLocationOptic: Optic<EditorState, JSXElementChild> = compose3Optics(
      forElementOptic(EP.parentPath(elementPathToDrag)),
      jsxConditionalExpressionOptic,
      conditionalWhenTrueOptic,
    )
    const removedOriginalLocation = unsafeGet(
      removedOriginalLocationOptic,
      renderResult.getEditorState().editor,
    )
    const removedOriginalUID = getUtopiaID(removedOriginalLocation)

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
      regular-utopia-storyboard-uid/scene-aaa/containing-div/then-then-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-true-case
          regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-true-case
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/${removedOriginalUID}-attribute
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-false-case
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/a25-attribute
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/else-div-element-else-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div`)
  })
  xit('dragging into child of an active clause, works as it would without the conditional', () => {
    // TODO: Fill this out.
  })
  it('can select and delete an inactive clause', async () => {
    const renderResult = await renderTestEditorWithCode(getProjectCode(), 'await-first-dom-report')

    // Determine the entry we want to select.
    const elementPathToDelete = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/containing-div/conditional1/else-div`,
    )
    const elseDivElement = unsafeGet(
      forElementOptic(elementPathToDelete),
      renderResult.getEditorState().editor,
    )

    // Getting info relating to what element will be selected.
    const navigatorEntryToSelect = await renderResult.renderedDOM.findByTestId(
      NavigatorItemTestId(
        varSafeNavigatorEntryToKey(syntheticNavigatorEntry(elementPathToDelete, elseDivElement)),
      ),
    )
    const navigatorEntryToSelectRect = navigatorEntryToSelect.getBoundingClientRect()
    const navigatorEntryToSelectCenter = getDomRectCenter(navigatorEntryToSelectRect)

    // Select the inactive entry in the navigator.
    await act(async () => {
      await mouseClickAtPoint(navigatorEntryToSelect, navigatorEntryToSelectCenter)
    })

    await renderResult.getDispatchFollowUpActionsFinished()

    const selectedViewPaths = renderResult.getEditorState().editor.selectedViews.map(EP.toString)
    expect(selectedViewPaths).toEqual([EP.toString(elementPathToDelete)])

    // Delete the inactive entry.
    await act(async () => {
      await pressKey('delete')
    })

    await renderResult.getDispatchFollowUpActionsFinished()

    // Need the underlying value in the clause to be able to construct the navigator entry.
    const removedOriginalLocationOptic: Optic<EditorState, JSXElementChild> = compose3Optics(
      forElementOptic(EP.parentPath(elementPathToDelete)),
      jsxConditionalExpressionOptic,
      conditionalWhenFalseOptic,
    )
    const removedOriginalLocation = unsafeGet(
      removedOriginalLocationOptic,
      renderResult.getEditorState().editor,
    )
    const removedOriginalUID = getUtopiaID(removedOriginalLocation)

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
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/a25-attribute
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/${removedOriginalUID}-attribute
      regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div`)
  })
  it('can select the true case clause by its label', async () => {
    const renderResult = await renderTestEditorWithCode(getProjectCode(), 'await-first-dom-report')

    // Determine the entry we want to select.
    const clausePath = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/containing-div/conditional1/conditional2/then-then-div`,
    )

    // Getting info relating to what element will be selected.
    const navigatorEntryToSelect = await renderResult.renderedDOM.findByTestId(
      NavigatorItemTestId(
        varSafeNavigatorEntryToKey(
          conditionalClauseNavigatorEntry(EP.parentPath(clausePath), 'true-case'),
        ),
      ),
    )
    const navigatorEntryToSelectRect = navigatorEntryToSelect.getBoundingClientRect()
    const navigatorEntryToSelectCenter = getDomRectCenter(navigatorEntryToSelectRect)

    // Select the false label entry in the navigator.
    await act(async () => {
      await mouseClickAtPoint(navigatorEntryToSelect, navigatorEntryToSelectCenter)
    })

    await renderResult.getDispatchFollowUpActionsFinished()

    const selectedViewPaths = renderResult.getEditorState().editor.selectedViews.map(EP.toString)
    expect(selectedViewPaths).toEqual([EP.toString(clausePath)])
  })
  it('can select the false case clause by its label', async () => {
    const renderResult = await renderTestEditorWithCode(getProjectCode(), 'await-first-dom-report')

    // Determine the entry we want to select.
    const clausePath = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/containing-div/conditional1/else-div`,
    )

    // Getting info relating to what element will be selected.
    const navigatorEntryToSelect = await renderResult.renderedDOM.findByTestId(
      NavigatorItemTestId(
        varSafeNavigatorEntryToKey(
          conditionalClauseNavigatorEntry(EP.parentPath(clausePath), 'false-case'),
        ),
      ),
    )
    const navigatorEntryToSelectRect = navigatorEntryToSelect.getBoundingClientRect()
    const navigatorEntryToSelectCenter = getDomRectCenter(navigatorEntryToSelectRect)

    // Select the false label entry in the navigator.
    await act(async () => {
      await mouseClickAtPoint(navigatorEntryToSelect, navigatorEntryToSelectCenter)
    })

    await renderResult.getDispatchFollowUpActionsFinished()

    const selectedViewPaths = renderResult.getEditorState().editor.selectedViews.map(EP.toString)
    expect(selectedViewPaths).toEqual([EP.toString(clausePath)])
  })
  it('can be collapsed', async () => {
    const renderResult = await renderTestEditorWithCode(getProjectCode(), 'await-first-dom-report')

    const collapseButton = await screen.findByTestId(
      `navigator-item-collapse-regular-${BakedInStoryboardUID}/${TestSceneUID}/containing-div/conditional1-button`,
    )
    const collapseButtonRect = collapseButton.getBoundingClientRect()
    const collapseButtonCenter = getDomRectCenter(collapseButtonRect)

    const trueBranchTestId = NavigatorItemTestId(
      varSafeNavigatorEntryToKey(
        conditionalClauseNavigatorEntry(
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/containing-div/conditional1`),
          'true-case',
        ),
      ),
    )
    const falseBranchTestId = NavigatorItemTestId(
      varSafeNavigatorEntryToKey(
        conditionalClauseNavigatorEntry(
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/containing-div/conditional1`),
          'false-case',
        ),
      ),
    )

    // by default, branches are shown
    {
      expect(renderResult.renderedDOM.queryByTestId(trueBranchTestId)).not.toBeNull()
      expect(renderResult.renderedDOM.queryByTestId(falseBranchTestId)).not.toBeNull()
    }

    // clicking once will collapse the item, hiding the contents
    {
      await act(async () => {
        await mouseClickAtPoint(collapseButton, collapseButtonCenter)
      })

      expect(renderResult.renderedDOM.queryByTestId(trueBranchTestId)).toBeNull()
      expect(renderResult.renderedDOM.queryByTestId(falseBranchTestId)).toBeNull()
    }

    // clicking again will expand the item, showing the contents again
    {
      await act(async () => {
        await mouseClickAtPoint(collapseButton, collapseButtonCenter)
      })

      expect(renderResult.renderedDOM.queryByTestId(trueBranchTestId)).not.toBeNull()
      expect(renderResult.renderedDOM.queryByTestId(falseBranchTestId)).not.toBeNull()
    }
  })

  interface PasteTestCase {
    description: string
    startingCode: string
    pathToCopy: ElementPath
    pathToPasteInto: ElementPath
    expectedTargetPasteParent: InsertionPath<ElementPath>
    expectedToasts: Array<string>
    expectedNavigatorStructure: string
    postPasteValidation: (
      pasteTestCase: PasteTestCase,
      startingEditorState: EditorState,
      endingEditorState: EditorState,
    ) => void
  }

  const pasteTestCases: Array<PasteTestCase> = [
    {
      description: 'can select and paste into an inactive clause containing a null attribute',
      startingCode: getProjectCode(),
      pathToCopy: EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/containing-div/sibling-div`,
      ),
      pathToPasteInto: EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/containing-div/conditional1/conditional2/a25`,
      ),
      expectedTargetPasteParent: conditionalClauseInsertionPath(
        EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/containing-div/conditional1/conditional2`,
        ),
        'false-case',
      ),
      expectedToasts: [],
      expectedNavigatorStructure: `  regular-utopia-storyboard-uid/scene-aaa
    regular-utopia-storyboard-uid/scene-aaa/containing-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-true-case
          regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-true-case
              regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/then-then-div
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-false-case
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/sib-element-sib
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/else-div-element-else-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div`,
      postPasteValidation: (
        pasteTestCase: PasteTestCase,
        startingEditorState: EditorState,
        endingEditorState: EditorState,
      ) => {
        const expectedPasteTargetOptic = compose3Optics(
          forElementOptic(EP.parentPath(pasteTestCase.pathToPasteInto)),
          fromTypeGuard(isJSXConditionalExpression),
          conditionalWhenFalseOptic,
        )
        const expectedPasteTargetBeforePaste = unsafeGet(
          expectedPasteTargetOptic,
          startingEditorState,
        )
        const expectedPasteTargetAfterPaste = unsafeGet(expectedPasteTargetOptic, endingEditorState)

        expect(expectedPasteTargetBeforePaste.type).toEqual('ATTRIBUTE_VALUE')
        expect(expectedPasteTargetAfterPaste.type).toEqual('JSX_ELEMENT')
      },
    },
  ]

  pasteTestCases.forEach((pasteTestCase) => {
    // eslint-disable-next-line jest/valid-title
    it(pasteTestCase.description, async () => {
      const renderResult = await renderTestEditorWithCode(
        pasteTestCase.startingCode,
        'await-first-dom-report',
      )
      const beforePasteEditorState = renderResult.getEditorState().editor

      // Determine the entry we want to copy and paste.
      const elementToCopy = unsafeGet(
        forElementOptic(pasteTestCase.pathToCopy),
        renderResult.getEditorState().editor,
      )

      // Getting info relating to what element will be pasted into.
      const elementToPasteInto = unsafeGet(
        forElementOptic(pasteTestCase.pathToPasteInto),
        renderResult.getEditorState().editor,
      )
      const navigatorEntryToPasteInto = await renderResult.renderedDOM.findByTestId(
        NavigatorItemTestId(
          varSafeNavigatorEntryToKey(
            syntheticNavigatorEntry(pasteTestCase.pathToPasteInto, elementToPasteInto),
          ),
        ),
      )
      const navigatorEntryToPasteIntoRect = navigatorEntryToPasteInto.getBoundingClientRect()
      const navigatorEntryToPasteIntoCenter = getDomRectCenter(navigatorEntryToPasteIntoRect)

      // Select the target paste into in the navigator.
      await act(async () => {
        await mouseClickAtPoint(navigatorEntryToPasteInto, navigatorEntryToPasteIntoCenter)
      })
      await renderResult.getDispatchFollowUpActionsFinished()
      const secondSelectedPaths = renderResult
        .getEditorState()
        .editor.selectedViews.map(EP.toString)
      expect(secondSelectedPaths).toEqual([EP.toString(pasteTestCase.pathToPasteInto)])

      // Trigger the paste.
      // Try to mimic the paste logic that calls this function.
      const editorStateForPaste = renderResult.getEditorState().editor
      const targetPasteParent = getTargetParentForPaste(
        editorStateForPaste.projectContents,
        editorStateForPaste.selectedViews,
        editorStateForPaste.nodeModules.files,
        editorStateForPaste.canvas.openFile?.filename,
        editorStateForPaste.jsxMetadata,
        editorStateForPaste.pasteTargetsToIgnore,
      )
      if (targetPasteParent == null) {
        throw new Error(`No target paste parent identified.`)
      }
      expect(targetPasteParent).toEqual(pasteTestCase.expectedTargetPasteParent)
      const pasteElements = pasteJSXElements(
        targetPasteParent,
        [elementPaste(elementToCopy, emptyImports(), pasteTestCase.pathToCopy)],
        renderResult.getEditorState().editor.jsxMetadata,
      )
      await renderResult.dispatch([pasteElements], true)
      await renderResult.getDispatchFollowUpActionsFinished()

      if (getPrintedUiJsCode(renderResult.getEditorState()) === pasteTestCase.startingCode) {
        throw new Error(`Code is unchanged.`)
      }

      expect(renderResult.getEditorState().editor.toasts.map((t) => t.message)).toEqual(
        pasteTestCase.expectedToasts,
      )

      expect(
        navigatorStructure(
          renderResult.getEditorState().editor,
          renderResult.getEditorState().derived,
        ),
      ).toEqual(pasteTestCase.expectedNavigatorStructure)

      pasteTestCase.postPasteValidation(
        pasteTestCase,
        beforePasteEditorState,
        renderResult.getEditorState().editor,
      )
    })
  })
})
