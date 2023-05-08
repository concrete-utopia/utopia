import { fireEvent, screen } from '@testing-library/react'
import { act } from 'react-dom/test-utils'
import {
  EditorRenderResult,
  TestSceneUID,
  formatTestProjectCode,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../components/canvas/ui-jsx.test-utils'
import { forElementOptic } from '../../core/model/common-optics'
import {
  ConditionalCase,
  conditionalClauseAsBoolean,
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
import {
  elementPaste,
  pasteJSXElements,
  setConditionalOverriddenCondition,
} from '../editor/actions/action-creators'
import { selectComponents } from '../editor/actions/meta-actions'
import {
  DerivedState,
  EditorState,
  NavigatorEntry,
  conditionalClauseNavigatorEntry,
  navigatorEntryToKey,
  regularNavigatorEntry,
  syntheticNavigatorEntry,
  varSafeNavigatorEntryToKey,
} from '../editor/store/editor-state'
import { InsertionPath, conditionalClauseInsertionPath } from '../editor/store/insertion-path'
import { NavigatorItemTestId } from './navigator-item/navigator-item'
import {
  DragItemTestId,
  TopDropTargetLineTestId,
} from './navigator-item/navigator-item-dnd-container'
import { navigatorDepth } from './navigator-utils'
import { NO_OP } from '../../core/shared/utils'
import { wait } from '../../utils/utils.test-utils'

const ASYNC_NOOP = async () => NO_OP()

async function dragElement(
  renderResult: EditorRenderResult,
  dragTargetID: string,
  dropTargetID: string,
  startPoint: WindowPoint,
  dragDelta: WindowPoint,
  hoverEvents: 'apply-hover-events' | 'do-not-apply-hover-events',
  midDragCallback: () => Promise<void> = ASYNC_NOOP,
): Promise<void> {
  const dragTarget = renderResult.renderedDOM.getByTestId(dragTargetID)
  const dropTarget = renderResult.renderedDOM.getByTestId(dropTargetID)

  const endPoint = offsetPoint(startPoint, dragDelta)

  await act(async () => {
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
  })

  await act(async () => {
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
  })

  if (hoverEvents === 'apply-hover-events') {
    await act(async () => {
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
    })

    await act(async () => {
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
    })

    await midDragCallback()

    await act(async () => {
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
    })
  }
}

function getProjectCode(): string {
  return formatTestProjectCode(`import * as React from 'react'
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
`)
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
  return formatTestProjectCode(`import * as React from 'react'
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
            <div data-uid='then-div' data-testid='then-div'></div>
          ) : (
            <div data-uid='else-div' data-testid='else-div'></div>
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
`)
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
        async () => {
          // parent highlight is not shown around conditional
          const parentEntry = renderResult.renderedDOM.getByTestId(
            `navigator-item-${varSafeNavigatorEntryToKey(
              regularNavigatorEntry(elementPathToTarget),
            )}`,
          )
          expect((parentEntry.firstChild as HTMLElement).style.border).toEqual(
            '1px solid transparent',
          )
        },
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
      DragItemTestId(varSafeNavigatorEntryToKey(regularNavigatorEntry(elementPathToDrag))),
    )
    const navigatorEntryToDragRect = navigatorEntryToDrag.getBoundingClientRect()
    const navigatorEntryToDragCenter = getDomRectCenter(navigatorEntryToDragRect)

    // Getting info relating to where the element will be dragged to.
    const navigatorEntryToTarget = await renderResult.renderedDOM.findByTestId(
      `navigator-item-synthetic_utopia_storyboard_uid/scene_aaa/containing_div/conditional1/conditional2/a25_attribute`,
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
        `navigator-item-synthetic_utopia_storyboard_uid/scene_aaa/containing_div/conditional1/conditional2/a25_attribute`,
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
        async () => {
          // drop target line is shown before the conditional
          const dropLine = renderResult.renderedDOM.getByTestId(
            TopDropTargetLineTestId(
              varSafeNavigatorEntryToKey(regularNavigatorEntry(elementPathToTarget)),
            ),
          )
          expect(dropLine.style.opacity).toEqual('1')
        },
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
      DragItemTestId(varSafeNavigatorEntryToKey(regularNavigatorEntry(elementPathToDrag))),
    )
    const navigatorEntryToDragRect = navigatorEntryToDrag.getBoundingClientRect()
    const navigatorEntryToDragCenter = getDomRectCenter(navigatorEntryToDragRect)

    // Getting info relating to where the element will be dragged to.
    const navigatorEntryToTarget = await renderResult.renderedDOM.findByTestId(
      `navigator-item-synthetic_utopia_storyboard_uid/scene_aaa/containing_div/conditional1/conditional2/a25_attribute`,
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
        `navigator-item-synthetic_utopia_storyboard_uid/scene_aaa/containing_div/conditional1/conditional2/a25_attribute`,
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
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/aae-attribute
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/else-div-element-else-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div`)
  })
  it('dragging into child of an active clause, works as it would without the conditional', async () => {
    const projectCode = getProjectCodeNotEmpty()

    const pathToDrag = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/containing-div/sibling-div`,
    )
    const dropTargetID = `navigator-item-${varSafeNavigatorEntryToKey(
      regularNavigatorEntry(
        EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/containing-div/conditional1/then-div`,
        ),
      ),
    )}`
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

    expect(
      navigatorStructure(
        renderResult.getEditorState().editor,
        renderResult.getEditorState().derived,
      ),
    ).toEqual(`  regular-utopia-storyboard-uid/scene-aaa
    regular-utopia-storyboard-uid/scene-aaa/containing-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-true-case
          regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/then-div
            regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/then-div/sibling-div
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/else-div-element-else-div`)
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

    // Select the inactive entry in the navigator. This will add an override, which will replace the previously
    // active entry with a synthetic element
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
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-element-conditional2
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/${removedOriginalUID}-attribute
      regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div`)
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
    expectedTargetPasteParent: InsertionPath
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
        'replace',
      ),
      expectedToasts: [],
      expectedNavigatorStructure: `  regular-utopia-storyboard-uid/scene-aaa
    regular-utopia-storyboard-uid/scene-aaa/containing-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-true-case
          regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-true-case
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/then-then-div-element-then-then-div
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-false-case
              regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/sib
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
  describe('overrides', () => {
    it('shows an accent color for overridden labels', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='aaa'>
          {
            // @utopia/uid=conditional
            true ? <div data-uid='bbb' /> : <div data-uid='ccc' />
          }
          </div>
          `),
        'await-first-dom-report',
      )

      async function getLabelColor(condCase: ConditionalCase) {
        return (
          await screen.findByTestId(
            `NavigatorItemTestId-conditional_clause_utopia_storyboard_uid/scene_aaa/app_entity:aaa/conditional_${condCase}-label`,
          )
        ).style.color
      }

      const target = EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:aaa/conditional')
      const defaultLabelColor = await getLabelColor('true-case')

      // with no overrides, both labels are the same
      {
        expect(await getLabelColor('false-case')).toEqual(defaultLabelColor)
      }

      // override a branch, its color changes
      {
        await renderResult.dispatch([setConditionalOverriddenCondition(target, false)], true)
        expect(await getLabelColor('false-case')).not.toEqual(defaultLabelColor)
        expect(await getLabelColor('true-case')).toEqual(defaultLabelColor)
      }

      // try the other way around
      {
        await renderResult.dispatch([setConditionalOverriddenCondition(target, true)], true)
        expect(await getLabelColor('true-case')).not.toEqual(defaultLabelColor)
        expect(await getLabelColor('false-case')).toEqual(defaultLabelColor)
      }
    })
  })
})

describe('Navigator conditional override toggling', () => {
  function codeWithOverride(override?: ConditionalCase) {
    const uidFlag = '// @utopia/uid=conditional'
    const commentFlags =
      override == null
        ? uidFlag
        : `${uidFlag}
          // @utopia/conditional=${conditionalClauseAsBoolean(override)}`

    return formatTestProjectCode(`
      import * as React from 'react'
      import { Scene, Storyboard } from 'utopia-api'
      
      export var ${BakedInStoryboardVariableName} = (
        <Storyboard data-uid='${BakedInStoryboardUID}'>
          {
            ${commentFlags}
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
                data-uid='true-div'
                data-testid='true-div'
              />
            ) : (
              <div
                style={{
                  height: 150,
                  width: 150,
                  position: 'absolute',
                  left: 154,
                  top: 134,
                  backgroundColor: 'red',
                }}
                data-uid='false-div'
                data-testid='false-div'
              />
            )
          }
        </Storyboard>
      )
    `)
  }
  const codeWithoutOverride = codeWithOverride()

  async function clickNavigatorRow(
    navigatorEntry: NavigatorEntry,
    renderResult: EditorRenderResult,
    expectedSelectedViews: Array<ElementPath>,
  ) {
    // Find the row for the element
    const navigatorEntryToSelect = await renderResult.renderedDOM.findByTestId(
      NavigatorItemTestId(varSafeNavigatorEntryToKey(navigatorEntry)),
    )

    const navigatorEntryToSelectRect = navigatorEntryToSelect.getBoundingClientRect()
    const navigatorEntryToSelectCenter = getDomRectCenter(navigatorEntryToSelectRect)

    // Click the row
    await act(async () => {
      await mouseClickAtPoint(navigatorEntryToSelect, navigatorEntryToSelectCenter)
    })

    await renderResult.getDispatchFollowUpActionsFinished()

    // ensure selection was correctly updated
    const selectedViewPaths = renderResult.getEditorState().editor.selectedViews
    expect(selectedViewPaths).toEqual(expectedSelectedViews)
  }

  async function clickLabelForCase(
    conditionalCase: ConditionalCase,
    renderResult: EditorRenderResult,
  ) {
    // Determine the entry we want to select.
    const clausePath = EP.fromString(`${BakedInStoryboardUID}/conditional/true-div`)
    const navigatorEntry = conditionalClauseNavigatorEntry(
      EP.parentPath(clausePath),
      conditionalCase,
    )

    return clickNavigatorRow(navigatorEntry, renderResult, [])
  }

  async function clickChildOfClause(
    elementUID: 'true-div' | 'false-div',
    isActive: boolean,
    renderResult: EditorRenderResult,
  ) {
    // Determine the entry we want to select.
    const elementPath = EP.fromString(`${BakedInStoryboardUID}/conditional/${elementUID}`)

    // Need the underlying value in the clause to be able to construct the navigator entry.
    const inactiveElementOptic: Optic<EditorState, JSXElementChild> = compose3Optics(
      forElementOptic(EP.parentPath(elementPath)),
      jsxConditionalExpressionOptic,
      elementUID === 'true-div' ? conditionalWhenTrueOptic : conditionalWhenFalseOptic,
    )

    const navigatorEntry = isActive
      ? regularNavigatorEntry(elementPath)
      : syntheticNavigatorEntry(
          elementPath,
          unsafeGet(inactiveElementOptic, renderResult.getEditorState().editor),
        )

    return clickNavigatorRow(navigatorEntry, renderResult, [elementPath])
  }

  it('active clause label does nothing if it IS the default clause', async () => {
    const renderResult = await renderTestEditorWithCode(
      codeWithoutOverride,
      'await-first-dom-report',
    )

    // The true case is the default and active case
    await clickLabelForCase('true-case', renderResult)

    // No override should have been added
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(codeWithoutOverride)
  })

  it('active clause label clears override if it IS NOT the default clause', async () => {
    const renderResult = await renderTestEditorWithCode(
      codeWithOverride('false-case'),
      'await-first-dom-report',
    )

    // The false case is the active case, but not the default
    await clickLabelForCase('false-case', renderResult)

    // The override should have been removed
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(codeWithoutOverride)
  })

  it('inactive clause label clears the override if it IS the default clause', async () => {
    const renderResult = await renderTestEditorWithCode(
      codeWithOverride('false-case'),
      'await-first-dom-report',
    )

    // The true case is the default, but isn't active
    await clickLabelForCase('true-case', renderResult)

    // The override should have been removed
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(codeWithoutOverride)
  })

  it('inactive clause label sets the override if it IS NOT the default clause', async () => {
    const renderResult = await renderTestEditorWithCode(
      codeWithoutOverride,
      'await-first-dom-report',
    )

    // The false case is neither active nor the default
    await clickLabelForCase('false-case', renderResult)

    // The conditional should have been overriden to use the false case
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      codeWithOverride('false-case'),
    )
  })

  it('active clause child just sets selection if it is the default clause', async () => {
    const renderResult = await renderTestEditorWithCode(
      codeWithoutOverride,
      'await-first-dom-report',
    )

    // The true case is the default and active case
    await clickChildOfClause('true-div', true, renderResult)

    // No override should have been added
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(codeWithoutOverride)
  })

  it('active clause child also just sets selection if it is not the default clause', async () => {
    const renderResult = await renderTestEditorWithCode(
      codeWithOverride('false-case'),
      'await-first-dom-report',
    )

    // The false case is the active but not default case
    await clickChildOfClause('false-div', true, renderResult)

    // The override should remain unchanged
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      codeWithOverride('false-case'),
    )
  })

  it('inactive clause child sets selection and adds an override if it IS NOT the default clause', async () => {
    const renderResult = await renderTestEditorWithCode(
      codeWithoutOverride,
      'await-first-dom-report',
    )

    // The false case is neither active nor the default
    await clickChildOfClause('false-div', false, renderResult)

    // The override should have been added
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      codeWithOverride('false-case'),
    )
  })

  it('inactive clause child sets selection and clears the override if it IS the default clause', async () => {
    const renderResult = await renderTestEditorWithCode(
      codeWithOverride('false-case'),
      'await-first-dom-report',
    )

    // The true case is the default but not the active case
    await clickChildOfClause('true-div', false, renderResult)

    // No override should have been added
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(codeWithoutOverride)
  })
})
