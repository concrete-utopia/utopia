import { fireEvent, screen } from '@testing-library/react'
import { act } from 'react-dom/test-utils'
import type { EditorRenderResult } from '../../components/canvas/ui-jsx.test-utils'
import {
  TestSceneUID,
  formatTestProjectCode,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../components/canvas/ui-jsx.test-utils'
import { forElementChildOptic } from '../../core/model/common-optics'
import type { ConditionalCase } from '../../core/model/conditionals'
import {
  conditionalClauseAsBoolean,
  conditionalWhenFalseOptic,
  conditionalWhenTrueOptic,
  getConditionalActiveCase,
  jsxConditionalExpressionOptic,
} from '../../core/model/conditionals'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { FOR_TESTS_setNextGeneratedUids } from '../../core/model/element-template-utils.test-utils'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import { getDomRectCenter } from '../../core/shared/dom-utils'
import { isLeft } from '../../core/shared/either'
import * as EP from '../../core/shared/element-path'
import { isJSXConditionalExpression } from '../../core/shared/element-template'
import type { WindowPoint } from '../../core/shared/math-utils'
import { offsetPoint, windowPoint } from '../../core/shared/math-utils'
import { fromTypeGuard } from '../../core/shared/optics/optic-creators'
import { unsafeGet } from '../../core/shared/optics/optic-utilities'
import type { ElementPath } from '../../core/shared/project-file-types'
import { getUtopiaID } from '../../core/shared/uid-utils'
import { NO_OP } from '../../core/shared/utils'
import { cmdModifier } from '../../utils/modifiers'
import { selectComponentsForTest } from '../../utils/utils.test-utils'
import {
  MockClipboardHandlers,
  firePasteEvent,
  mouseClickAtPoint,
  pressKey,
} from '../canvas/event-helpers.test-utils'
import { setConditionalOverriddenCondition } from '../editor/actions/action-creators'
import { selectComponents } from '../editor/actions/meta-actions'
import type {
  DerivedState,
  EditorState,
  EditorStoreFull,
  EditorStorePatched,
  NavigatorEntry,
} from '../editor/store/editor-state'
import {
  conditionalClauseNavigatorEntry,
  navigatorEntryToKey,
  regularNavigatorEntry,
  syntheticNavigatorEntry,
  varSafeNavigatorEntryToKey,
} from '../editor/store/editor-state'
import type { InsertionPath } from '../editor/store/insertion-path'
import {
  conditionalClauseInsertionPath,
  replaceWithSingleElement,
} from '../editor/store/insertion-path'
import { NavigatorItemTestId } from './navigator-item/navigator-item'
import {
  DragItemTestId,
  TopDropTargetLineTestId,
} from './navigator-item/navigator-item-dnd-container'
import { navigatorDepth } from './navigator-utils'

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

function getProjectCodeMapExpressionWithMultipleValues(): string {
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
          true ? [0, 1, 2].map(i => <div><div>hello {i}</div></div>) : <div data-uid='else-div' />
        }
      </div>
    </Scene>
  </Storyboard>
)
`
}

function getProjectCodeMapExpressionWithMultipleValuesInlinedAndNullInactive(): string {
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
          true ? [0, 1, 2].map(i => <div>hello {i}</div>) : null
        }
      </div>
      {
        // @utopia/uid=268
        [0, 1, 2].map(i => <div>another {i}</div>)
      }
      <div data-uid='hey'>hey</div>
    </Scene>
  </Storyboard>
)
`
}

function getProjectCodeMapExpressionWithMultipleValuesInlinedAndNotNullInactive(): string {
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
          true ? (
            // @utopia/uid=50c
            [0, 1, 2].map(i => <div>hello {i}</div>)
          ) : <div data-uid='false-branch' />
        }
      </div>
      {
        // @utopia/uid=268
        [0, 1, 2].map(i => <div>another {i}</div>)
      }
      <div data-uid='hey'>hey</div>
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

function getConditionalCaseFromPath(renderResult: EditorRenderResult, path: ElementPath) {
  const conditional = MetadataUtils.findElementByElementPath(
    renderResult.getEditorState().editor.jsxMetadata,
    path,
  )
  if (
    conditional == null ||
    isLeft(conditional.element) ||
    !isJSXConditionalExpression(conditional.element.value)
  ) {
    throw new Error('Not a conditional')
  }

  return getConditionalActiveCase(
    path,
    conditional.element.value,
    renderResult.getEditorState().editor.spyMetadata,
  )
}

describe('conditionals in the navigator', () => {
  const clipboardMock = new MockClipboardHandlers().mock()

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
          expect((parentEntry.firstChild as HTMLElement).style.outline).toEqual(
            'transparent solid 1px',
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

    expect(
      getConditionalCaseFromPath(
        renderResult,
        EP.fromString('utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2'),
      ),
    ).toEqual('true-case')

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

    expect(
      getConditionalCaseFromPath(
        renderResult,
        EP.fromString('utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2'),
      ),
    ).toEqual('true-case')
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

    await selectComponentsForTest(renderResult, [elementPathToDrag])

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

    await dragElement(
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
  it('dragging into an empty inactive clause, takes the place of the empty value and makes it active', async () => {
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

    expect(
      getConditionalCaseFromPath(
        renderResult,
        EP.fromString('utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2'),
      ),
    ).toEqual('true-case')

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
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/then-then-div-element-then-then-div
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-false-case
              regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/sibling-div
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/else-div-element-else-div`)

    expect(
      getConditionalCaseFromPath(
        renderResult,
        EP.fromString('utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2'),
      ),
    ).toEqual('false-case')
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
    const inactiveElementOptic = forElementChildOptic(EP.parentPath(elementPathToDrag))
      .compose(jsxConditionalExpressionOptic)
      .compose(conditionalWhenFalseOptic)
    const inactiveElement = unsafeGet(inactiveElementOptic, renderResult.getEditorState())

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
    const removedOriginalLocationOptic = forElementChildOptic(EP.parentPath(elementPathToDrag))
      .compose(jsxConditionalExpressionOptic)
      .compose(conditionalWhenFalseOptic)
    const removedOriginalLocation = unsafeGet(
      removedOriginalLocationOptic,
      renderResult.getEditorState(),
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
      regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/else-div`)
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
    const removedOriginalLocationOptic = forElementChildOptic(EP.parentPath(elementPathToDrag))
      .compose(jsxConditionalExpressionOptic)
      .compose(conditionalWhenTrueOptic)
    const removedOriginalLocation = unsafeGet(
      removedOriginalLocationOptic,
      renderResult.getEditorState(),
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
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/${removedOriginalUID}-attribute
            conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2-false-case
              synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/conditional2/a25-attribute
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional1-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional1/else-div-element-else-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/then-then-div`)
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
      forElementChildOptic(elementPathToDelete),
      renderResult.getEditorState(),
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
    const removedOriginalLocationOptic = forElementChildOptic(EP.parentPath(elementPathToDelete))
      .compose(jsxConditionalExpressionOptic)
      .compose(conditionalWhenFalseOptic)
    const removedOriginalLocation = unsafeGet(
      removedOriginalLocationOptic,
      renderResult.getEditorState(),
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
      startingEditorState: EditorStorePatched,
      endingEditorStore: EditorStorePatched,
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
        replaceWithSingleElement(),
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
        startingEditorStore: EditorStorePatched,
        endingEditorStore: EditorStorePatched,
      ) => {
        const expectedPasteTargetOptic = forElementChildOptic(
          EP.parentPath(pasteTestCase.pathToPasteInto),
        )
          .compose(fromTypeGuard(isJSXConditionalExpression))
          .compose(conditionalWhenFalseOptic)
        const expectedPasteTargetBeforePaste = unsafeGet(
          expectedPasteTargetOptic,
          startingEditorStore,
        )
        const expectedPasteTargetAfterPaste = unsafeGet(expectedPasteTargetOptic, endingEditorStore)

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
      const beforePasteEditorState = renderResult.getEditorState()

      await selectComponentsForTest(renderResult, [pasteTestCase.pathToCopy])
      await pressKey('c', { modifiers: cmdModifier })

      // Getting info relating to what element will be pasted into.
      const elementToPasteInto = unsafeGet(
        forElementChildOptic(pasteTestCase.pathToPasteInto),
        renderResult.getEditorState(),
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

      const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')

      firePasteEvent(canvasRoot)

      // Wait for the next frame
      await clipboardMock.pasteDone
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
        renderResult.getEditorState(),
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

      // even without overrides, labels are never the same
      {
        expect(await getLabelColor('false-case')).not.toEqual(await getLabelColor('true-case'))
      }

      // override a branch, its color changes
      {
        await renderResult.dispatch([setConditionalOverriddenCondition(target, false)], true)
        expect(await getLabelColor('false-case')).not.toEqual(defaultLabelColor)
        expect(await getLabelColor('true-case')).not.toEqual(await getLabelColor('false-case'))
      }

      // try the other way around
      {
        await renderResult.dispatch([setConditionalOverriddenCondition(target, true)], true)
        expect(await getLabelColor('true-case')).not.toEqual(defaultLabelColor)
        expect(await getLabelColor('false-case')).not.toEqual(await getLabelColor('true-case'))
      }
    })
  })

  describe('js expressions', () => {
    it('shows the right label for branches with js expressions', async () => {
      await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              true ? (
                // @utopia/uid=ba9
                (() => <div>HELLO!</div>)()
              ) : <div />
            }
            </div>
            `),
        'await-first-dom-report',
      )

      const exprLabel = await screen.findByTestId(
        `NavigatorItemTestId-regular_utopia_storyboard_uid/scene_aaa/app_entity:aaa/conditional/ba9-label`,
      )

      expect(exprLabel.innerText).toEqual('CODE')

      const generatedElementLabel = await screen.findByTestId(
        `NavigatorItemTestId-regular_utopia_storyboard_uid/scene_aaa/app_entity:aaa/conditional/ba9/33d~~~1-label`,
      )

      expect(generatedElementLabel.innerText).toEqual('HELLO!')
    })
    it('shows generated expression values in black', async () => {
      await renderTestEditorWithCode(
        getProjectCodeMapExpressionWithMultipleValues(),
        'await-first-dom-report',
      )

      const labelColor = (
        await screen.findByTestId(
          `NavigatorItemTestId-regular_utopia_storyboard_uid/scene_aaa/containing_div/conditional/b5e/46a~~~1`,
        )
      ).style.color

      expect(labelColor).toEqual('var(--utopitheme-fg5)')
    })
    it('supports map expressions that return multiple values', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCodeMapExpressionWithMultipleValues(),
        'await-first-dom-report',
      )

      expect(
        navigatorStructure(
          renderResult.getEditorState().editor,
          renderResult.getEditorState().derived,
        ),
      ).toEqual(`  regular-utopia-storyboard-uid/scene-aaa
    regular-utopia-storyboard-uid/scene-aaa/containing-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional-true-case
          regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional/b5e
            regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional/b5e/46a~~~1
              regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional/b5e/46a~~~1/33d
            regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional/b5e/46a~~~2
              regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional/b5e/46a~~~2/33d
            regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional/b5e/46a~~~3
              regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional/b5e/46a~~~3/33d
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional/else-div-element-else-div`)
    })
    it('keeps the right order for inlined map expressions with multiple values (null inactive branch)', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCodeMapExpressionWithMultipleValuesInlinedAndNullInactive(),
        'await-first-dom-report',
      )

      expect(
        navigatorStructure(
          renderResult.getEditorState().editor,
          renderResult.getEditorState().derived,
        ),
      ).toEqual(`  regular-utopia-storyboard-uid/scene-aaa
    regular-utopia-storyboard-uid/scene-aaa/containing-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional-true-case
          regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional/b35
            regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional/b35/33d~~~1
            regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional/b35/33d~~~2
            regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional/b35/33d~~~3
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional/a25-attribute
    regular-utopia-storyboard-uid/scene-aaa/268
      regular-utopia-storyboard-uid/scene-aaa/268/46a~~~1
      regular-utopia-storyboard-uid/scene-aaa/268/46a~~~2
      regular-utopia-storyboard-uid/scene-aaa/268/46a~~~3
    regular-utopia-storyboard-uid/scene-aaa/hey`)
    })
    it('keeps the right order for inlined map expressions with multiple values (not-null inactive branch)', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCodeMapExpressionWithMultipleValuesInlinedAndNotNullInactive(),
        'await-first-dom-report',
      )

      expect(
        navigatorStructure(
          renderResult.getEditorState().editor,
          renderResult.getEditorState().derived,
        ),
      ).toEqual(`  regular-utopia-storyboard-uid/scene-aaa
    regular-utopia-storyboard-uid/scene-aaa/containing-div
      regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional-true-case
          regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional/50c
            regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional/50c/33d~~~1
            regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional/50c/33d~~~2
            regular-utopia-storyboard-uid/scene-aaa/containing-div/conditional/50c/33d~~~3
        conditional-clause-utopia-storyboard-uid/scene-aaa/containing-div/conditional-false-case
          synthetic-utopia-storyboard-uid/scene-aaa/containing-div/conditional/false-branch-element-false-branch
    regular-utopia-storyboard-uid/scene-aaa/268
      regular-utopia-storyboard-uid/scene-aaa/268/46a~~~1
      regular-utopia-storyboard-uid/scene-aaa/268/46a~~~2
      regular-utopia-storyboard-uid/scene-aaa/268/46a~~~3
    regular-utopia-storyboard-uid/scene-aaa/hey`)
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
    const inactiveElementOptic = forElementChildOptic(EP.parentPath(elementPath))
      .compose(jsxConditionalExpressionOptic)
      .compose(elementUID === 'true-div' ? conditionalWhenTrueOptic : conditionalWhenFalseOptic)

    const navigatorEntry = isActive
      ? regularNavigatorEntry(elementPath)
      : syntheticNavigatorEntry(
          elementPath,
          unsafeGet(inactiveElementOptic, renderResult.getEditorState()),
        )

    return clickNavigatorRow(navigatorEntry, renderResult, [elementPath])
  }

  it('the default clause can be turned on and toggled', async () => {
    const renderResult = await renderTestEditorWithCode(
      codeWithoutOverride,
      'await-first-dom-report',
    )

    // The true case is the default and active case, override is set
    {
      await clickLabelForCase('true-case', renderResult)
      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        codeWithOverride('true-case'),
      )
    }

    // The true case is the default and active case, override is removed
    {
      await clickLabelForCase('true-case', renderResult)
      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(codeWithoutOverride)
    }
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

    // The false override is set, clicking on true activates the true override
    {
      await clickLabelForCase('true-case', renderResult)
      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        codeWithOverride('true-case'),
      )
    }

    // The true override is set, clicking on false activates the false override
    {
      await clickLabelForCase('false-case', renderResult)
      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        codeWithOverride('false-case'),
      )
    }

    // Clicking again on false removes the override
    {
      await clickLabelForCase('false-case', renderResult)
      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(codeWithoutOverride)
    }
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
