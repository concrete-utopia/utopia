import { r } from 'tar'
import { elementPath } from '../../core/shared/element-path'
import { assertNever } from '../../core/shared/utils'
import { mouseClickAtPoint } from '../canvas/event-helpers.test-utils'
import { getPrintedUiJsCode, renderTestEditorWithCode } from '../canvas/ui-jsx.test-utils'
import { selectComponents } from '../editor/actions/action-creators'
import { AspectRatioLockButtonTestId } from './sections/layout-section/self-layout-subsection/gigantic-size-pins-subsection'

describe('inspector', () => {
  it('toggle aspect ratio lock off', async () => {
    const codeAfterToggle = await runToggleAspectRatioLockTest('locked')
    expect(codeAfterToggle).toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 33,
        top: 307,
        width: 363,
        height: 426,
      }}
      data-uid='7a0'
    />
  </Storyboard>
)
`)
  })

  it('toggle aspect ratio lock on', async () => {
    const codeAfterToggle = await runToggleAspectRatioLockTest('not-locked')
    expect(codeAfterToggle).toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 33,
        top: 307,
        width: 363,
        height: 426,
      }}
      data-uid='7a0'
      data-aspect-ratio-locked
    />
  </Storyboard>
)
`)
  })
})

async function runToggleAspectRatioLockTest(
  aspectRatioLocked: AspectRatioLockedState,
): Promise<string> {
  const editor = await renderTestEditorWithCode(
    projectSource(aspectRatioLocked),
    'await-first-dom-report',
  )
  const target = elementPath([['0cd', '7a0']])

  await editor.dispatch([selectComponents([target], false)], true)
  await editor.getDispatchFollowUpActionsFinished()

  const aspectRatioLockButton = editor.renderedDOM.getByTestId(AspectRatioLockButtonTestId)
  const aspectRatioLockButtonBounds = aspectRatioLockButton.getBoundingClientRect()
  mouseClickAtPoint(aspectRatioLockButton, {
    x: aspectRatioLockButtonBounds.x + 1,
    y: aspectRatioLockButtonBounds.y + 1,
  })

  await editor.getDispatchFollowUpActionsFinished()
  return getPrintedUiJsCode(editor.getEditorState())
}

type AspectRatioLockedState = 'locked' | 'not-locked'

const isAspectRatioLocked = (state: AspectRatioLockedState): boolean => {
  switch (state) {
    case 'locked':
      return true
    case 'not-locked':
      return false
    default:
      assertNever(state)
  }
}

function projectSource(aspectRatioLockedState: AspectRatioLockedState) {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 33,
        top: 307,
        width: 363,
        height: 426,
      }}
      data-uid='7a0'
      data-aspect-ratio-locked${isAspectRatioLocked(aspectRatioLockedState) ? `` : `={false}`}
    />
  </Storyboard>
)
`
}
