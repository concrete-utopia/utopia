import { act } from '@testing-library/react'
import type { ProjectContentTreeRoot } from '../../../assets'
import { contentsToTree, getProjectFileByFilePath } from '../../../assets'
import * as EP from '../../../../core/shared/element-path'
import { codeFile, isTextFile } from '../../../../core/shared/project-file-types'
import { cmdModifier } from '../../../../utils/modifiers'
import { selectComponents, setFocusedElement } from '../../../editor/actions/action-creators'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { mouseDragFromPointToPoint } from '../../event-helpers.test-utils'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import { formatTestProjectCode, renderTestEditorWithProjectContent } from '../../ui-jsx.test-utils'

const defaultAbsoluteChildCode = `
import * as React from 'react'
export const AbsoluteChild = (absoluteChildProps) => {
  return (
    <div
      style={{
        position: 'absolute',
        left: 52,
        top: 30,
        width: 92,
        height: 100,
        borderWidth: 10,
        borderColor: 'black',
        borderStyle: 'solid',
        backgroundColor: 'yellow',
        ...absoluteChildProps.style
      }}
      data-uid='absolutechild'
      data-testid='absolutechild'
    />
  )
}`

const defaultAbsoluteParentCode = `
import * as React from 'react'
import { AbsoluteChild } from './absolutechild'
export const AbsoluteParent = () => {
  return (
    <div
      style={{
        position: 'absolute',
        width: 251,
        height: 500,
        left: 0,
        top: 0,
        backgroundColor: 'lightblue',
      }}
      data-uid='absoluteparent'
      data-testid='absoluteparent'
    >
      <AbsoluteChild data-uid='absolutechildinparent' />
    </div>
  )
}`

const defaultAppCode = `
import * as React from 'react'
import { AbsoluteParent } from './absoluteparent'
import { FlexParent } from './flexparent'

export var App = () => {
  return (
    <div
      style={{
        position: 'absolute',
        width: 700,
        height: 600,
        backgroundColor: 'white',
      }}
      data-uid='container'
      data-testid='container'
    >
      <AbsoluteParent data-uid='absoluteparentinapp' />
      <FlexParent data-uid='flexparentinapp' />
    </div>
  )
}`

const defaultFlexChildCode = `
import * as React from 'react'
export const FlexChild1 = () => {
  return (
    <div
      style={{
        width: 100,
        height: 100,
        borderWidth: 10,
        borderColor: 'black',
        borderStyle: 'solid',
        backgroundColor: 'teal',
      }}
      data-uid='flexchild1'
      data-testid='flexchild1'
    />
  )
}

export const FlexChild2 = () => {
  return (
    <div
      style={{
        width: 100,
        height: 100,
        borderWidth: 10,
        borderColor: 'black',
        borderStyle: 'solid',
        backgroundColor: 'red',
      }}
      data-uid='flexchild2'
      data-testid='flexchild2'
    />
  )
}`

const defaultFlexParentCode = `
import * as React from 'react'
import { FlexChild1, FlexChild2 } from './flexchild'
export const FlexParent = () => {
  return (
    <div
      style={{
        display: 'flex',
        flexDirection: 'row',
        position: 'absolute',
        width: 250,
        height: 500,
        left: 350,
        top: 0,
        backgroundColor: 'lightgreen',
      }}
      data-uid='flexparent'
      data-testid='flexparent'
    >
      <FlexChild1 data-uid='flexchild1inparent' />
      <FlexChild2 data-uid='flexchild2inparent' />
    </div>
  )
}`

function makeTestProjectContents(): ProjectContentTreeRoot {
  return contentsToTree({
    ['/package.json']: codeFile(
      `
{
  "name": "Utopia Project",
  "version": "0.1.0",
  "utopia": {
    "main-ui": "utopia/storyboard.js",
    "html": "public/index.html",
    "js": "src/index.js"
  },
  "dependencies": {
    "react": "16.13.1",
    "react-dom": "16.13.1",
    "utopia-api": "0.4.1",
    "non-existant-dummy-library": "8.0.27",
    "@heroicons/react": "1.0.1",
    "@emotion/react": "11.9.3"
  }
}`,
      null,
    ),
    ['/src/absolutechild.js']: codeFile(defaultAbsoluteChildCode, null),
    ['/src/absoluteparent.js']: codeFile(defaultAbsoluteParentCode, null),
    ['/src/app.js']: codeFile(defaultAppCode, null),
    ['/src/flexchild.js']: codeFile(defaultFlexChildCode, null),
    ['/src/flexparent.js']: codeFile(defaultFlexParentCode, null),
    ['/utopia/storyboard.js']: codeFile(
      `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      style={{
        width: 744,
        height: 1133,
        display: 'flex',
        flexDirection: 'column',
        left: -52,
        top: 9,
        position: 'absolute',
      }}
      data-label='Main Scene'
      data-uid='mainscene'
      data-testid='mainscene'
    >
      <App style={{}} data-uid='app' data-testid='app' />
    </Scene>
  </Storyboard>
)`,
      null,
    ),
  })
}

function getFileCode(renderResult: EditorRenderResult, filename: string): string {
  const projectContents = renderResult.getEditorState().editor.projectContents
  const possibleFile = getProjectFileByFilePath(projectContents, filename)
  if (possibleFile == null) {
    throw new Error(`Could not find file ${filename}.`)
  } else if (isTextFile(possibleFile)) {
    return possibleFile.fileContents.code
  } else {
    throw new Error(`File ${filename} was an unexpected type: ${possibleFile.type}.`)
  }
}

describe('Absolute Reparent Strategy (Multi-File)', () => {
  it('reparents to the end', async () => {
    const renderResult = await renderTestEditorWithProjectContent(
      makeTestProjectContents(),
      'await-first-dom-report',
    )

    await act(() => {
      return renderResult.dispatch(
        [
          setFocusedElement(
            EP.fromString('storyboard/mainscene/app:container/absoluteparentinapp'),
          ),
        ],
        true,
      )
    })
    await act(() => {
      return renderResult.dispatch(
        [
          selectComponents(
            [
              EP.fromString(
                'storyboard/mainscene/app:container/absoluteparentinapp:absoluteparent/absolutechildinparent',
              ),
            ],
            false,
          ),
        ],
        true,
      )
    })

    const absoluteChild = await renderResult.renderedDOM.findByTestId('absolutechild')
    const absoluteChildRect = absoluteChild.getBoundingClientRect()
    const absoluteChildCenter = {
      x: absoluteChildRect.x + absoluteChildRect.width / 2,
      y: absoluteChildRect.y + absoluteChildRect.height / 2,
    }
    const flexParent = await renderResult.renderedDOM.findByTestId('flexparent')
    const flexParentRect = flexParent.getBoundingClientRect()
    const dropTargetPoint = {
      x: flexParentRect.x - 10,
      y: flexParentRect.y + 10,
    }

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await mouseDragFromPointToPoint(canvasControlsLayer, absoluteChildCenter, dropTargetPoint, {
      modifiers: cmdModifier,
    })

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getFileCode(renderResult, '/src/absolutechild.js')).toEqual(
      formatTestProjectCode(defaultAbsoluteChildCode),
    )
    expect(getFileCode(renderResult, '/src/absoluteparent.js')).toEqual(
      formatTestProjectCode(`import * as React from 'react'
import { AbsoluteChild } from './absolutechild'
export const AbsoluteParent = () => {
  return (
    <div
      style={{
        position: 'absolute',
        width: 251,
        height: 500,
        left: 0,
        top: 0,
        backgroundColor: 'lightblue',
      }}
      data-uid='absoluteparent'
      data-testid='absoluteparent'
    />
  )
}
`),
    )
    expect(getFileCode(renderResult, '/src/app.js')).toEqual(
      formatTestProjectCode(`import * as React from 'react'
import { AbsoluteParent } from './absoluteparent'
import { FlexParent } from './flexparent'
import { AbsoluteChild } from '/src/absolutechild.js'

export var App = () => {
  return (
    <div
      style={{
        position: 'absolute',
        width: 700,
        height: 600,
        backgroundColor: 'white',
      }}
      data-uid='container'
      data-testid='container'
    >
      <AbsoluteParent data-uid='absoluteparentinapp' />
      <FlexParent data-uid='flexparentinapp' />
      <AbsoluteChild
        data-uid='absolutechildinparent'
        style={{ left: 294, top: -40 }}
      />
    </div>
  )
}
`),
    )
    expect(getFileCode(renderResult, '/src/flexchild.js')).toEqual(
      formatTestProjectCode(defaultFlexChildCode),
    )
    expect(getFileCode(renderResult, '/src/flexparent.js')).toEqual(
      formatTestProjectCode(defaultFlexParentCode),
    )
  })
})
