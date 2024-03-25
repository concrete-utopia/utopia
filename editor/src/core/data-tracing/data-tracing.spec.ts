import type { EditorRenderResult } from '../../components/canvas/ui-jsx.test-utils'
import {
  formatTestProjectCode,
  renderTestEditorWithCode,
} from '../../components/canvas/ui-jsx.test-utils'
import { resetCanvas, setFocusedElement } from '../../components/editor/actions/action-creators'
import * as EPP from '../../components/template-property-path'
import { BakedInStoryboardVariableName } from '../model/scene-utils'
import { right } from '../shared/either'
import * as EP from '../shared/element-path'
import type { ElementPath } from '../shared/project-file-types'
import * as PP from '../shared/property-path'
import { traceDataFromProp } from './data-tracing'

describe('Data Tracing', () => {
  it('Pointing it at a string literal prop just returns the string literal prop all right', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStoryboard(`
      function App() {
        return <div data-uid='my-component' title='string literal here' />
      }
      `),
      'await-first-dom-report',
    )

    await focusOnComponentForTest(editor, EP.fromString('sb/app'))

    const traceResult = traceDataFromProp(
      EPP.create(EP.fromString('sb/app:my-component'), PP.create('title')),
      editor.getEditorState().editor.jsxMetadata,
      editor.getEditorState().editor.projectContents,
    )

    expect(traceResult).toEqual(
      right([EPP.create(EP.fromString('sb/app:my-component'), PP.create('title'))]),
    )
  })

  it('Traces back a prop to a string literal jsx attribute', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStoryboard(`
      function MyComponent({title}) {
        return <div data-uid='component-root' title={title} />
      }

      function App() {
        return <MyComponent data-uid='my-component' title='string literal here' />
      }
      `),
      'await-first-dom-report',
    )

    await focusOnComponentForTest(editor, EP.fromString('sb/app:my-component'))

    const traceResult = traceDataFromProp(
      EPP.create(EP.fromString('sb/app:my-component:component-root'), PP.create('title')),
      editor.getEditorState().editor.jsxMetadata,
      editor.getEditorState().editor.projectContents,
    )

    expect(traceResult).toEqual(
      right([
        EPP.create(EP.fromString('sb/app:my-component'), PP.create('title')),
        EPP.create(EP.fromString('sb/app:my-component:component-root'), PP.create('title')),
      ]),
    )
  })
})

function makeTestProjectCodeWithStoryboard(codeForComponents: string): string {
  const code = `
    import * as React from 'react'
    import { Scene, Storyboard } from 'utopia-api'

    ${codeForComponents}

    export var ${BakedInStoryboardVariableName} = (props) => {
      return (
        <Storyboard data-uid='sb'>
          <App data-uid='app'/>
        </Storyboard>
      )
    }
  `

  return formatTestProjectCode(code)
}

async function focusOnComponentForTest(
  editor: EditorRenderResult,
  elementPath: ElementPath,
): Promise<void> {
  function arrayOfPrefixes(ep: ElementPath): Array<ElementPath> {
    return ep.parts.map((_, index) => ({
      type: 'elementpath',
      parts: ep.parts.slice(0, index + 1),
    }))
  }

  const pathsLeadingToComponent = arrayOfPrefixes(elementPath)

  for await (const path of pathsLeadingToComponent) {
    await editor.dispatch([setFocusedElement(path)], true)
    await editor.dispatch([resetCanvas()], true)
    await editor.getDispatchFollowUpActionsFinished()
  }
}
