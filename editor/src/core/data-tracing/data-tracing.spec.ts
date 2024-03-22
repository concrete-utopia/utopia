import {
  formatTestProjectCode,
  renderTestEditorWithCode,
} from '../../components/canvas/ui-jsx.test-utils'
import * as EPP from '../../components/template-property-path'
import { BakedInStoryboardVariableName } from '../model/scene-utils'
import * as PP from '../shared/property-path'
import * as EP from '../shared/element-path'
import { traceDataFromProp } from './data-tracing'
import { resetCanvas, setFocusedElement } from '../../components/editor/actions/action-creators'
import { right } from '../shared/either'

describe('Data Tracing', () => {
  it('Traces back a prop to a string literal jsx attribute', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStoryboard(`
      function C({title}) {
        return <div data-uid='title'>{title}</div>
      }

      function MyComponent({title}) {
        return <C data-uid='component-root' title={title} />
      }

      function App() {
        return <MyComponent data-uid='my-component' title='string literal here' />
      }
      `),
      'await-first-dom-report',
    )

    await editor.dispatch([setFocusedElement(EP.fromString('sb/app'))], true)
    await editor.dispatch([resetCanvas()], true)
    await editor.getDispatchFollowUpActionsFinished()

    await editor.dispatch([setFocusedElement(EP.fromString('sb/app:my-component'))], true)
    await editor.dispatch([resetCanvas()], true)
    await editor.getDispatchFollowUpActionsFinished()

    const traceResultForDirectlyPointingAtALiteral = traceDataFromProp(
      EPP.create(EP.fromString('sb/app:my-component'), PP.create('title')),
      editor.getEditorState().editor.jsxMetadata,
      editor.getEditorState().editor.projectContents,
    )

    expect(traceResultForDirectlyPointingAtALiteral).toEqual(
      right([EPP.create(EP.fromString('sb/app:my-component'), PP.create('title'))]),
    )

    const traceResultPointingAtAnIdentifierPointingToAProp = traceDataFromProp(
      EPP.create(EP.fromString('sb/app:my-component:component-root'), PP.create('title')),
      editor.getEditorState().editor.jsxMetadata,
      editor.getEditorState().editor.projectContents,
    )

    expect(traceResultPointingAtAnIdentifierPointingToAProp).toEqual(
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
