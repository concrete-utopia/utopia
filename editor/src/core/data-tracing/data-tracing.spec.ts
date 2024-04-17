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
import { jsIdentifier } from '../shared/element-template'
import type { ElementPath } from '../shared/project-file-types'
import * as PP from '../shared/property-path'
import { dataTracingToLiteralAttribute, traceDataFromProp } from './data-tracing'

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
      [],
    )

    expect(traceResult).toEqual(
      dataTracingToLiteralAttribute(EP.fromString('sb/app:my-component'), PP.create('title'), []),
    )
  })

  it('Traces back a regular prop to a string literal jsx attribute', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStoryboard(`
      function MyComponent(props) {
        return <div data-uid='component-root' title={props.title} />
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
      [],
    )

    expect(traceResult).toEqual(
      dataTracingToLiteralAttribute(EP.fromString('sb/app:my-component'), PP.create('title'), []),
    )
  })

  it('Traces back a destrucuted prop to a string literal jsx attribute', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStoryboard(`
      function MyComponent({ propA, title, propC, ...restOfProps }) {
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
      [],
    )

    expect(traceResult).toEqual(
      dataTracingToLiteralAttribute(EP.fromString('sb/app:my-component'), PP.create('title'), []),
    )
  })

  it('Traces back a spread destructured prop to a string literal jsx attribute', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStoryboard(`
      function MyComponent({ propA, propC, ...restOfProps }) {
        return <div data-uid='component-root' title={restOfProps.title} />
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
      [],
    )

    expect(traceResult).toEqual(
      dataTracingToLiteralAttribute(EP.fromString('sb/app:my-component'), PP.create('title'), []),
    )
  })

  it('Traces back an evil undrilled prop use to a string literal jsx attribute', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStoryboard(`
      function MyInnerComponent(props) {
        return <div data-uid='component-inner-root' title={props.allProps.title} />
      }

      function MyComponent(props) {
        return <MyInnerComponent data-uid='component-root' allProps={props} />
      }

      function App() {
        return <MyComponent data-uid='my-component' title='string literal here' />
      }
      `),
      'await-first-dom-report',
    )

    await focusOnComponentForTest(editor, EP.fromString('sb/app:my-component:component-root'))

    const traceResult = traceDataFromProp(
      EPP.create(
        EP.fromString('sb/app:my-component:component-root:component-inner-root'),
        PP.create('title'),
      ),
      editor.getEditorState().editor.jsxMetadata,
      editor.getEditorState().editor.projectContents,
      [],
    )

    expect(traceResult).toEqual(
      dataTracingToLiteralAttribute(EP.fromString('sb/app:my-component'), PP.create('title'), []),
    )
  })

  it('Traces back a prop to an object literal jsx attribute with a data path', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStoryboard(`
      function MyComponent({doc}) {
        return <div data-uid='component-root' title={doc.title} />
      }

      function App() {
        return <MyComponent data-uid='my-component' doc={{title: 'string literal here'}} />
      }
      `),
      'await-first-dom-report',
    )

    await focusOnComponentForTest(editor, EP.fromString('sb/app:my-component'))

    const traceResult = traceDataFromProp(
      EPP.create(EP.fromString('sb/app:my-component:component-root'), PP.create('title')),
      editor.getEditorState().editor.jsxMetadata,
      editor.getEditorState().editor.projectContents,
      [],
    )

    expect(traceResult).toEqual(
      dataTracingToLiteralAttribute(EP.fromString('sb/app:my-component'), PP.create('doc'), [
        'title',
      ]),
    )
  })

  it('Traces back a prop to an object literal jsx attribute with a deep data path', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStoryboard(`
      function MyComponent({doc}) {
        return <div data-uid='component-root' title={doc.very.deep.title} />
      }

      function App() {
        return <MyComponent data-uid='my-component' doc={{very: { deep: { title: 'string literal here' } } }} />
      }
      `),
      'await-first-dom-report',
    )

    await focusOnComponentForTest(editor, EP.fromString('sb/app:my-component'))

    const traceResult = traceDataFromProp(
      EPP.create(EP.fromString('sb/app:my-component:component-root'), PP.create('title')),
      editor.getEditorState().editor.jsxMetadata,
      editor.getEditorState().editor.projectContents,
      [],
    )

    expect(traceResult).toEqual(
      dataTracingToLiteralAttribute(EP.fromString('sb/app:my-component'), PP.create('doc'), [
        'very',
        'deep',
        'title',
      ]),
    )
  })

  it('Traces back a prop to an object literal jsx attribute with a deep data path through multiple components', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStoryboard(`
      function MyInnerComponent({title}) {
        return <div data-uid='inner-component-root' innerTitle={title.value} />
      }
      
      function MyComponent({doc}) {
        return <MyInnerComponent data-uid='component-root' title={doc.very.deep.title} />
      }

      function App() {
        return <MyComponent data-uid='my-component' doc={{very: { deep: { title: {value: 'string literal here'} } } }} />
      }
      `),
      'await-first-dom-report',
    )

    await focusOnComponentForTest(editor, EP.fromString('sb/app:my-component:component-root'))

    const traceResult = traceDataFromProp(
      EPP.create(
        EP.fromString('sb/app:my-component:component-root:inner-component-root'),
        PP.create('innerTitle'),
      ),
      editor.getEditorState().editor.jsxMetadata,
      editor.getEditorState().editor.projectContents,
      [],
    )

    expect(traceResult).toEqual(
      dataTracingToLiteralAttribute(EP.fromString('sb/app:my-component'), PP.create('doc'), [
        'very',
        'deep',
        'title',
        'value',
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
