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
import {
  dataTracingToAHookCall,
  dataTracingToLiteralAttribute,
  traceDataFromProp,
} from './data-tracing'

describe('Data Tracing', () => {
  describe('Tracing through direct props use', () => {
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

    it('Supports elements other than the root element', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
      function App() {
        return (
          <div data-uid='my-component'>
            <div data-uid='inner-child' title='string literal here' />
          </div>
        )
      }
      `),
        'await-first-dom-report',
      )

      await focusOnComponentForTest(editor, EP.fromString('sb/app'))

      const traceResult = traceDataFromProp(
        EPP.create(EP.fromString('sb/app:my-component/inner-child'), PP.create('title')),
        editor.getEditorState().editor.jsxMetadata,
        editor.getEditorState().editor.projectContents,
        [],
      )

      expect(traceResult).toEqual(
        dataTracingToLiteralAttribute(
          EP.fromString('sb/app:my-component/inner-child'),
          PP.create('title'),
          [],
        ),
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

    it('Traces back a regular prop of a child element', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
      function MyComponent(props) {
        return (
          <div data-uid='component-root'>
            <div data-uid='inner-child' title={props.title} />
          </div>
        )
      }

      function App() {
        return <MyComponent data-uid='my-component' title='string literal here' />
      }
      `),
        'await-first-dom-report',
      )

      await focusOnComponentForTest(editor, EP.fromString('sb/app:my-component'))

      const traceResult = traceDataFromProp(
        EPP.create(
          EP.fromString('sb/app:my-component:component-root/inner-child'),
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

  describe('Element Access', () => {
    it('simple element access is supported', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
        function useLoaderData() {
          return { reviews: [{ hello: 'there'}] }
        }

      function MyComponent(props) {
        const data = useLoaderData()
        const reviews = data.reviews
        return <div data-uid='component-root' title={reviews[0].hello} />
      }

      function App() {
        return <MyComponent data-uid='my-component' />
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
        dataTracingToAHookCall(
          EP.fromString('sb/app:my-component:component-root'),
          'useLoaderData',
          ['reviews', '0', 'hello'],
        ),
      )
    })
  })

  describe('Data tracing to a useLoaderData() hook', () => {
    it('Traces back a prop to a useLoaderData() hook', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
        function useLoaderData() {
          return { title: 'string literal here' }
        }

      function MyComponent(props) {
        const data = useLoaderData()
        return <div data-uid='component-root'>
          <div data-uid='inner-child' title={data.title}>{data.title}</div>
        </div>
      }

      function App() {
        return <MyComponent data-uid='my-component' />
      }
      `),
        'await-first-dom-report',
      )

      await focusOnComponentForTest(editor, EP.fromString('sb/app:my-component'))

      const traceResult = traceDataFromProp(
        EPP.create(
          EP.fromString('sb/app:my-component:component-root/inner-child'),
          PP.create('title'),
        ),
        editor.getEditorState().editor.jsxMetadata,
        editor.getEditorState().editor.projectContents,
        [],
      )

      expect(traceResult).toEqual(
        dataTracingToAHookCall(
          EP.fromString('sb/app:my-component:component-root'),
          'useLoaderData',
          ['title'],
        ),
      )
    })

    it('Traces back a prop to a destructured useLoaderData() hook', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
        function useLoaderData() {
          return { title: 'string literal here' }
        }

      function MyComponent(props) {
        const { title } = useLoaderData()
        return <div data-uid='component-root' title={title} />
      }

      function App() {
        return <MyComponent data-uid='my-component' />
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
        dataTracingToAHookCall(
          EP.fromString('sb/app:my-component:component-root'),
          'useLoaderData',
          ['title'],
        ),
      )
    })

    it('Traces back a prop to a useLoaderData() hook through assignment indirections', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
        function useLoaderData() {
          return { title: 'string literal here' }
        }

      function MyComponent(props) {
        const data = useLoaderData()
        const { title } = data
        return <div data-uid='component-root' title={title} />
      }

      function App() {
        return <MyComponent data-uid='my-component' />
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
        dataTracingToAHookCall(
          EP.fromString('sb/app:my-component:component-root'),
          'useLoaderData',
          ['title'],
        ),
      )
    })

    it('Traces back a prop through local consts to useLoaderData() hook', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
        function useLoaderData() {
          return { title: 'string literal here' }
        }

      function MyComponent(props) {
        const data = useLoaderData()
        const title = data.title
        return <div data-uid='component-root' title={title} />
      }

      function App() {
        return <MyComponent data-uid='my-component' />
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
        dataTracingToAHookCall(
          EP.fromString('sb/app:my-component:component-root'),
          'useLoaderData',
          ['title'],
        ),
      )
    })

    it('Traces back a prop to a useLoaderData with a deep data path through multiple components', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
      function MyInnerComponent({title}) {
        return <div data-uid='inner-component-root' innerTitle={title.value} />
      }
      
      function MyComponent({doc}) {
        return <MyInnerComponent data-uid='component-root' title={doc.very.deep.title} />
      }

      function useLoaderData() {
        return {very: { deep: { title: {value: 'string literal here'} } } }
      }

      function App() {
        const data = useLoaderData()
        return <MyComponent data-uid='my-component' doc={data} />
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
        dataTracingToAHookCall(EP.fromString('sb/app:my-component'), 'useLoaderData', [
          'very',
          'deep',
          'title',
          'value',
        ]),
      )
    })

    it('Traces back a prop to a useLoaderData with a deep data path through multiple components and destructure', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
      function MyInnerComponent({title}) {
        return <div data-uid='inner-component-root' innerTitle={title.value} />
      }
      
      function MyComponent({doc}) {
        return <MyInnerComponent data-uid='component-root' title={doc.title} />
      }

      function useLoaderData() {
        return {very: { deep: { title: {value: 'string literal here'} } } }
      }

      function App() {
        const { very } = useLoaderData()
        const { deep } = very
        return <MyComponent data-uid='my-component' doc={deep} />
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
        dataTracingToAHookCall(EP.fromString('sb/app:my-component'), 'useLoaderData', [
          'very',
          'deep',
          'title',
          'value',
        ]),
      )
    })
  })

  describe('Tracing through local variable indirection', () => {
    it('Traces back a regular prop to a string literal jsx attribute via a single const indirection', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
      function MyComponent(props) {
        const title = props.title
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

    it('Traces back a regular prop to a string literal jsx attribute via a destructured indirection', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
      function MyComponent(props) {
        const { title } = props
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
  })

  xdescribe('Tracing through a map function', () => {
    it('Traces back a prop to a string literal jsx attribute via a map function', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
      function MyComponent(props) {
        return <div data-uid='component-root'>
          {
            // @utopia/uid=map
            props.titles.map((title) => 
              (<div data-uid='mapped' key={title} title={title}>{title}</div>)
            )
          }
        </div>
      }

      function App() {
        return <MyComponent data-uid='my-component' titles={['a', 'b', 'c']} />
      }
      `),
        'await-first-dom-report',
      )

      await focusOnComponentForTest(editor, EP.fromString('sb/app:my-component:component-root'))

      const traceResult = traceDataFromProp(
        EPP.create(
          EP.fromString('sb/app:my-component:component-root/map/mapped~~~2'),
          PP.create('title'),
        ),
        editor.getEditorState().editor.jsxMetadata,
        editor.getEditorState().editor.projectContents,
        [],
      )

      expect(traceResult).toEqual(
        dataTracingToLiteralAttribute(
          EP.fromString('sb/app:my-component'),
          PP.create('titles'),
          [],
        ),
      )
    })
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
