import type { JSIdentifier, JSXTextBlock } from 'utopia-shared/src/types'
import type { EditorRenderResult } from '../../components/canvas/ui-jsx.test-utils'
import {
  formatTestProjectCode,
  renderTestEditorWithCode,
} from '../../components/canvas/ui-jsx.test-utils'
import { resetCanvas, setFocusedElement } from '../../components/editor/actions/action-creators'
import { getElementFromProjectContents } from '../../components/editor/store/editor-state'
import * as EPP from '../../components/template-property-path'
import { BakedInStoryboardVariableName } from '../model/scene-utils'
import { right } from '../shared/either'
import * as EP from '../shared/element-path'
import type { JSExpression } from '../shared/element-template'
import type { ElementPath } from '../shared/project-file-types'
import * as PP from '../shared/property-path'
import {
  dataPathNotPossible,
  dataPathSuccess,
  dataTracingFailed,
  dataTracingToAHookCall,
  dataTracingToElementAtScope,
  dataTracingToLiteralAssignment,
  dataTracingToLiteralAttribute,
  traceDataFromElement,
  traceDataFromProp,
  traceDataFromVariableName,
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToLiteralAttribute(
          EP.fromString('sb/app:my-component'),
          PP.create('title'),
          dataPathSuccess([]),
        ),
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToLiteralAttribute(
          EP.fromString('sb/app:my-component/inner-child'),
          PP.create('title'),
          dataPathSuccess([]),
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToLiteralAttribute(
          EP.fromString('sb/app:my-component'),
          PP.create('title'),
          dataPathSuccess([]),
        ),
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToLiteralAttribute(
          EP.fromString('sb/app:my-component'),
          PP.create('title'),
          dataPathSuccess([]),
        ),
      )
    })

    it('Traces back a destructed prop to a string literal jsx attribute', async () => {
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToLiteralAttribute(
          EP.fromString('sb/app:my-component'),
          PP.create('title'),
          dataPathSuccess([]),
        ),
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToLiteralAttribute(
          EP.fromString('sb/app:my-component'),
          PP.create('title'),
          dataPathSuccess([]),
        ),
      )
    })

    it('Traces back an evil undrilled prop use to a string literal jsx attribute', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
      function MyInnerComponent({ allProps }) {
        return <div data-uid='component-inner-root' title={allProps.title} />
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToLiteralAttribute(
          EP.fromString('sb/app:my-component'),
          PP.create('title'),
          dataPathSuccess([]),
        ),
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToLiteralAttribute(
          EP.fromString('sb/app:my-component'),
          PP.create('doc'),
          dataPathSuccess(['title']),
        ),
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToLiteralAttribute(
          EP.fromString('sb/app:my-component'),
          PP.create('doc'),
          dataPathSuccess(['very', 'deep', 'title']),
        ),
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToLiteralAttribute(
          EP.fromString('sb/app:my-component'),
          PP.create('doc'),
          dataPathSuccess(['very', 'deep', 'title', 'value']),
        ),
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

        return <div data-uid='component-root' title={reviews[0]['hello'] } />
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToAHookCall(
          EP.fromString('sb/app:my-component:component-root'),
          'useLoaderData',
          dataPathSuccess(['reviews', '0', 'hello']),
        ),
      )
    })

    it('more complex access is supported', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
        function useLoaderData() {
          return { reviews: [{ hello: 'there'}] }
        }

      function MyComponent(props) {
        const { reviews, otherStuff } = useLoaderData()
        const review = reviews[0]

        return <div data-uid='component-root' title={review['hello']} />
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToAHookCall(
          EP.fromString('sb/app:my-component:component-root'),
          'useLoaderData',
          dataPathSuccess(['reviews', '0', 'hello']),
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToAHookCall(
          EP.fromString('sb/app:my-component:component-root'),
          'useLoaderData',
          dataPathSuccess(['title']),
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToAHookCall(
          EP.fromString('sb/app:my-component:component-root'),
          'useLoaderData',
          dataPathSuccess(['title']),
        ),
      )
    })

    it('traces back through a spread value in an object destructured hook value', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
        function useObject() {
          return { first: 'first', second: 'second', third: 'third', fourth: 'fourth' }
        }

      function MyComponent(props) {
        const { first, second, ...rest } = useObject()
        return <div data-uid='component-root' title={rest.third} />
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToAHookCall(
          EP.fromString('sb/app:my-component:component-root'),
          'useObject',
          dataPathSuccess(['third']),
        ),
      )
    })

    it('traces back through an array destructured hook, but without a path', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
        function useArray() {
          return [ 'first', 'second', 'string literal here']
        }

      function MyComponent(props) {
        const [ _first, _second, ...rest ] = useArray()
        return <div data-uid='component-root' title={rest} />
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToAHookCall(
          EP.fromString('sb/app:my-component:component-root'),
          'useArray',
          dataPathNotPossible,
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToAHookCall(
          EP.fromString('sb/app:my-component:component-root'),
          'useLoaderData',
          dataPathSuccess(['title']),
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToAHookCall(
          EP.fromString('sb/app:my-component:component-root'),
          'useLoaderData',
          dataPathSuccess(['title']),
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToAHookCall(
          EP.fromString('sb/app:my-component'),
          'useLoaderData',
          dataPathSuccess(['very', 'deep', 'title', 'value']),
        ),
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToAHookCall(
          EP.fromString('sb/app:my-component'),
          'useLoaderData',
          dataPathSuccess(['very', 'deep', 'title', 'value']),
        ),
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToLiteralAttribute(
          EP.fromString('sb/app:my-component'),
          PP.create('title'),
          dataPathSuccess([]),
        ),
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToLiteralAttribute(
          EP.fromString('sb/app:my-component'),
          PP.create('title'),
          dataPathSuccess([]),
        ),
      )
    })

    it('Traces back a regular prop to a string literal jsx attribute via a destructured indirection that renames', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
      function MyComponent(props) {
        const { title: actualTitle } = props
        return <div data-uid='component-root' title={actualTitle} />
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
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToLiteralAttribute(
          EP.fromString('sb/app:my-component'),
          PP.create('title'),
          dataPathSuccess([]),
        ),
      )
    })
  })

  describe('Tracing through a map function', () => {
    it('Traces back a prop to a string literal jsx attribute via a map function', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
      function MyComponent(props) {
        return <div data-uid='component-root'>
          {
            // @utopia/uid=map
            props.titles.map((title) => 
              (<div data-uid='mapped' key={title}>
                <div data-uid='mapped-child' title={title}>{title}</div>
              </div>)
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
          EP.fromString('sb/app:my-component:component-root/map/mapped~~~2/mapped-child'),
          PP.create('title'),
        ),
        editor.getEditorState().editor.jsxMetadata,
        editor.getEditorState().editor.projectContents,
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToLiteralAttribute(
          EP.fromString('sb/app:my-component'),
          PP.create('titles'),
          dataPathSuccess(['1']),
        ),
      )
    })

    it('Works with a simple hook case', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
        function useLoaderData() {
          return { reviews: [{ title: 'Good' }, { title: 'Bad' }] }
        }

      function MyComponent(props) {
        const { reviews } = useLoaderData()
        return <div data-uid='component-root'>
          {
            // @utopia/uid=map
            reviews.map((review) => 
              (<div data-uid='mapped' key={review.title}>
                <div data-uid='mapped-child' title={review.title}>{review.title}</div>
              </div>)
            )
          }
        </div>
      }

      function App() {
        return <MyComponent data-uid='my-component' />
      }
      `),
        'await-first-dom-report',
      )

      await focusOnComponentForTest(editor, EP.fromString('sb/app:my-component:component-root'))

      const traceResult = traceDataFromProp(
        EPP.create(
          EP.fromString('sb/app:my-component:component-root/map/mapped~~~2/mapped-child'),
          PP.create('title'),
        ),
        editor.getEditorState().editor.jsxMetadata,
        editor.getEditorState().editor.projectContents,
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToAHookCall(
          EP.fromString('sb/app:my-component:component-root'),
          'useLoaderData',
          dataPathSuccess(['reviews', '1', 'title']),
        ),
      )
    })

    it('Works with a destructure in the map function', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
        function useLoaderData() {
          return { reviews: [{ title: 'Good' }, { title: 'Bad' }] }
        }

      function MyComponent(props) {
        const { reviews } = useLoaderData()
        return <div data-uid='component-root'>
          {
            // @utopia/uid=map
            reviews.map(({ title }) => 
              (<div data-uid='mapped' key={title}>
                <div data-uid='mapped-child' title={title}>{title}</div>
              </div>)
            )
          }
        </div>
      }

      function App() {
        return <MyComponent data-uid='my-component' />
      }
      `),
        'await-first-dom-report',
      )

      await focusOnComponentForTest(editor, EP.fromString('sb/app:my-component:component-root'))

      const traceResult = traceDataFromProp(
        EPP.create(
          EP.fromString('sb/app:my-component:component-root/map/mapped~~~2/mapped-child'),
          PP.create('title'),
        ),
        editor.getEditorState().editor.jsxMetadata,
        editor.getEditorState().editor.projectContents,
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToAHookCall(
          EP.fromString('sb/app:my-component:component-root'),
          'useLoaderData',
          dataPathSuccess(['reviews', '1', 'title']),
        ),
      )
    })
  })

  describe('Tracing data from an element', () => {
    it('Traces back to a hook call from an element in scope', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
        function MyInnerComponent({title}) {
          return <div data-uid='inner-component-root'>{
            // @utopia/uid=map
            title.value.map((el) => <div key={el}>{el}</div>)
          }</div>
        }
        
        function MyComponent({doc}) {
          return <MyInnerComponent data-uid='component-root' title={doc.title} />
        }
  
        function useLoaderData() {
          return {very: { deep: { title: {value: ['hello', 'world']} } } }
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

      const mappedElement: JSExpression = (() => {
        const mapElement = getElementFromProjectContents(
          EP.fromString('sb/app:my-component:component-root:inner-component-root/map'),
          editor.getEditorState().editor.projectContents,
        )
        if (mapElement == null || mapElement.type !== 'JSX_MAP_EXPRESSION') {
          throw new Error('Could not find map element')
        }
        return mapElement.valueToMap
      })()

      const traceResult = traceDataFromElement(
        mappedElement,
        EP.fromString('sb/app:my-component:component-root:inner-component-root'),
        editor.getEditorState().editor.jsxMetadata,
        editor.getEditorState().editor.projectContents,
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToAHookCall(
          EP.fromString('sb/app:my-component'),
          'useLoaderData',
          dataPathSuccess(['very', 'deep', 'title', 'value']),
        ),
      )
    })

    it('The element in scope is a child prop', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
        function MyInnerComponent({name}) {
          return <div data-uid='inner-component-root'>
            Hello, {name}!
          </div>
        }
        
        function MyComponent({doc}) {
          return <MyInnerComponent data-uid='component-root' name={doc.name} />
        }
  
        function useLoaderData() {
          return {very: { deep: { name: "world" } } }
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

      const mappedElement: JSIdentifier = (() => {
        const rootElement = getElementFromProjectContents(
          EP.fromString('sb/app:my-component:component-root:inner-component-root'),
          editor.getEditorState().editor.projectContents,
        )
        if (rootElement == null || rootElement.type !== 'JSX_ELEMENT') {
          throw new Error('Could not find map element')
        }
        const target = rootElement.children[1]
        if (target == null || target.type !== 'JS_IDENTIFIER') {
          throw new Error('Could not find target identifier')
        }
        return target
      })()

      const traceResult = traceDataFromElement(
        mappedElement,
        EP.fromString('sb/app:my-component:component-root:inner-component-root'),
        editor.getEditorState().editor.jsxMetadata,
        editor.getEditorState().editor.projectContents,
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToAHookCall(
          EP.fromString('sb/app:my-component'),
          'useLoaderData',
          dataPathSuccess(['very', 'deep', 'name']),
        ),
      )
    })

    it('The element in scope is a text literal child', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
        function App() {
          return <div data-uid='app-root'>
            Hello, {"world"}!
          </div>
        }
        `),
        'await-first-dom-report',
      )

      await focusOnComponentForTest(editor, EP.fromString('sb/app:app-root'))

      const mappedElement: JSXTextBlock = (() => {
        const rootElement = getElementFromProjectContents(
          EP.fromString('sb/app:app-root'),
          editor.getEditorState().editor.projectContents,
        )
        if (rootElement == null || rootElement.type !== 'JSX_ELEMENT') {
          throw new Error('Could not find map element')
        }
        const target = rootElement.children[0]
        if (target == null || target.type !== 'JSX_TEXT_BLOCK') {
          throw new Error('Could not find target identifier')
        }
        return target
      })()

      const traceResult = traceDataFromElement(
        mappedElement,
        EP.fromString('sb/app:my-component:app-root'),
        editor.getEditorState().editor.jsxMetadata,
        editor.getEditorState().editor.projectContents,
        dataPathSuccess([]),
      )

      expect(traceResult).toEqual(
        dataTracingToElementAtScope(
          EP.fromString('sb/app:my-component:app-root'),
          mappedElement,
          dataPathSuccess([]),
        ),
      )
    })
  })

  describe('Tracing data from the arbitrary js block of a component', () => {
    it('can trace data from the arbitrary js block', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
        function MyComponent({ doc }) {
          return <h1 data-uid='component-root'>{doc.title.value}</h1>
        }
  
        function useLoaderData() {
          return { very: { deep: { title: {value: ['hello', 'world'] } }, a: [1, 2] } }
        }
  
        function App() {
          const { very } = useLoaderData()
          const { deep, a } = very
          const deepButWithAccess = very.deep
          
          const [helloFromDestructuredArray] = a

          return <MyComponent data-uid='my-component' doc={deep} />
        }
        `),
        'await-first-dom-report',
      )

      await focusOnComponentForTest(editor, EP.fromString('sb/app:my-component'))

      {
        const trace = traceDataFromVariableName(
          EP.fromString('sb/app:my-component'),
          'deep',
          editor.getEditorState().editor.jsxMetadata,
          editor.getEditorState().editor.projectContents,
          dataPathSuccess([]),
        )

        expect(trace).toEqual(
          dataTracingToAHookCall(
            EP.fromString('sb/app:my-component'),
            'useLoaderData',
            dataPathSuccess(['very', 'deep']),
          ),
        )
      }
      {
        const trace = traceDataFromVariableName(
          EP.fromString('sb/app:my-component'),
          'deepButWithAccess',
          editor.getEditorState().editor.jsxMetadata,
          editor.getEditorState().editor.projectContents,
          dataPathSuccess([]),
        )

        expect(trace).toEqual(
          dataTracingToAHookCall(
            EP.fromString('sb/app:my-component'),
            'useLoaderData',
            dataPathSuccess(['very', 'deep']),
          ),
        )
      }
      {
        const trace = traceDataFromVariableName(
          EP.fromString('sb/app:my-component'),
          'helloFromDestructuredArray',
          editor.getEditorState().editor.jsxMetadata,
          editor.getEditorState().editor.projectContents,
          dataPathSuccess([]),
        )

        expect(trace).toEqual(
          dataTracingToAHookCall(
            EP.fromString('sb/app:my-component'),
            'useLoaderData',
            dataPathSuccess(['very', 'a', '0', 'helloFromDestructuredArray']),
          ),
        )
      }
    })

    it('can to a literal-valued variable in the arbitrary js block', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboard(`
        function MyComponent({ doc }) {

          const valueFromTheDeep = doc.title.value

          return <h1 data-uid='component-root'>{doc.title.value}</h1>
        }
  
        function App() {
          const label = 'yes'
          const loaded = true
          const words = ['hello', 'cheese']
          const deeplyNestedObject = { very: { deep: { title: {value: ['hello', 'world'] } }, a: [1, 2] } }
          const { very } = deeplyNestedObject

          return <MyComponent data-uid='my-component' doc={very.deep} />
        }
        `),
        'await-first-dom-report',
      )

      await focusOnComponentForTest(editor, EP.fromString('sb/app:my-component'))

      {
        const trace = traceDataFromVariableName(
          EP.fromString('sb/app:my-component'),
          'label',
          editor.getEditorState().editor.jsxMetadata,
          editor.getEditorState().editor.projectContents,
          dataPathSuccess([]),
        )

        expect(trace).toEqual(
          dataTracingToLiteralAssignment(EP.fromString('sb/app:my-component'), dataPathSuccess([])),
        )
      }
      {
        const trace = traceDataFromVariableName(
          EP.fromString('sb/app:my-component'),
          'loaded',
          editor.getEditorState().editor.jsxMetadata,
          editor.getEditorState().editor.projectContents,
          dataPathSuccess([]),
        )

        expect(trace).toEqual(
          dataTracingToLiteralAssignment(EP.fromString('sb/app:my-component'), dataPathSuccess([])),
        )
      }
      {
        const trace = traceDataFromVariableName(
          EP.fromString('sb/app:my-component'),
          'words',
          editor.getEditorState().editor.jsxMetadata,
          editor.getEditorState().editor.projectContents,
          dataPathSuccess([]),
        )

        expect(trace).toEqual(
          dataTracingToLiteralAssignment(EP.fromString('sb/app:my-component'), dataPathSuccess([])),
        )
      }
      {
        const trace = traceDataFromVariableName(
          EP.fromString('sb/app:my-component'),
          'deeplyNestedObject',
          editor.getEditorState().editor.jsxMetadata,
          editor.getEditorState().editor.projectContents,
          dataPathSuccess([]),
        )

        expect(trace).toEqual(
          dataTracingToLiteralAssignment(EP.fromString('sb/app:my-component'), dataPathSuccess([])),
        )
      }
      {
        const trace = traceDataFromVariableName(
          EP.fromString('sb/app:my-component'),
          'very',
          editor.getEditorState().editor.jsxMetadata,
          editor.getEditorState().editor.projectContents,
          dataPathSuccess([]),
        )

        expect(trace).toEqual(
          dataTracingToLiteralAssignment(
            EP.fromString('sb/app:my-component'),
            dataPathSuccess(['very']),
          ),
        )
      }
      {
        const trace = traceDataFromVariableName(
          EP.fromString('sb/app:my-component:component-root'),
          'valueFromTheDeep',
          editor.getEditorState().editor.jsxMetadata,
          editor.getEditorState().editor.projectContents,
          dataPathSuccess([]),
        )

        expect(trace).toEqual(
          dataTracingToLiteralAssignment(
            EP.fromString('sb/app:my-component'),
            dataPathSuccess(['very', 'deep', 'title', 'value']),
          ),
        )
      }
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
