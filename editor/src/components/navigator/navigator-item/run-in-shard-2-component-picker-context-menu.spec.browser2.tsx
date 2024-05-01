import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import { selectComponentsForTest, wait } from '../../../utils/utils.test-utils'
import {
  formatTestProjectCode,
  getPrintedUiJsCode,
  getPrintedUiJsCodeWithoutUIDs,
  renderTestEditorWithCode,
  renderTestEditorWithModel,
} from '../../canvas/ui-jsx.test-utils'
import { StoryboardFilePath, navigatorEntryToKey } from '../../editor/store/editor-state'
import * as EP from '../../../core/shared/element-path'
import {
  mouseClickAtPoint,
  mouseMoveToPoint,
  pressKey,
} from '../../canvas/event-helpers.test-utils'
import {
  componentPickerCloseButtonTestId,
  componentPickerFilterInputTestId,
  componentPickerOptionTestId,
  componentPickerTestIdForProp,
} from './component-picker'
import { waitFor } from '@testing-library/react'
import { labelTestIdForComponentIcon } from './component-picker-context-menu'
import { addChildButtonTestId } from './navigator-item-components'

describe('The navigator component picker context menu', () => {
  const PreferredChildComponents = [
    {
      component: 'FlexRow',
      moduleName: '/src/utils',
      variants: [
        {
          label: 'with three placeholders',
          imports: 'import { FlexRow } from "/src/utils"',
          code: '<FlexRow>three totally real placeholders</FlexRow>',
        },
        {
          label: 'Fully Loaded (limited time only!)',
          imports: 'import { FlexRow } from "/src/utils"',
          code: '<FlexRow>The Fully Loaded one</FlexRow>',
        },
        {
          label: 'With Auto-Sizing Content',
          imports: 'import { FlexRow } from "/src/utils"',
          code: '<FlexRow>Sizes automatically</FlexRow>',
        },
      ],
    },
    {
      component: 'FlexCol',
      moduleName: '/src/utils',
      variants: [],
    },
    {
      component: 'RandomComponent',
      moduleName: '/src/utils',
      variants: [],
    },
  ]

  const TestProject = createModifiedProject({
    [StoryboardFilePath]: `
    import * as React from 'react'
    import { Storyboard } from 'utopia-api'

    export const Card = (props) => {
      return (
        <div style={props.style}>
          {props.title}
          {props.children}
        </div>
      )
    }

    export var storyboard = (
      <Storyboard data-uid='sb'>
        <Card
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 945,
            top: 111,
            width: 139,
            height: 87,
          }}
          data-uid='card'
        />
      </Storyboard>
    )
    `,
    ['/utopia/other-components.utopia.js']: `import {
      Card,
      FlexRow,
      FlexCol,
      RandomComponent,
    } from '/src/other-utils'
    // These components have the wrong icons and preferredContents so that we can check we're getting the correct components in the lookup
    
    const Components = {
      '/src/other-utils': {
        FlexRow: {
          component: FlexRow,
          icon: 'column',
          properties: {},
        },
        FlexCol: {
          component: FlexCol,
          icon: 'regular',
          properties: {},
        },
        RandomComponent: {
          component: RandomComponent,
          icon: 'row',
          properties: {},
        },
        Card: {
          component: Card,
          properties: {
            title: {
              control: 'jsx',
              preferredContents: [],
            },
          },
          variants: [],
        },
      },
    }
    
    export default Components    
    `,
    ['/utopia/components.utopia.js']: `import { Card } from './storyboard'
    import {
      FlexRow,
      FlexCol,
      RandomComponent,
    } from '/src/utils'
    
    const Components = {
      '/src/utils': {
        FlexRow: {
          component: FlexRow,
          icon: 'row',
          properties: {},
        },
        FlexCol: {
          component: FlexCol,
          icon: 'column',
          properties: {},
        },
        RandomComponent: {
          component: RandomComponent,
          properties: {},
        },
      },
      '/utopia/storyboard': {
        Card: {
          component: Card,
          properties: {
            title: {
              control: 'jsx',
              preferredContents: ${JSON.stringify(PreferredChildComponents)},
            },
          },
          children: {
            preferredContents: ${JSON.stringify(PreferredChildComponents)},
          },
          variants: [],
        },
      },
    }
    
    export default Components    
    `,
    ['/src/other-utils.js']: `
    import * as React from 'react'

    export function FlexRow({ children, style, ...props }) {
      return (
        <div
          {...props}
          style={{
            position: 'relative',
            display: 'flex',
            flexDirection: 'row',
            ...style,
          }}
        >
          {children}
        </div>
      )
    }

    export function FlexCol({ children, style, ...props }) {
      return (
        <div
          {...props}
          style={{
            position: 'relative',
            display: 'flex',
            flexDirection: 'column',
            ...style,
          }}
        >
          {children}
        </div>
      )
    }

    export function RandomComponent() {
      return <div />
    }

    export function Card() {
      return <div />
    }
    `,
    ['/src/utils.js']: `
    import * as React from 'react'

    export function FlexRow({ children, style, ...props }) {
      return (
        <div
          {...props}
          style={{
            position: 'relative',
            display: 'flex',
            flexDirection: 'row',
            ...style,
          }}
        >
          {children}
        </div>
      )
    }

    export function FlexCol({ children, style, ...props }) {
      return (
        <div
          {...props}
          style={{
            position: 'relative',
            display: 'flex',
            flexDirection: 'column',
            ...style,
          }}
        >
          {children}
        </div>
      )
    }

    export function RandomComponent() {
      return <div />
    }
    `,
  })

  function variantsForComponent(componentName: string) {
    return [
      { label: '(empty)', code: `<${componentName} />` },
      ...(PreferredChildComponents.find((v) => v.component === componentName)?.variants ?? []),
    ]
  }

  xit('Should be displayed with the correct prop when clicking a render prop slot in the navigator', async () => {
    const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/card')])
    const emptySlot = editor.renderedDOM.getByTestId(
      'toggle-render-prop-NavigatorItemTestId-slot_sb/card/prop_label_title',
    )
    await mouseClickAtPoint(emptySlot, { x: 2, y: 2 })

    const moreButton = await waitFor(() => editor.renderedDOM.getByText('More...'))
    await mouseClickAtPoint(moreButton, { x: 3, y: 3 })

    const renderPropPicker = editor.renderedDOM.queryByTestId(componentPickerTestIdForProp('title'))
    expect(renderPropPicker).not.toBeNull()

    const propField = editor.renderedDOM.queryByTestId(
      `${componentPickerTestIdForProp('title')}-prop-field`,
    )
    expect(propField).not.toBeNull()
    expect(propField!.textContent).toEqual('Title')
  })

  xit('Should include all registered preferred children for that prop', async () => {
    const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/card')])
    const emptySlot = editor.renderedDOM.getByTestId(
      'toggle-render-prop-NavigatorItemTestId-slot_sb/card/prop_label_title',
    )
    await mouseClickAtPoint(emptySlot, { x: 2, y: 2 })

    const moreButton = await waitFor(() => editor.renderedDOM.getByText('More...'))
    await mouseClickAtPoint(moreButton, { x: 3, y: 3 })

    for (const childComponent of PreferredChildComponents) {
      const renderedOptionLabel = editor.renderedDOM.queryByTestId(
        componentPickerOptionTestId(childComponent.component),
      )
      expect(renderedOptionLabel).not.toBeNull()

      for (const variant of variantsForComponent(childComponent.component)) {
        const renderedOptionVariant = editor.renderedDOM.queryByTestId(
          componentPickerOptionTestId(childComponent.component, variant.label),
        )
        expect(renderedOptionVariant).not.toBeNull()
      }
    }
  })

  xit('Filters the available options', async () => {
    const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/card')])
    const emptySlot = editor.renderedDOM.getByTestId(
      'toggle-render-prop-NavigatorItemTestId-slot_sb/card/prop_label_title',
    )
    await mouseClickAtPoint(emptySlot, { x: 2, y: 2 })

    const moreButton = await waitFor(() => editor.renderedDOM.getByText('More...'))
    await mouseClickAtPoint(moreButton, { x: 3, y: 3 })

    // Select the search bar
    const filterInput = editor.renderedDOM.getByTestId(componentPickerFilterInputTestId)
    filterInput.focus()
    document.execCommand('insertText', false, 'flexr')
    // await pressKey('Enter', { targetElement: filterInput })

    // Check that it was filtered correctly
    for (const childComponent of PreferredChildComponents) {
      const renderedOptionLabel = editor.renderedDOM.queryByTestId(
        componentPickerOptionTestId(childComponent.component),
      )

      if (childComponent.component === 'FlexRow') {
        expect(renderedOptionLabel).not.toBeNull()
      } else {
        expect(renderedOptionLabel).toBeNull()
      }

      for (const variant of variantsForComponent(childComponent.component)) {
        const renderedOptionVariant = editor.renderedDOM.queryByTestId(
          componentPickerOptionTestId(childComponent.component, variant.label),
        )

        if (childComponent.component === 'FlexRow') {
          expect(renderedOptionVariant).not.toBeNull()
        } else {
          expect(renderedOptionVariant).toBeNull()
        }
      }
    }
  })

  xit('Clears the filter when hitting esc', async () => {
    const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/card')])
    const emptySlot = editor.renderedDOM.getByTestId(
      'toggle-render-prop-NavigatorItemTestId-slot_sb/card/prop_label_title',
    )
    await mouseClickAtPoint(emptySlot, { x: 2, y: 2 })

    const moreButton = await waitFor(() => editor.renderedDOM.getByText('More...'))
    await mouseClickAtPoint(moreButton, { x: 3, y: 3 })

    // Select the search bar
    const filterInput = editor.renderedDOM.getByTestId(componentPickerFilterInputTestId)
    filterInput.focus()
    document.execCommand('insertText', false, 'flexr')

    await pressKey('Escape', { targetElement: filterInput })

    for (const childComponent of PreferredChildComponents) {
      const renderedOptionLabel = editor.renderedDOM.queryByTestId(
        componentPickerOptionTestId(childComponent.component),
      )

      expect(renderedOptionLabel).not.toBeNull()

      for (const variant of variantsForComponent(childComponent.component)) {
        const renderedOptionVariant = editor.renderedDOM.queryByTestId(
          componentPickerOptionTestId(childComponent.component, variant.label),
        )

        expect(renderedOptionVariant).not.toBeNull()
      }
    }
  })

  xit('Closes when clicking the X', async () => {
    const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/card')])
    const emptySlot = editor.renderedDOM.getByTestId(
      'toggle-render-prop-NavigatorItemTestId-slot_sb/card/prop_label_title',
    )
    await mouseClickAtPoint(emptySlot, { x: 2, y: 2 })

    const moreButton = await waitFor(() => editor.renderedDOM.getByText('More...'))
    await mouseClickAtPoint(moreButton, { x: 3, y: 3 })

    const renderPropPickerBefore = editor.renderedDOM.queryByTestId(
      componentPickerTestIdForProp('title'),
    )
    expect(renderPropPickerBefore).not.toBeNull()

    const xButton = editor.renderedDOM.getByTestId(componentPickerCloseButtonTestId)
    await mouseClickAtPoint(xButton, { x: 2, y: 2 })

    const renderPropPickerAfter = editor.renderedDOM.queryByTestId(
      componentPickerTestIdForProp('title'),
    )
    expect(renderPropPickerAfter).toBeNull()
  })

  xit('Does not close when clicking the empty space within the popup', async () => {
    const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/card')])
    const emptySlot = editor.renderedDOM.getByTestId(
      'toggle-render-prop-NavigatorItemTestId-slot_sb/card/prop_label_title',
    )
    await mouseClickAtPoint(emptySlot, { x: 2, y: 2 })

    const moreButton = await waitFor(() => editor.renderedDOM.getByText('More...'))
    await mouseClickAtPoint(moreButton, { x: 3, y: 3 })

    const renderPropPickerBefore = editor.renderedDOM.queryByTestId(
      componentPickerTestIdForProp('title'),
    )
    expect(renderPropPickerBefore).not.toBeNull()

    await mouseClickAtPoint(renderPropPickerBefore!, { x: 2, y: 2 })

    const renderPropPickerAfter = editor.renderedDOM.queryByTestId(
      componentPickerTestIdForProp('title'),
    )
    expect(renderPropPickerAfter).not.toBeNull()
  })

  xit('Selecting a component from the full picker should insert that component into the render prop', async () => {
    const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/card')])
    const emptySlot = editor.renderedDOM.getByTestId(
      'toggle-render-prop-NavigatorItemTestId-slot_sb/card/prop_label_title',
    )
    await mouseClickAtPoint(emptySlot, { x: 2, y: 2 })

    const moreButton = await waitFor(() => editor.renderedDOM.getByText('More...'))
    await mouseClickAtPoint(moreButton, { x: 3, y: 3 })

    const optionLabel = 'FlexRow'
    const optionVariantName = 'with three placeholders'
    const renderedOptionVariant = editor.renderedDOM.getByTestId(
      componentPickerOptionTestId(optionLabel, optionVariantName),
    )
    await mouseClickAtPoint(renderedOptionVariant, { x: 2, y: 2 })
    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState(), StoryboardFilePath)).toEqual(
      formatTestProjectCode(`
    import * as React from 'react'
    import { Storyboard } from 'utopia-api'
    import { FlexRow } from '/src/utils'

    export const Card = (props) => {
      return (
        <div style={props.style}>
          {props.title}
          {props.children}
        </div>
      )
    }

    export var storyboard = (
      <Storyboard>
        <Card
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 945,
            top: 111,
            width: 139,
            height: 87,
          }}
          title={
            <FlexRow>three totally real placeholders</FlexRow>
          }
        />
      </Storyboard>
    )
    `),
    )
  })

  it('simple picker returns the correct registered components for render props', async () => {
    const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/card')])
    const emptySlot = editor.renderedDOM.getByTestId(
      'toggle-render-prop-NavigatorItemTestId-slot_sb/card/prop_label_title',
    )
    await mouseClickAtPoint(emptySlot, { x: 2, y: 2 })

    const flexRowRow = editor.renderedDOM.queryByTestId(
      labelTestIdForComponentIcon('FlexRow', '/src/utils', 'row'),
    )
    expect(flexRowRow).not.toBeNull()

    const flexColRow = editor.renderedDOM.queryByTestId(
      labelTestIdForComponentIcon('FlexCol', '/src/utils', 'column'),
    )
    expect(flexColRow).not.toBeNull()

    const randomComponentRow = editor.renderedDOM.queryByTestId(
      labelTestIdForComponentIcon('RandomComponent', '/src/utils', 'regular'),
    )
    expect(randomComponentRow).not.toBeNull()
  })

  it('simple picker returns the correct registered components for children', async () => {
    const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/card')])
    const addChildButton = editor.renderedDOM.getByTestId(
      addChildButtonTestId(EP.fromString('sb/card')),
    )
    await mouseClickAtPoint(addChildButton, { x: 2, y: 2 })

    const flexRowRow = editor.renderedDOM.queryByTestId(
      labelTestIdForComponentIcon('FlexRow', '/src/utils', 'row'),
    )
    expect(flexRowRow).not.toBeNull()

    const flexColRow = editor.renderedDOM.queryByTestId(
      labelTestIdForComponentIcon('FlexCol', '/src/utils', 'column'),
    )
    expect(flexColRow).not.toBeNull()

    const randomComponentRow = editor.renderedDOM.queryByTestId(
      labelTestIdForComponentIcon('RandomComponent', '/src/utils', 'regular'),
    )
    expect(randomComponentRow).not.toBeNull()
  })

  it('Selecting a component with no variants from the simple picker for a render prop should insert that component into the render prop', async () => {
    const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/card')])
    const emptySlot = editor.renderedDOM.getByTestId(
      'toggle-render-prop-NavigatorItemTestId-slot_sb/card/prop_label_title',
    )
    await mouseClickAtPoint(emptySlot, { x: 2, y: 2 })

    const menuButton = await waitFor(() => editor.renderedDOM.getByText('FlexCol'))
    await mouseClickAtPoint(menuButton, { x: 3, y: 3 })

    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState(), StoryboardFilePath)).toEqual(
      formatTestProjectCode(`
    import * as React from 'react'
    import { Storyboard } from 'utopia-api'
    import { FlexCol } from '/src/utils'

    export const Card = (props) => {
      return (
        <div style={props.style}>
          {props.title}
          {props.children}
        </div>
      )
    }

    export var storyboard = (
      <Storyboard>
        <Card
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 945,
            top: 111,
            width: 139,
            height: 87,
          }}
          title={
            <FlexCol />
          }
        />
      </Storyboard>
    )
    `),
    )
  })

  it('Selecting a component with no variants from the simple picker for a conditional slot should insert that component into the render prop', async () => {
    const editor = await renderTestEditorWithCode(
      `import * as React from 'react'
      import { Storyboard } from 'utopia-api'
      
      export var storyboard = (
        <Storyboard data-uid='sb'>
          {true ? null : null}
        </Storyboard>
      )
      
      export const Column = () => (
        <div
          style={{ display: 'flex', flexDirection: 'column' }}
        ></div>
      )
    `,
      'await-first-dom-report',
    )
    const emptySlot = editor.renderedDOM.getByTestId(
      'toggle-render-prop-NavigatorItemTestId-synthetic_sb/5af/129_attribute',
    )
    await mouseClickAtPoint(emptySlot, { x: 2, y: 2 })

    const menuButton = await waitFor(() => editor.renderedDOM.getByText('Column'))
    await mouseClickAtPoint(menuButton, { x: 3, y: 3 })

    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState(), StoryboardFilePath)).toEqual(
      formatTestProjectCode(`
      import * as React from 'react'
      import { Storyboard } from 'utopia-api'
  
      export var storyboard = (
       <Storyboard>
          {
            // @utopia/conditional=false
            true ? null : <Column />
          }
        </Storyboard>
      )
    
      export const Column = () => (
        <div
          style={{ display: 'flex', flexDirection: 'column' }}
        />
      )
    `),
    )
  })

  it('Selecting a component with no variants from the simple picker for adding a child should insert that component into the render prop', async () => {
    const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/card')])
    const addChildButton = editor.renderedDOM.getByTestId(
      addChildButtonTestId(EP.fromString('sb/card')),
    )
    await mouseClickAtPoint(addChildButton, { x: 2, y: 2 })

    const menuButton = await waitFor(() => editor.renderedDOM.getByText('FlexCol'))
    await mouseClickAtPoint(menuButton, { x: 3, y: 3 })

    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState(), StoryboardFilePath)).toEqual(
      formatTestProjectCode(`
    import * as React from 'react'
    import { Storyboard } from 'utopia-api'
    import { FlexCol } from '/src/utils'

    export const Card = (props) => {
      return (
        <div style={props.style}>
          {props.title}
          {props.children}
        </div>
      )
    }

    export var storyboard = (
      <Storyboard>
        <Card
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 945,
            top: 111,
            width: 139,
            height: 87,
          }}
        >
          <FlexCol />
        </Card>
      </Storyboard>
    )
    `),
    )
  })

  it('Selecting a component from the simple picker in a submenu for a render prop should insert that component into the render prop', async () => {
    const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/card')])
    const emptySlot = editor.renderedDOM.getByTestId(
      'toggle-render-prop-NavigatorItemTestId-slot_sb/card/prop_label_title',
    )
    await mouseClickAtPoint(emptySlot, { x: 2, y: 2 })

    const submenuButton = await waitFor(() => editor.renderedDOM.getByText('FlexRow'))
    await mouseMoveToPoint(submenuButton, { x: 3, y: 3 })

    const renderedOptionVariant = await waitFor(() =>
      editor.renderedDOM.getByText('with three placeholders'),
    )
    await mouseClickAtPoint(renderedOptionVariant, { x: 2, y: 2 })
    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState(), StoryboardFilePath)).toEqual(
      formatTestProjectCode(`
    import * as React from 'react'
    import { Storyboard } from 'utopia-api'
    import { FlexRow } from '/src/utils'

    export const Card = (props) => {
      return (
        <div style={props.style}>
          {props.title}
          {props.children}
        </div>
      )
    }

    export var storyboard = (
      <Storyboard>
        <Card
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 945,
            top: 111,
            width: 139,
            height: 87,
          }}
          title={
            <FlexRow>three totally real placeholders</FlexRow>
          }
        />
      </Storyboard>
    )
    `),
    )
  })

  it('Selecting a component from the simple picker in a submenu for adding a child should insert that component into the render prop', async () => {
    const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/card')])
    const addChildButton = editor.renderedDOM.getByTestId(
      addChildButtonTestId(EP.fromString('sb/card')),
    )
    await mouseClickAtPoint(addChildButton, { x: 2, y: 2 })

    const submenuButton = await waitFor(() => editor.renderedDOM.getByText('FlexRow'))
    await mouseMoveToPoint(submenuButton, { x: 3, y: 3 })

    const renderedOptionVariant = await waitFor(() =>
      editor.renderedDOM.getByText('with three placeholders'),
    )
    await mouseClickAtPoint(renderedOptionVariant, { x: 2, y: 2 })
    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState(), StoryboardFilePath)).toEqual(
      formatTestProjectCode(`
    import * as React from 'react'
    import { Storyboard } from 'utopia-api'
    import { FlexRow } from '/src/utils'

    export const Card = (props) => {
      return (
        <div style={props.style}>
          {props.title}
          {props.children}
        </div>
      )
    }

    export var storyboard = (
      <Storyboard>
        <Card
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 945,
            top: 111,
            width: 139,
            height: 87,
          }}
        >
          <FlexRow>three totally real placeholders</FlexRow>
        </Card>
      </Storyboard>
    )
    `),
    )
  })

  it('deduplicates render prop UIDs', async () => {
    const editor = await renderTestEditorWithModel(
      createModifiedProject({
        [StoryboardFilePath]: `
        import * as React from 'react'
        import * as Utopia from 'utopia-api'
        import { Storyboard, Scene } from 'utopia-api'

        export function Flex({ hello }) {
          return <div>{hello.greeting}</div>
        }
        
        export function Card({ header, children }) {
          return (
            <div data-uid='root'>
              <h2 data-uid='082'>{header}</h2>
              {children}
            </div>
          )
        }
        
        var Playground = ({ style }) => {
          return (
            <div style={style} data-uid='pg-root'>
              <Card
                data-uid='card'
              >
                <p data-uid='contents'>Card contents</p>
              </Card>
            </div>
          )
        }
        
        export var storyboard = (
          <Storyboard data-uid='sb'>
            <Scene
              style={{
                width: 521,
                height: 266,
                position: 'absolute',
                left: 554,
                top: 247,
                backgroundColor: 'white',
              }}
              data-uid='scene'
              data-testid='scene'
              commentId='120'
            >
              <Playground
                style={{
                  width: 454,
                  height: 177,
                  position: 'absolute',
                  left: 34,
                  top: 44,
                  backgroundColor: 'white',
                  display: 'flex',
                  alignItems: 'center',
                  justifyContent: 'center',
                }}
                className='playground'
                css={{ color: 'red' }}
                data-uid='pg'
              />
            </Scene>
          </Storyboard>
        )        
      `,
        ['/utopia/components.utopia.js']: `import { Card } from './storyboard'

        const Components = {
          '/utopia/storyboard': {
            Card: {
              component: Card,
              properties: {
                title: {
                  control: 'jsx',
                  preferredContents: [
                    {
                      component: 'Flex',
                      variants: [
                        {
                          label: 'Flex Hello',
                          imports:
                            'import { Flex } from "/utopia/storyboard"',
                          code: '<Flex hello={{ greeting: "there" }} />',
                        },
                      ],
                    },
                  ],
                },
              },
              variants: [],
            },
          },
        }
        
        export default Components         
      `,
      }),
      'await-first-dom-report',
    )

    const emptySlot = editor.renderedDOM.getByTestId(
      'toggle-render-prop-NavigatorItemTestId-slot_sb/scene/pg:pg_root/card/prop_label_title',
    )
    await mouseClickAtPoint(emptySlot, { x: 2, y: 2 })
    await editor.getDispatchFollowUpActionsFinished()

    const menuButton = await waitFor(() => editor.renderedDOM.getByText('Flex Hello'))
    await mouseClickAtPoint(menuButton, { x: 3, y: 3 })

    expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
      'regular-sb/scene',
      'regular-sb/scene/pg',
      'regular-sb/scene/pg:pg-root',
      'regular-sb/scene/pg:pg-root/card',
      'render-prop-sb/scene/pg:pg-root/card/prop-label-title-title',
      'synthetic-sb/scene/pg:pg-root/card/pro-element-pro',
      'render-prop-sb/scene/pg:pg-root/card/prop-label-children-children',
      'regular-sb/scene/pg:pg-root/card/contents',
    ])
    expect(
      editor.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual([
      'regular-sb/scene',
      'regular-sb/scene/pg',
      'regular-sb/scene/pg:pg-root',
      'regular-sb/scene/pg:pg-root/card',
      'render-prop-sb/scene/pg:pg-root/card/prop-label-title-title',
      'synthetic-sb/scene/pg:pg-root/card/pro-element-pro',
      'render-prop-sb/scene/pg:pg-root/card/prop-label-children-children',
      'regular-sb/scene/pg:pg-root/card/contents',
    ])
  })

  it('can replace element inside map', async () => {
    const editor = await renderTestEditorWithCode(
      `import * as React from 'react'
    import { Scene, Storyboard, FlexRow } from 'utopia-api'
    
    export var storyboard = (
      <Storyboard data-uid='sb'>
        <Scene
          id='playground-scene'
          commentId='playground-scene'
          style={{
            width: 700,
            height: 759,
            position: 'absolute',
            left: 212,
            top: 128,
          }}
          data-label='Playground'
          data-uid='scene'
        >
          <FlexRow data-uid='flexrow'>
            {
              // @utopia/uid=map
              [1, 1].map(() => (
              <img
                style={{
                  width: 220,
                  height: 220,
                  display: 'inline-block',
                }}
                src='/editor/utopia-logo-white-fill.png?hash=4c7df4ba0e8686df4fe14e3570f2d3002304b930'
                data-uid='img'
              />
              ))
            }
          </FlexRow>
        </Scene>
      </Storyboard>
    )
`,
      'await-first-dom-report',
    )

    await mouseClickAtPoint(
      editor.renderedDOM.getByTestId('NavigatorItemTestId-regular_sb/scene/flexrow/map/img~~~1'),
      { x: 2, y: 2 },
    )

    await mouseClickAtPoint(
      editor.renderedDOM.getByTestId('replace-element-button-sb/scene/flexrow/map/img~~~1'),
      { x: 2, y: 2 },
    )

    await mouseClickAtPoint(editor.renderedDOM.getByText('div'), { x: 2, y: 2 })

    // the element inside the map has been changed to a `div`
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Scene, Storyboard, FlexRow } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      id='playground-scene'
      commentId='playground-scene'
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <FlexRow data-uid='flexrow'>
        {
          // @utopia/uid=map
          [1, 1].map(() => (
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
              }}
              data-uid='pro'
            />
          ))
        }
      </FlexRow>
    </Scene>
  </Storyboard>
)
`)
  })

  it('can replace element inside conditional', async () => {
    const editor = await renderTestEditorWithCode(
      `import * as React from 'react'
      import { Scene, Storyboard, FlexRow } from 'utopia-api'
      
      export var storyboard = (
        <Storyboard data-uid='sb'>
          <Scene
            id='playground-scene'
            commentId='playground-scene'
            style={{
              width: 700,
              height: 759,
              position: 'absolute',
              left: 212,
              top: 128,
            }}
            data-label='Playground'
            data-uid='scene'
          >
            <FlexRow data-uid='flexrow'>
              {
                // @utopia/uid=conditional
                true ? (
                  <img
                    style={{
                      width: 220,
                      height: 220,
                      display: 'inline-block',
                    }}
                    src='/editor/utopia-logo-white-fill.png?hash=4c7df4ba0e8686df4fe14e3570f2d3002304b930'
                    data-uid='img'
                  />
                ) : null
              }
            </FlexRow>
          </Scene>
        </Storyboard>
      )
      export const Column = () => (
        <div
          style={{ display: 'flex', flexDirection: 'column' }}
        ></div>
      )      
`,
      'await-first-dom-report',
    )

    await mouseClickAtPoint(
      editor.renderedDOM.getByTestId(
        'NavigatorItemTestId-regular_sb/scene/flexrow/conditional/img-label',
      ),
      { x: 2, y: 2 },
    )

    await mouseClickAtPoint(
      editor.renderedDOM.getByTestId('replace-element-button-sb/scene/flexrow/conditional/img'),
      { x: 2, y: 2 },
    )

    await mouseClickAtPoint(editor.renderedDOM.getByText('Column'), { x: 2, y: 2 })

    // the element inside the map has been changed to a `div`
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Scene, Storyboard, FlexRow } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      id='playground-scene'
      commentId='playground-scene'
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <FlexRow data-uid='flexrow'>
        {
          // @utopia/uid=conditional
          true ? <Column data-uid='new' /> : null
        }
      </FlexRow>
    </Scene>
  </Storyboard>
)
export const Column = () => (
  <div
    style={{ display: 'flex', flexDirection: 'column' }}
    data-uid='45a'
  />
)
`)
  })
})
