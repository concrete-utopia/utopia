import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import { selectComponentsForTest, wait } from '../../../utils/utils.test-utils'
import type { EditorRenderResult } from '../../canvas/ui-jsx.test-utils'
import {
  formatTestProjectCode,
  getPrintedUiJsCode,
  getPrintedUiJsCodeWithoutUIDs,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  renderTestEditorWithModel,
  TestScenePath,
} from '../../canvas/ui-jsx.test-utils'
import {
  StoryboardFilePath,
  navigatorEntryToKey,
  regularNavigatorEntry,
  varSafeNavigatorEntryToKey,
} from '../../editor/store/editor-state'
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
import { fireEvent, waitFor } from '@testing-library/react'
import { labelTestIdForComponentIcon } from './component-picker-context-menu'
import { ReplaceElementButtonTestId, addChildButtonTestId } from './navigator-item-components'
import { NavigatorContainerId } from '../navigator'
import { act } from 'react-dom/test-utils'
import { cmdModifier } from '../../../utils/modifiers'

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
        <div style={props.style} data-uid='card-root'>
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
        <Card
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 800,
            top: 111,
            width: 139,
            height: 87,
          }}
          data-uid='card-with-title'
          title={<div data-uid='card-title-div'/>}
        />
        <div data-uid='empty-div'/>
        <div data-uid='non-empty-div'>
          <span>Something</span>
        </div>
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
          variants: [
            {
              label: 'with a column',
              imports: 'import { FlexRow, FlexCol } from "/src/other-utils"',
              code: '<FlexRow><FlexCol /></FlexRow>',
            },
          ]
        },
        FlexCol: {
          component: FlexCol,
          icon: 'component',
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
        <Card
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 800,
            top: 111,
            width: 139,
            height: 87,
          }}
          title={<div />}
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
      labelTestIdForComponentIcon('RandomComponent', '/src/utils', 'component'),
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
      labelTestIdForComponentIcon('RandomComponent', '/src/utils', 'component'),
    )
    expect(randomComponentRow).not.toBeNull()

    const listRow = editor.renderedDOM.queryByTestId(
      labelTestIdForComponentIcon('List', '', 'code'),
    )
    expect(listRow).not.toBeNull()
  })

  it('Replacing while maintaining children permits a component with children when the target has none', async () => {
    const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/empty-div')])
    const navigatorElement = editor.renderedDOM.getByTestId(
      `navigator-item-${varSafeNavigatorEntryToKey(
        regularNavigatorEntry(EP.fromString('sb/empty-div')),
      )}`,
    )
    await act(async () => {
      fireEvent(
        navigatorElement,
        new MouseEvent('contextmenu', {
          bubbles: true,
          cancelable: true,
          clientX: 3,
          clientY: 3,
          buttons: 0,
          button: 2,
        }),
      )
    })

    await editor.getDispatchFollowUpActionsFinished()

    const replaceThisMenuButton = await waitFor(() => editor.renderedDOM.getByText('Replace This…'))
    await mouseClickAtPoint(replaceThisMenuButton, { x: 3, y: 3 })

    await editor.getDispatchFollowUpActionsFinished()

    const flexRowButton = await editor.renderedDOM.findByTestId(
      'component-picker-item-/src/other-utils.js-with a column',
    )

    await mouseClickAtPoint(flexRowButton, { x: 3, y: 3 })

    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState(), StoryboardFilePath)).toEqual(
      formatTestProjectCode(`
    import * as React from 'react'
    import { Storyboard } from 'utopia-api'
    import { FlexRow, FlexCol } from '/src/other-utils'

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
        />
        <Card
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 800,
            top: 111,
            width: 139,
            height: 87,
          }}
          title={<div />}
        />
        <FlexRow>
          <FlexCol />
        </FlexRow>
        <div>
          <span>Something</span>
        </div>
      </Storyboard>
    )
    `),
    )
  })

  it('a component with children wont be included when trying to replace an element that also has children', async () => {
    const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/card')])
    const navigatorElement = editor.renderedDOM.getByTestId(
      `navigator-item-${varSafeNavigatorEntryToKey(
        regularNavigatorEntry(EP.fromString('sb/empty-div')),
      )}`,
    )
    await act(async () => {
      fireEvent(
        navigatorElement,
        new MouseEvent('contextmenu', {
          bubbles: true,
          cancelable: true,
          clientX: 3,
          clientY: 3,
          buttons: 0,
          button: 2,
        }),
      )
    })

    await editor.getDispatchFollowUpActionsFinished()

    const replaceThisMenuButton = await waitFor(() => editor.renderedDOM.getByText('Replace This…'))
    await mouseClickAtPoint(replaceThisMenuButton, { x: 3, y: 3 })

    await editor.getDispatchFollowUpActionsFinished()

    const flexRowButton = editor.renderedDOM.queryByTestId('/src/other-utils.js-FlexRow')

    expect(flexRowButton).toBeNull()
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
        <Card
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 800,
            top: 111,
            width: 139,
            height: 87,
          }}
          title={<div />}
        />
        <div/>
        <div>
          <span>Something</span>
        </div>
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
      'toggle-render-prop-NavigatorItemTestId-synthetic_sb/3d7/019_attribute',
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
        <Card
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 800,
            top: 111,
            width: 139,
            height: 87,
          }}
          title={<div />}
        />
        <div/>
        <div>
          <span>Something</span>
        </div>
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
        <Card
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 800,
            top: 111,
            width: 139,
            height: 87,
          }}
          title={<div />}
        />
        <div/>
        <div>
          <span>Something</span>
        </div>
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
        <Card
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 800,
            top: 111,
            width: 139,
            height: 87,
          }}
          title={<div />}
        />
        <div/>
        <div>
          <span>Something</span>
        </div>
      </Storyboard>
    )
    `),
    )
  })

  it('Selecting a List from the simple picker for adding a child should insert the code for the List', async () => {
    const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/card')])
    const addChildButton = editor.renderedDOM.getByTestId(
      addChildButtonTestId(EP.fromString('sb/card')),
    )
    await mouseClickAtPoint(addChildButton, { x: 2, y: 2 })

    const submenuButton = await waitFor(() => editor.renderedDOM.getByText('List'))
    await mouseClickAtPoint(submenuButton, { x: 2, y: 2 })

    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState(), StoryboardFilePath)).toEqual(
      formatTestProjectCode(`
    import * as React from 'react'
    import { Storyboard } from 'utopia-api'
    import { Placeholder } from 'utopia-api'

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
          {[1, 2, 3].map((listItem) => (
            <Placeholder />
          ))}
        </Card>
        <Card
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 800,
            top: 111,
            width: 139,
            height: 87,
          }}
          title={<div />}
        />
        <div/>
        <div>
          <span>Something</span>
        </div>
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
    expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Scene, Storyboard, FlexRow } from 'utopia-api'

export var storyboard = (
  <Storyboard>
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
    >
      <FlexRow>
        {
          // @utopia/uid=conditional
          true ? <Column /> : null
        }
      </FlexRow>
    </Scene>
  </Storyboard>
)
export const Column = () => (
  <div
    style={{ display: 'flex', flexDirection: 'column' }}
  />
)
`)
  })

  describe('Wrapping in a List', () => {
    const target = EP.fromString('sb/card-with-title')

    const expectedOutput = formatTestProjectCode(`
      import * as React from 'react'
      import { Storyboard } from 'utopia-api'
      import { Placeholder } from 'utopia-api'

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
          />
          {[1, 2, 3].map((listItem) => (
              <Card
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 800,
                  top: 111,
                  width: 139,
                  height: 87,
                }}
                title={<div />}
              />
            ))}
          <div/>
          <div>
            <span>Something</span>
          </div>
        </Storyboard>
      )
    `)

    it('Works when using the context menu', async () => {
      const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
      await selectComponentsForTest(editor, [target])
      await openContextMenuAndClick(editor, [{ text: 'Wrap in…' }, { text: 'List' }])
      expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState(), StoryboardFilePath)).toEqual(
        expectedOutput,
      )
    })
  })

  describe('wrap in div', () => {
    const entryPoints = {
      'choose div from dropdown': async (renderResult: EditorRenderResult): Promise<void> => {
        await openContextMenuAndClick(renderResult, [
          { text: 'Wrap in…' },
          { text: 'div', testId: 'Div-div' },
        ])
      },
      'cmd + enter': (): Promise<void> => pressKey('Enter', { modifiers: cmdModifier }),
    }

    Object.entries(entryPoints).forEach(([entrypoint, trigger]) => {
      describe(`${entrypoint}`, () => {
        it('wrap absolute elements', async () => {
          const renderResult = await renderTestEditorWithCode(
            makeTestProjectCodeWithSnippet(` <div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
            data-uid='root'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 20,
                top: 22,
                width: 48,
                height: 41,
              }}
              data-uid='one'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 82,
                top: 28,
                width: 38,
                height: 29,
              }}
              data-uid='two'
            />
          </div>`),
            'await-first-dom-report',
          )
          await selectComponentsForTest(renderResult, [
            EP.appendNewElementPath(TestScenePath, ['root', 'one']),
            EP.appendNewElementPath(TestScenePath, ['root', 'two']),
          ])
          await trigger(renderResult)

          expect(
            renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
          ).toEqual([
            'regular-utopia-storyboard-uid/scene-aaa',
            'regular-utopia-storyboard-uid/scene-aaa/app-entity',
            'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
            'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/wra',
            'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/wra/one',
            'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/wra/two',
          ])

          expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
            makeTestProjectCodeWithSnippet(` <div
          style={{
            height: '100%',
            width: '100%',
            contain: 'layout',
          }}
          data-uid='root'
        >
          <div
            style={{
              contain: 'layout',
              width: 100,
              height: 41,
              position: 'absolute',
              top: 22,
              left: 20,
            }}
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 48,
                height: 41,
              }}
              data-uid='one'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 62,
                top: 6,
                width: 38,
                height: 29,
              }}
              data-uid='two'
            />
          </div>
        </div>`),
          )
        })
        it('wrap flex elements', async () => {
          const renderResult = await renderTestEditorWithCode(
            makeTestProjectCodeWithSnippet(` <div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
            data-uid='root'
          >
            <div
              style={{
                contain: 'layout',
                width: 'max-content',
                height: 'max-content',
                position: 'absolute',
                top: 22,
                left: 20,
                display: 'flex',
                flexDirection: 'row',
                gap: 12,
                alignItems: 'center',
                justifyContent: 'flex-start',
              }}
              data-uid='container'
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  width: 48,
                  height: 41,
                  contain: 'layout',
                }}
                data-uid='one'
              />
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  width: 38,
                  height: 29,
                  contain: 'layout',
                }}
                data-uid='two'
              />
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  width: 38,
                  height: 29,
                  contain: 'layout',
                }}
                data-uid='three'
              />
            </div>
          </div>`),
            'await-first-dom-report',
          )
          await selectComponentsForTest(renderResult, [
            EP.appendNewElementPath(TestScenePath, ['root', 'container', 'one']),
            EP.appendNewElementPath(TestScenePath, ['root', 'container', 'two']),
          ])
          await trigger(renderResult)

          expect(
            renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
          ).toEqual([
            'regular-utopia-storyboard-uid/scene-aaa',
            'regular-utopia-storyboard-uid/scene-aaa/app-entity',
            'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
            'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container',
            'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/wra',
            'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/wra/one',
            'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/wra/two',
            'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/three',
          ])

          expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
            makeTestProjectCodeWithSnippet(`<div
          style={{
            height: '100%',
            width: '100%',
            contain: 'layout',
          }}
          data-uid='root'
        >
          <div
            style={{
              contain: 'layout',
              width: 'max-content',
              height: 'max-content',
              position: 'absolute',
              top: 22,
              left: 20,
              display: 'flex',
              flexDirection: 'row',
              gap: 12,
              alignItems: 'center',
              justifyContent: 'flex-start',
            }}
            data-uid='container'
          >
            <div
              style={{
                contain: 'layout',
                width: 98,
                height: 41,
              }}
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  width: 48,
                  height: 41,
                  contain: 'layout',
                  top: 0,
                  left: 0,
                  position: 'absolute',
                }}
                data-uid='one'
              />
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  width: 38,
                  height: 29,
                  contain: 'layout',
                  top: 6,
                  left: 60,
                  position: 'absolute',
                }}
                data-uid='two'
              />
            </div>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                width: 38,
                height: 29,
                contain: 'layout',
              }}
              data-uid='three'
            />
          </div>
        </div>`),
          )
        })
      })
    })
  })

  describe('Replacing a regular element', () => {
    const target = EP.fromString('sb/card-with-title')

    const expectedOutput = formatTestProjectCode(`
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
          />
          <FlexCol />
          <div/>
          <div>
            <span>Something</span>
          </div>
        </Storyboard>
      )
    `)

    it('Works when clicking the navigator button', async () => {
      const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
      await selectComponentsForTest(editor, [target])
      const replaceButton = editor.renderedDOM.getByTestId(ReplaceElementButtonTestId(target, null))
      await mouseClickAtPoint(replaceButton, { x: 2, y: 2 })

      const menuButton = await waitFor(() => editor.renderedDOM.getAllByText('FlexCol')[1]) // The first result is from other-utils
      await mouseClickAtPoint(menuButton, { x: 3, y: 3 })

      await editor.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState(), StoryboardFilePath)).toEqual(
        expectedOutput,
      )
    })

    it('Works when using the context menu', async () => {
      const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
      await selectComponentsForTest(editor, [target])

      const navigatorRow = editor.renderedDOM.getByTestId(NavigatorContainerId)

      await act(async () => {
        fireEvent(
          navigatorRow,
          new MouseEvent('contextmenu', {
            bubbles: true,
            cancelable: true,
            clientX: 3,
            clientY: 3,
            buttons: 0,
            button: 2,
          }),
        )
      })

      await editor.getDispatchFollowUpActionsFinished()

      const replaceButton = await waitFor(() => editor.renderedDOM.getByText('Replace Everything…'))
      await mouseClickAtPoint(replaceButton, { x: 3, y: 3 })

      const menuButton = await waitFor(() => editor.renderedDOM.getAllByText('FlexCol')[1])
      await mouseClickAtPoint(menuButton, { x: 3, y: 3 })

      await editor.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState(), StoryboardFilePath)).toEqual(
        expectedOutput,
      )
    })
  })

  describe('Replacing a render prop', () => {
    const target = EP.fromString('sb/card-with-title/card-title-div')

    const expectedOutput = formatTestProjectCode(`
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
          />
          <Card
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 800,
              top: 111,
              width: 139,
              height: 87,
            }}
            title={<FlexCol />}
          />
          <div/>
          <div>
            <span>Something</span>
          </div>
        </Storyboard>
      )
    `)

    it('Works when clicking the navigator button', async () => {
      const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
      await selectComponentsForTest(editor, [target])
      const replaceButton = editor.renderedDOM.getByTestId(
        ReplaceElementButtonTestId(target, 'title'),
      )
      await mouseClickAtPoint(replaceButton, { x: 2, y: 2 })

      const menuButton = await waitFor(() => editor.renderedDOM.getByText('FlexCol'))
      await mouseClickAtPoint(menuButton, { x: 3, y: 3 })

      await editor.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState(), StoryboardFilePath)).toEqual(
        expectedOutput,
      )
    })

    it('Works when using the context menu', async () => {
      const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
      await selectComponentsForTest(editor, [target])

      const navigatorRow = editor.renderedDOM.getByTestId(NavigatorContainerId)

      await act(async () => {
        fireEvent(
          navigatorRow,
          new MouseEvent('contextmenu', {
            bubbles: true,
            cancelable: true,
            clientX: 3,
            clientY: 3,
            buttons: 0,
            button: 2,
          }),
        )
      })

      await editor.getDispatchFollowUpActionsFinished()

      const replaceButton = await waitFor(() => editor.renderedDOM.getByText('Replace Everything…'))
      await mouseClickAtPoint(replaceButton, { x: 3, y: 3 })

      const menuButton = await waitFor(() => editor.renderedDOM.getByText('FlexCol'))
      await mouseClickAtPoint(menuButton, { x: 3, y: 3 })

      await editor.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState(), StoryboardFilePath)).toEqual(
        expectedOutput,
      )
    })

    it('Works when replacing the root element of a component', async () => {
      const editor = await renderTestEditorWithCode(
        TestProjectWithRootElement,
        'await-first-dom-report',
      )
      const targetPath = EP.fromString('sb/scene/pg:pg-root')
      await selectComponentsForTest(editor, [targetPath])

      const replaceButton = editor.renderedDOM.getByTestId(
        ReplaceElementButtonTestId(targetPath, null),
      )
      await mouseClickAtPoint(replaceButton, { x: 2, y: 2 })

      const menuButton = await waitFor(() => editor.renderedDOM.getByText('FlexCol'))
      await mouseClickAtPoint(menuButton, { x: 3, y: 3 })

      await editor.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState(), StoryboardFilePath))
        .toEqual(`import * as React from 'react'
import { Scene, Storyboard, Ellipse } from 'utopia-api'
import { FlexCol } from 'utopia-api'

const Playground = ({ children, style }) => (
  <FlexCol style={{ position: 'absolute' }} />
)

export var storyboard = (
  <Storyboard>
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
    >
      <Playground style={{}}>
        <Ellipse
          style={{
            backgroundColor: '#aaaaaa33',
            width: 100,
            height: 100,
          }}
        />
        <Ellipse
          style={{
            backgroundColor: '#aaaaaa33',
            width: 100,
            height: 100,
          }}
        />
        <Ellipse
          style={{
            backgroundColor: '#aaaaaa33',
            width: 100,
            height: 100,
          }}
        />
      </Playground>
    </Scene>
  </Storyboard>
)
`)
    })

    it('Works when swapping the root element of a component', async () => {
      const editor = await renderTestEditorWithCode(
        TestProjectWithRootElement,
        'await-first-dom-report',
      )
      const targetPath = EP.fromString('sb/scene/pg:pg-root')
      await selectComponentsForTest(editor, [targetPath])

      const navigatorRow = editor.renderedDOM.getByTestId(NavigatorContainerId)

      await act(async () => {
        fireEvent(
          navigatorRow,
          new MouseEvent('contextmenu', {
            bubbles: true,
            cancelable: true,
            clientX: 3,
            clientY: 3,
            buttons: 0,
            button: 2,
          }),
        )
      })

      await editor.getDispatchFollowUpActionsFinished()

      const replaceButton = await waitFor(() => editor.renderedDOM.getByText('Replace This…'))
      await mouseClickAtPoint(replaceButton, { x: 3, y: 3 })

      const flexRowButton = await waitFor(() => editor.renderedDOM.getByText('FlexRow'))
      await mouseClickAtPoint(flexRowButton, { x: 3, y: 3 })

      await editor.getDispatchFollowUpActionsFinished()

      // in the code below, FlexRow wraps {children}, preserving them in the rendered content
      expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState(), StoryboardFilePath))
        .toEqual(`import * as React from 'react'
import { Scene, Storyboard, Ellipse } from 'utopia-api'
import { FlexRow } from 'utopia-api'

const Playground = ({ children, style }) => (
  <FlexRow style={style}>{children}</FlexRow>
)

export var storyboard = (
  <Storyboard>
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
    >
      <Playground style={{}}>
        <Ellipse
          style={{
            backgroundColor: '#aaaaaa33',
            width: 100,
            height: 100,
          }}
        />
        <Ellipse
          style={{
            backgroundColor: '#aaaaaa33',
            width: 100,
            height: 100,
          }}
        />
        <Ellipse
          style={{
            backgroundColor: '#aaaaaa33',
            width: 100,
            height: 100,
          }}
        />
      </Playground>
    </Scene>
  </Storyboard>
)
`)
    })
  })
})

const TestProjectWithRootElement = `import * as React from 'react'
import {
  Scene,
  Storyboard,
  Ellipse,
} from 'utopia-api'

const Playground = ({ children, style }) => (
  <div data-uid='pg-root' style={style}>
    {children}
  </div>
)

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
      <Playground style={{}} data-uid='pg'>
        <Ellipse
          style={{
            backgroundColor: '#aaaaaa33',
            width: 100,
            height: 100,
          }}
          data-uid='aaa'
        />
        <Ellipse
          style={{
            backgroundColor: '#aaaaaa33',
            width: 100,
            height: 100,
          }}
          data-uid='aag'
        />
        <Ellipse
          style={{
            backgroundColor: '#aaaaaa33',
            width: 100,
            height: 100,
          }}
          data-uid='aai'
        />
      </Playground>
    </Scene>
  </Storyboard>
)
`

type Selector = {
  text: string
  testId?: string
}
async function openContextMenuAndClick(editor: EditorRenderResult, selectors: Selector[]) {
  const navigatorRow = editor.renderedDOM.getByTestId(NavigatorContainerId)

  await act(async () => {
    fireEvent(
      navigatorRow,
      new MouseEvent('contextmenu', {
        bubbles: true,
        cancelable: true,
        clientX: 3,
        clientY: 3,
        buttons: 0,
        button: 2,
      }),
    )
  })

  await editor.getDispatchFollowUpActionsFinished()

  for (const selector of selectors) {
    // we actually need the await here: https://eslint.org/docs/latest/rules/no-await-in-loop#when-not-to-use-it
    // eslint-disable-next-line no-await-in-loop
    const button = await waitFor(() =>
      selector.testId != null
        ? editor.renderedDOM.getByTestId(`component-picker-item-${selector.testId}`)
        : editor.renderedDOM.getByText(selector.text),
    )
    // eslint-disable-next-line no-await-in-loop
    await mouseClickAtPoint(button, { x: 3, y: 3 })
  }

  await editor.getDispatchFollowUpActionsFinished()
}
