import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import { selectComponentsForTest } from '../../../utils/utils.test-utils'
import {
  formatTestProjectCode,
  getPrintedUiJsCodeWithoutUIDs,
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

describe('The navigator render prop picker', () => {
  const PreferredChildComponents = [
    {
      component: 'FlexRow',
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
      variants: [
        {
          label: 'Basic FlexCol',
          imports: 'import { FlexCol } from "/src/utils"',
          code: '<FlexCol />',
        },
      ],
    },
    {
      component: 'Flex',
      variants: [
        {
          label: 'Flex',
          imports: 'import { Flex } from "/src/utils"',
          code: '<Flex />',
        },
      ],
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
    ['/utopia/components.utopia.js']: `import { Card } from './storyboard'

    const Components = {
      '/utopia/storyboard': {
        Card: {
          component: Card,
          supportsChildren: true,
          properties: {
            title: {
              control: 'jsx',
              preferredContents: ${JSON.stringify(PreferredChildComponents)},
            },
          },
          variants: [],
        },
      },
    }
    
    export default Components    
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
    `,
  })

  function variantsForComponent(componentName: string) {
    return [
      { label: '(empty)', code: `<${componentName} />` },
      ...(PreferredChildComponents.find((v) => v.component === componentName)?.variants ?? []),
    ]
  }

  it('Should be displayed with the correct prop when clicking a render prop slot in the navigator', async () => {
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

  it('Should include all registered preferred children for that prop', async () => {
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

  it('Filters the available options', async () => {
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

  it('Clears the filter when hitting esc', async () => {
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

  it('Closes when clicking the X', async () => {
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

  it('Does not close when clicking the empty space within the popup', async () => {
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

  it('Selecting a component from the full picker should insert that component into the render prop', async () => {
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

  it('Selecting a component with no variants from the simple picker should insert that component into the render prop', async () => {
    const editor = await renderTestEditorWithModel(TestProject, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/card')])
    const emptySlot = editor.renderedDOM.getByTestId(
      'toggle-render-prop-NavigatorItemTestId-slot_sb/card/prop_label_title',
    )
    await mouseClickAtPoint(emptySlot, { x: 2, y: 2 })

    const menuButton = await waitFor(() => editor.renderedDOM.getByText('Basic FlexCol'))
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

  it('Selecting a component from the simple picker in a submenu should insert that component into the render prop', async () => {
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
})
