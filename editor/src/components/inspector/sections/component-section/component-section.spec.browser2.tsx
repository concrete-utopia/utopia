import { within } from '@testing-library/react'
import * as EP from '../../../../core/shared/element-path'
import { selectComponentsForTest } from '../../../../utils/utils.test-utils'
import { pressKey } from '../../../canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../../../canvas/ui-jsx.test-utils'
import { renderTestEditorWithModel } from '../../../canvas/ui-jsx.test-utils'
import { createModifiedProject } from '../../../../sample-projects/sample-project-utils.test-utils'
import { StoryboardFilePath } from '../../../editor/store/editor-state'
import { applyPrettier } from 'utopia-vscode-common'

// comment out tests temporarily because it causes a dom-walker test to fail
// describe('Image preview for string control', () => {
//   it('shows image preview for urls with image extension', async () => {
//     const editor = await renderTestEditorWithModel(
//       projectWithImage(
//         'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAIAAAACCAYAAABytg0kAAAAEklEQVQIW2P8z8AARAwMjDAGACwBA/+8RVWvAAAAAElFTkSuQmCC',
//       ),
//       'await-first-dom-report',
//     )
//     await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/image')])

//     expect(editor.renderedDOM.queryAllByTestId(ImagePreviewTestId)).toHaveLength(1)
//   })
//   it('does not show image preview for urls without image extension', async () => {
//     const editor = await renderTestEditorWithModel(
//       projectWithImage('https://i.pinimg.com/474x/4d/79/99/4d7999a51a1a397189a6f98168bcde45'),
//       'await-first-dom-report',
//     )
//     await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/image')])

//     expect(editor.renderedDOM.queryAllByTestId(ImagePreviewTestId)).toHaveLength(0)
//   })
//   it('does not show image preview for non-urls', async () => {
//     const editor = await renderTestEditorWithModel(
//       projectWithImage('hello'),
//       'await-first-dom-report',
//     )
//     await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/image')])

//     expect(editor.renderedDOM.queryAllByTestId(ImagePreviewTestId)).toHaveLength(0)
//   })
// })

describe('Controls from registering components', () => {
  it('registering internal component', async () => {
    const editor = await renderTestEditorWithModel(
      registerComponentProject,
      'await-first-dom-report',
    )
    await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/title')])

    const dataPickerOpenerButton = editor.renderedDOM.getByTestId(
      `text-string-input-property-control`,
    )
    dataPickerOpenerButton.focus()
    document.execCommand('insertText', false, 'New title')
    await pressKey('Enter', { targetElement: dataPickerOpenerButton })

    const theScene = editor.renderedDOM.getByTestId('scene')
    expect(within(theScene).queryByText('New title')).not.toBeNull()
  })

  it('registering internal component with html prop shows preview', async () => {
    const editor = await renderTestEditorWithModel(
      registerComponentProjectWithHtmlProp,
      'await-first-dom-report',
    )
    await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/title')])

    const theInspector = editor.renderedDOM.getByTestId('inspector-sections-container')
    expect(within(theInspector).queryByText('Hello Utopia')).not.toBeNull()
  })

  it('registering external component', async () => {
    const editor = await renderTestEditorWithModel(
      registerThirdPartyComponentProject,
      'await-first-dom-report',
    )
    await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/title')])

    const dataPickerOpenerButton = editor.renderedDOM.getByTestId(
      `sampleprop-string-input-property-control`,
    )
    dataPickerOpenerButton.focus()
    document.execCommand('insertText', false, 'New props value')
    await pressKey('Enter', { targetElement: dataPickerOpenerButton })

    const theView = editor.renderedDOM.getByTestId('view')
    expect(theView.outerHTML).toContain('sampleprop="New props value"')
  })

  describe('preferred child components', () => {
    it('preferred child components with internal component', async () => {
      const editor = await renderTestEditorWithModel(
        DataPickerProjectShell(
          `export function Link({ href, children }) {
        return (
          <a href={href} data-uid='root'>
            {children}
          </a>
        )
      }
      
      var Playground = ({ style }) => {
        return (
          <div style={style} data-uid='dbc'>
            <Link href='/new' data-uid='78c'>
              nowhere
            </Link>
          </div>
        )
      }`,
          `{
        '/utopia/storyboard': {
          Link: {
            component: Link,
            properties: {
              children: {
                control: 'array',
                propertyControl: { control: 'jsx' },
              },
            },
            children: {
              preferredContents: [
                {
                  component: 'Link',
                  variants: { label: 'span', code: '<span>Link</span>' }
                },
              ]
            },
            variants: [],
          },
        },
      }`,
          `import { Link } from './storyboard'`,
        ),
        'await-first-dom-report',
      )

      // elementToInsert is omitted from the object below because it's a function
      expect(
        editor.getEditorState().editor.propertyControlsInfo['/utopia/storyboard'],
      ).toMatchObject({
        Link: {
          preferredChildComponents: [
            {
              name: 'Link',
              variants: [
                {
                  importsToAdd: {},
                  insertMenuLabel: 'span',
                },
              ],
            },
          ],
          properties: {
            children: {
              control: 'array',
              propertyControl: {
                control: 'jsx',
              },
            },
          },
          variants: [
            {
              importsToAdd: {
                '/utopia/storyboard': {
                  importedAs: null,
                  importedFromWithin: [
                    {
                      alias: 'Link',
                      name: 'Link',
                    },
                  ],
                  importedWithName: null,
                },
              },
              insertMenuLabel: 'Link',
            },
          ],
        },
      })
    })

    it('preferred child components with render prop', async () => {
      const editor = await renderTestEditorWithModel(
        DataPickerProjectShell(
          `export function Card({ header, children }) {
        return (
          <div data-uid='root'>
            <h2>{header}</h2>
            {children}
          </div>
        )
      }
      
      var Playground = ({ style }) => {
        return (
          <div style={style} data-uid='dbc'>
            <Card header={<span>Title</span>} data-uid='78c'>
              <p>Card contents</p>
            </Card>
          </div>
        )
      }`,
          `{
        '/utopia/storyboard': {
          Card: {
            component: Card,
            properties: {
              header: {
                control: 'array',
                propertyControl: {
                  control: 'jsx',
                  preferredContents: [
                    {
                      component: 'span',
                      variants: { label: 'span', code: '<span>Title</span>' }
                    },
                  ],
                },
              },
            },
            children: 'supported',
            variants: [],
          },
        },
      }`,
          `import { Card } from './storyboard';`,
        ),
        'await-first-dom-report',
      )

      // elementToInsert is omitted from the object below because it's a function
      expect(
        editor.getEditorState().editor.propertyControlsInfo['/utopia/storyboard'],
      ).toMatchObject({
        Card: {
          preferredChildComponents: [],
          properties: {
            header: {
              control: 'array',
              propertyControl: {
                control: 'jsx',
                preferredChildComponents: [
                  {
                    name: 'span',
                    variants: [
                      {
                        importsToAdd: {},
                        insertMenuLabel: 'span',
                      },
                    ],
                  },
                ],
              },
            },
          },
          variants: [
            {
              importsToAdd: {
                '/utopia/storyboard': {
                  importedAs: null,
                  importedFromWithin: [
                    {
                      alias: 'Card',
                      name: 'Card',
                    },
                  ],
                  importedWithName: null,
                },
              },
              insertMenuLabel: 'Card',
            },
          ],
        },
      })
    })
  })
})

// describe('Delete cartouche handling', () => {
//   async function getEditorWithPropertyExtras(
//     propertyExtras: string,
//     textField: string,
//   ): Promise<EditorRenderResult> {
//     const editor = await renderTestEditorWithModel(
//       registerComponentProjectWithCartouche(propertyExtras, textField),
//       'await-first-dom-report',
//     )
//     await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/title')])
//     return editor
//   }
//   it('optional field', async () => {
//     const editor = await getEditorWithPropertyExtras(``, `text={textForTitle}`)
//     const deleteCartoucheButton = editor.renderedDOM.getByTestId(`delete-cartouche-text`)
//     await mouseClickAtPoint(deleteCartoucheButton, { x: 2, y: 2 })
//     expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
//       registerProjectWithCartoucheStoryboard(``, ``),
//     )
//   })
//   it('required field without default value', async () => {
//     const editor = await getEditorWithPropertyExtras(`required: true`, `text={textForTitle}`)
//     const deleteCartoucheButton = editor.renderedDOM.queryByTestId(`delete-cartouche-text`)
//     expect(deleteCartoucheButton).toBeNull()
//     expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
//       registerComponentProjectWithCartoucheStoryboard(
//         `required: true`,
//         `text={textForTitle}`,
//       ),
//     )
//   })
//   it('required field with default value', async () => {
//     const editor = await getEditorWithPropertyExtras(
//       `required: true, defaultValue: 'Placeholder!'`,
//       `text={textForTitle}`,
//     )
//     const deleteCartoucheButton = editor.renderedDOM.getByTestId(`delete-cartouche-text`)
//     await mouseClickAtPoint(deleteCartoucheButton, { x: 2, y: 2 })
//     expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
//       registerComponentProjectWithCartoucheStoryboard(
//         `required: true, defaultValue: 'Placeholder!'`,
//         `text='Placeholder!'`,
//       ),
//     )
//   })
// })

function registerComponentProjectWithCartoucheStoryboard(
  propertyExtras: string,
  textField: string,
): string {
  return applyPrettier(
    `import * as React from 'react'
import {
Storyboard,
Scene,
} from 'utopia-api'

function Title({ text }) {
return <h2 data-uid='0cd'>{text}</h2>
}

var Playground = ({ style }) => {
const textForTitle = 'Hello Utopia'
return (
  <div style={style} data-uid='root'>
    <Title ${textField} data-uid='title' />
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
      title='Hello Utopia'
      data-uid='pg'
    />
  </Scene>
</Storyboard>
)
`,
    false,
  ).formatted
}

function registerComponentProjectWithCartouche(propertyExtras: string, textField: string) {
  return createModifiedProject({
    [StoryboardFilePath]: registerComponentProjectWithCartoucheStoryboard(
      propertyExtras,
      textField,
    ),
    ['/utopia/components.utopia.js']: `const Components = {
    '/utopia/storyboard': {
      Title: {
        properties: {
          text: {
            control: 'string-input',${propertyExtras}
          },
        },
        variants: [
          {
            code: '<Title />',
          },
        ],
      },
    },
  }
  
  export default Components  
  `,
  })
}

const registerComponentProject = createModifiedProject({
  [StoryboardFilePath]: `import * as React from 'react'
import {
  Storyboard,
  Scene,
} from 'utopia-api'

export function Title({ text }) {
  return <h2 data-uid='0cd'>{text}</h2>
}

var Playground = ({ style }) => {
  return (
    <div style={style} data-uid='root'>
      <Title text='Hello Utopia' data-uid='title' />
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
        title='Hello Utopia'
        data-uid='pg'
      />
    </Scene>
  </Storyboard>
)
`,
  ['/utopia/components.utopia.js']: `import { Title } from './storyboard'

  const Components = {
  '/utopia/storyboard': {
    Title: {
      component: Title,
      properties: {
        text: {
          control: 'string-input',
        },
      },
      variants: [
        {
          label: 'Title',
          imports: 'import { Title } from "/utopia/storyboard"',
          code: '<Title />',
        },
      ],
    },
  },
}

export default Components  
`,
})

const registerThirdPartyComponentProject = createModifiedProject({
  [StoryboardFilePath]: `import * as React from 'react'
import {
  Storyboard,
  Scene,
  View,
} from 'utopia-api'

var Playground = ({ style }) => {
  return (
    <div style={style} data-uid='root' data-testid='view'>
      <View sampleprop='Hello Utopia' data-uid='title'>
        Hello Utopia
      </View>
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
        title='Hello Utopia'
        data-uid='pg'
      />
    </Scene>
  </Storyboard>
)
`,
  ['/utopia/components.utopia.js']: `import { View } from 'utopia-api'
  
  const Components = {
    'utopia-api': {
      View: {
        component: View,
        properties: {
          sampleprop: {
            control: 'string-input',
            required: true,
            defaultValue: 'Sample',
          },
        },
        variants: [
          {
            label: 'View',
            imports: 'import { View } from "utopia-api"',
            code: '<View />',
          },
        ],
      },
    },
  }
  
  export default Components
  
`,
})

function DataPickerProjectShell(contents: string, componentDescriptor?: string, imports?: string) {
  const projectContents = {
    [StoryboardFilePath]: `import * as React from 'react'
    import * as Utopia from 'utopia-api'
    import {
      Storyboard,
      Scene,
    } from 'utopia-api'
    
    ${contents}
    
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
  }

  if (componentDescriptor != null) {
    projectContents['/utopia/components.utopia.js'] = `${imports}
      
    const Components = ${componentDescriptor}
    
    export default Components`
  }
  return createModifiedProject(projectContents)
}

const registerComponentProjectWithHtmlProp = createModifiedProject({
  [StoryboardFilePath]: `import * as React from 'react'
  import {
    Storyboard,
    Scene,
  } from 'utopia-api'
  
  export function Title({ text }) {
    return <h2 data-uid='0cd'>{text}</h2>
  }
  
  var Playground = ({ style }) => {
    return (
      <div style={style} data-uid='root'>
        <Title text='<p>Hello Utopia</p>' data-uid='title' />
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
          title='Hello Utopia'
          data-uid='pg'
        />
      </Scene>
    </Storyboard>
  )
  `,
  ['/utopia/components.utopia.js']: `import { Title } from './storyboard'

  const Components = {
    '/utopia/storyboard': {
      Title: {
        component: Title,
        properties: {
          text: {
            control: 'html-input',
          },
        },
        variants: [
          {
            label: 'Title',
            imports: 'import { Title } from "/utopia/storyboard"',
            code: '<Title />',
          },
        ],
      },
    },
  }
  
  export default Components  
`,
})

const projectWithImage = (imageUrl: string) =>
  createModifiedProject({
    [StoryboardFilePath]: `import * as React from 'react'
import {
  Storyboard,
  Scene,
} from 'utopia-api'

function Image({ url }) {
  return <img src={url} />
}

var Playground = ({ style }) => {
  return (
    <div style={style} data-uid='root'>
      <Image url='${imageUrl}' data-uid='image' />
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
        title='Hello Utopia'
        data-uid='pg'
      />
    </Scene>
  </Storyboard>
)
`,
    ['/utopia/components.utopia.js']: `const Components = {
  '/utopia/storyboard': {
    Image: {
      properties: {
        url: {
          control: 'string-input',
        },
      },
      variants: [
        {
          code: '<Image />',
        },
      ],
    }
  },
}

export default Components  
`,
  })

function getRenderedOptions(editor: EditorRenderResult) {
  return [
    ...editor.renderedDOM.baseElement.querySelectorAll(
      `[data-testid^="variable-from-scope"] [data-testid="variable-name"]`,
    ),
  ].map((node) => node.textContent)
}
