import { within } from '@testing-library/react'
import * as EP from '../../../../core/shared/element-path'
import { selectComponentsForTest, wait } from '../../../../utils/utils.test-utils'
import { mouseClickAtPoint, pressKey } from '../../../canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../../../canvas/ui-jsx.test-utils'
import {
  getPrintedUiJsCode,
  renderTestEditorWithCode,
  renderTestEditorWithModel,
} from '../../../canvas/ui-jsx.test-utils'
import {
  DataPickerPopupButtonTestId,
  DataPickerPopupTestId,
  VariableFromScopeOptionTestId,
} from './component-section'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { isRight } from '../../../../core/shared/either'
import {
  isJSElementAccess,
  isJSExpressionValue,
  isJSIdentifier,
} from '../../../../core/shared/element-template'
import { createModifiedProject } from '../../../../sample-projects/sample-project-utils.test-utils'
import { StoryboardFilePath } from '../../../editor/store/editor-state'
import { applyPrettier } from 'utopia-vscode-common'
import { ImagePreviewTestId } from './property-content-preview'

xdescribe('Set element prop via the data picker', () => {
  it('can pick from the property data picker', async () => {
    function checkIsItalicised(testID: string, shouldBeItalicised: boolean): void {
      const htmlElement = editor.renderedDOM.getByTestId(testID)
      if (shouldBeItalicised) {
        expect(htmlElement.style.getPropertyValue('font-style')).toContain('italic')
      } else {
        expect(htmlElement.style.getPropertyValue('font-style')).not.toContain('italic')
      }
    }
    const editor = await renderTestEditorWithModel(project, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/title')])

    const dataPickerOpenerButton = editor.renderedDOM.getByTestId(DataPickerPopupButtonTestId)
    await mouseClickAtPoint(dataPickerOpenerButton, { x: 2, y: 2 })

    const dataPickerPopup = editor.renderedDOM.queryByTestId(DataPickerPopupTestId)
    expect(dataPickerPopup).not.toBeNull()

    const theScene = editor.renderedDOM.getByTestId('scene')
    const theInspector = editor.renderedDOM.getByTestId('inspector-sections-container')

    const options = getRenderedOptions(editor)

    // the items from the data picker are expected here, so that the numbers in `VariableFromScopeOptionTestId`
    // below are put in context
    expect(options).toEqual([
      'titleToo',
      'alternateTitle',
      'titles',
      'one',
      'also JS',
      'titleIdeas',
      'titleIdeas[0]',
    ])
    // choose a string-valued variable
    let currentOption = editor.renderedDOM.getByTestId(VariableFromScopeOptionTestId('0'))
    await mouseClickAtPoint(currentOption, { x: 2, y: 2 })
    expect(within(theScene).queryByText('Title too')).not.toBeNull()
    expect(within(theInspector).queryByText('Title too')).not.toBeNull()
    checkIsItalicised(`variable-from-scope-span-titleToo`, true)
    checkIsItalicised(`variable-from-scope-span-alternateTitle`, false)
    checkIsItalicised(`variable-from-scope-span-titles`, false)
    checkIsItalicised(`variable-from-scope-span-titles,one`, false)
    checkIsItalicised(`variable-from-scope-span-titles,also JS`, false)
    checkIsItalicised(`variable-from-scope-span-titleIdeas`, false)
    checkIsItalicised(`variable-from-scope-span-titleIdeas,0`, false)

    // choose another string-valued variable
    currentOption = editor.renderedDOM.getByTestId(VariableFromScopeOptionTestId('1'))
    await mouseClickAtPoint(currentOption, { x: 2, y: 2 })
    expect(within(theScene).queryByText('Alternate title')).not.toBeNull()
    expect(within(theInspector).queryByText('Alternate title')).not.toBeNull()
    checkIsItalicised(`variable-from-scope-span-titleToo`, false)
    checkIsItalicised(`variable-from-scope-span-alternateTitle`, true)
    checkIsItalicised(`variable-from-scope-span-titles`, false)
    checkIsItalicised(`variable-from-scope-span-titles,one`, false)
    checkIsItalicised(`variable-from-scope-span-titles,also JS`, false)
    checkIsItalicised(`variable-from-scope-span-titleIdeas`, false)
    checkIsItalicised(`variable-from-scope-span-titleIdeas,0`, false)

    // choose an object prop
    currentOption = editor.renderedDOM.getByTestId(VariableFromScopeOptionTestId('2-0'))
    await mouseClickAtPoint(currentOption, { x: 2, y: 2 })
    expect(within(theScene).queryByText('The First Title')).not.toBeNull()
    expect(within(theInspector).queryByText('The First Title')).not.toBeNull()
    checkIsItalicised(`variable-from-scope-span-titleToo`, false)
    checkIsItalicised(`variable-from-scope-span-alternateTitle`, false)
    checkIsItalicised(`variable-from-scope-span-titles`, true)
    checkIsItalicised(`variable-from-scope-span-titles,one`, true)
    checkIsItalicised(`variable-from-scope-span-titles,also JS`, false)
    checkIsItalicised(`variable-from-scope-span-titleIdeas`, false)
    checkIsItalicised(`variable-from-scope-span-titleIdeas,0`, false)

    // choose an object prop
    currentOption = editor.renderedDOM.getByTestId(VariableFromScopeOptionTestId('2-1'))
    await mouseClickAtPoint(currentOption, { x: 2, y: 2 })
    expect(within(theScene).queryByText('Sweet')).not.toBeNull()
    expect(within(theInspector).queryByText('Sweet')).not.toBeNull()
    checkIsItalicised(`variable-from-scope-span-titleToo`, false)
    checkIsItalicised(`variable-from-scope-span-alternateTitle`, false)
    checkIsItalicised(`variable-from-scope-span-titles`, true)
    checkIsItalicised(`variable-from-scope-span-titles,one`, false)
    checkIsItalicised(`variable-from-scope-span-titles,also JS`, true)
    checkIsItalicised(`variable-from-scope-span-titleIdeas`, false)
    checkIsItalicised(`variable-from-scope-span-titleIdeas,0`, false)

    // choose an array element
    currentOption = editor.renderedDOM.getByTestId(VariableFromScopeOptionTestId('3-0'))
    await mouseClickAtPoint(currentOption, { x: 2, y: 2 })
    expect(within(theScene).queryByText('Chapter One')).not.toBeNull()
    expect(within(theInspector).queryByText('Chapter One')).not.toBeNull()
    checkIsItalicised(`variable-from-scope-span-titleToo`, false)
    checkIsItalicised(`variable-from-scope-span-alternateTitle`, false)
    checkIsItalicised(`variable-from-scope-span-titles`, false)
    checkIsItalicised(`variable-from-scope-span-titles,one`, false)
    checkIsItalicised(`variable-from-scope-span-titles,also JS`, false)
    checkIsItalicised(`variable-from-scope-span-titleIdeas`, true)
    checkIsItalicised(`variable-from-scope-span-titleIdeas,0`, true)
  })

  it('with number input control descriptor present', async () => {
    const editor = await renderTestEditorWithModel(
      projectWithNumberInputControlDescription,
      'await-first-dom-report',
    )
    await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/counter')])

    const dataPickerOpenerButton = editor.renderedDOM.getByTestId(DataPickerPopupButtonTestId)
    await mouseClickAtPoint(dataPickerOpenerButton, { x: 2, y: 2 })

    const dataPickerPopup = editor.renderedDOM.queryByTestId(DataPickerPopupTestId)
    expect(dataPickerPopup).not.toBeNull()

    const options = getRenderedOptions(editor)

    expect(options).toEqual([
      'currentCount', // at the top because the number input control descriptor is present
      'titleToo', // because the current value of `count` is a string
      'titleIdeas', // doesn't match control descriptor, nor the shape of the prop value
      'titleIdeas[0]',
    ])
  })

  it('with array control descriptor present', async () => {
    const editor = await renderTestEditorWithModel(
      projectWithObjectsAndArrays,
      'await-first-dom-report',
    )
    await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/toc')])

    const dataPickerOpenerButton = editor.renderedDOM.getByTestId(DataPickerPopupButtonTestId)
    await mouseClickAtPoint(dataPickerOpenerButton, { x: 2, y: 2 })

    const dataPickerPopup = editor.renderedDOM.queryByTestId(DataPickerPopupTestId)
    expect(dataPickerPopup).not.toBeNull()

    const options = getRenderedOptions(editor)

    expect(options).toEqual([
      'titleIdeas', // the array is at the top because of the array descriptor
      'titleIdeas[0]', // <- array element
      'titleToo',
      'currentCount',
      'bookInfo',
      'title',
      'published',
      'description',
      'likes',
    ])
  })

  it('with object control descriptor present', async () => {
    const editor = await renderTestEditorWithModel(
      projectWithObjectsAndArrays,
      'await-first-dom-report',
    )
    await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/bd')])

    const dataPickerOpenerButton = editor.renderedDOM.getByTestId(DataPickerPopupButtonTestId)
    await mouseClickAtPoint(dataPickerOpenerButton, { x: 2, y: 2 })

    const dataPickerPopup = editor.renderedDOM.queryByTestId(DataPickerPopupTestId)
    expect(dataPickerPopup).not.toBeNull()

    const options = getRenderedOptions(editor)

    expect(options).toEqual([
      'bookInfo', // object is at the top because of the object descriptor
      'title', // <- object key
      'published', // <- object key
      'description', // <- object key
      'likes', // <- object key
      'titleToo',
      'currentCount',
      'titleIdeas',
      'titleIdeas[0]',
    ])
  })

  it("with another array matching the prop array's shape", async () => {
    const editor = await renderTestEditorWithModel(
      DataPickerProjectShell(`
      function TableOfContents({ titles }) {
        const content = 'Content'
      
        return (
          <>
            {titles.map((t) => (
              <h2 data-uid='a9c'>{t}</h2>
            ))}
          </>
        )
      }
      
      var Playground = () => {
        const titleToo = 'Title too'
        const currentCount = 12
        const authors = ['Jack London', 'Mary Shelley']
        const titleIdeas = ['Chapter One', 'Chapter Two']
      
        return (
          <div data-uid='root'>
            <TableOfContents titles={titleIdeas} data-uid='toc' />
          </div>
        )
      }`),
      'await-first-dom-report',
    )
    await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/toc')])

    const dataPickerOpenerButton = editor.renderedDOM.getByTestId(DataPickerPopupButtonTestId)
    await mouseClickAtPoint(dataPickerOpenerButton, { x: 2, y: 2 })

    const dataPickerPopup = editor.renderedDOM.queryByTestId(DataPickerPopupTestId)
    expect(dataPickerPopup).not.toBeNull()

    const options = getRenderedOptions(editor)

    expect(options).toEqual([
      'authors', // at the top because it's an array of string, same as titleIdeas
      'authors[0]',
      'titleIdeas', // the original array of string
      'titleIdeas[0]',
      'titleToo',
      'currentCount',
    ])
  })

  it("with another object matching the prop object's shape", async () => {
    const editor = await renderTestEditorWithModel(
      DataPickerProjectShell(`
      function BookDetail({ book }) {
        const content = 'Content'
      
        return (
          <div data-uid='aak'>
            <h1 data-uid='aae'>{book.title}</h1>
            <code data-uid='aai'>{book.published}</code>
            <p data-uid='88b'>{book.description}</p>
            <p data-uid='a48'>Likes: {book.likes}</p>
          </div>
        )
      }
      
      var Playground = () => {
        const titleToo = 'Title too'
        const authors = { jack: "Jack London", mary: "Mary Shelley" }
      
        const bookInfo = {
          title: 'Moby Dick',
          published: 'August 1888',
          description: 'An oldie but goldie',
          likes: 33,
        }
      
        const alternateBookInfo = {
          title: 'Frankenstein',
          published: 'August 1866',
          description: 'Short, fun read',
          likes: 66,
        }
      
        return (
          <div data-uid='root'>
            <BookDetail book={bookInfo} data-uid='bd' />
          </div>
        )
      }`),
      'await-first-dom-report',
    )
    await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/bd')])

    const dataPickerOpenerButton = editor.renderedDOM.getByTestId(DataPickerPopupButtonTestId)
    await mouseClickAtPoint(dataPickerOpenerButton, { x: 2, y: 2 })

    const dataPickerPopup = editor.renderedDOM.queryByTestId(DataPickerPopupTestId)
    expect(dataPickerPopup).not.toBeNull()

    const options = getRenderedOptions(editor)

    expect(options).toEqual([
      'bookInfo', // object with matching shape
      'title', // <- object key
      'published', // <- object key
      'description', // <- object key
      'likes', // <- object key
      'alternateBookInfo', // object with matching shape
      'title', // <- object key
      'published', // <- object key
      'description', // <- object key
      'likes', // <- object key
      'titleToo',
      'authors', // object with a shape that doesn't match
      'jack',
      'mary',
    ])
  })

  it('style props are filterd from `props`', async () => {
    const editor = await renderTestEditorWithModel(
      DataPickerProjectShell(`
      function TableOfContents({ titles }) {
        const content = 'Content'
      
        return (
          <>
            {titles.map((t) => (
              <h2 data-uid='a9c'>{t}</h2>
            ))}
          </>
        )
      }
      
      var Playground = (props) => {
        const titleToo = 'Title too'
        const currentCount = 12
        const authors = ['Jack London', 'Mary Shelley']
        const titleIdeas = ['Chapter One', 'Chapter Two']
      
        return (
          <div data-uid='root'>
            <TableOfContents titles={titleIdeas} data-uid='toc' />
          </div>
        )
      }`),
      'await-first-dom-report',
    )
    await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/toc')])

    const dataPickerOpenerButton = editor.renderedDOM.getByTestId(DataPickerPopupButtonTestId)
    await mouseClickAtPoint(dataPickerOpenerButton, { x: 2, y: 2 })

    const dataPickerPopup = editor.renderedDOM.queryByTestId(DataPickerPopupTestId)
    expect(dataPickerPopup).not.toBeNull()

    const options = getRenderedOptions(editor)

    expect(options).toEqual([
      'authors',
      'authors[0]',
      'titleIdeas',
      'titleIdeas[0]',
      'props',
      'titleToo',
      'currentCount',
    ])
  })

  it('style props are filtered from destructured props', async () => {
    const editor = await renderTestEditorWithModel(
      DataPickerProjectShell(`
      function TableOfContents({ titles }) {
        const content = 'Content'
      
        return (
          <>
            {titles.map((t) => (
              <h2 data-uid='a9c'>{t}</h2>
            ))}
          </>
        )
      }
      
      var Playground = ({ style, className, css }) => {
        const titleToo = 'Title too'
        const currentCount = 12
        const authors = ['Jack London', 'Mary Shelley']
        const titleIdeas = ['Chapter One', 'Chapter Two']
      
        return (
          <div data-uid='root'>
            <TableOfContents titles={titleIdeas} data-uid='toc' />
          </div>
        )
      }`),
      'await-first-dom-report',
    )
    await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/toc')])

    const dataPickerOpenerButton = editor.renderedDOM.getByTestId(DataPickerPopupButtonTestId)
    await mouseClickAtPoint(dataPickerOpenerButton, { x: 2, y: 2 })

    const dataPickerPopup = editor.renderedDOM.queryByTestId(DataPickerPopupTestId)
    expect(dataPickerPopup).not.toBeNull()

    const options = getRenderedOptions(editor)

    expect(options).toEqual([
      'authors',
      'authors[0]',
      'titleIdeas',
      'titleIdeas[0]',
      'titleToo',
      'currentCount',
    ])
  })

  it('object props are reordered by relevance', async () => {
    const editor = await renderTestEditorWithModel(
      DataPickerProjectShell(`
      function Title({ text }) {
        const content = 'Content'
      
        return <h2 data-uid='aam'>{text}</h2>
      }
      
      var Playground = ({ style }) => {
        const titles = {
          aNumber: 2,
          aBoolean: true,
          actualStringTitle: 'The First Title',
        }
      
        return (
          <div style={style} data-uid='root'>
            <Title
              text={'hi'}
              data-uid='bd'
              style={{
                width: 134,
                height: 28,
                position: 'absolute',
                left: 160,
                top: 75,
              }}
            />
          </div>
        )
      }`),
      'await-first-dom-report',
    )
    await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/bd')])

    const dataPickerOpenerButton = editor.renderedDOM.getByTestId(DataPickerPopupButtonTestId)
    await mouseClickAtPoint(dataPickerOpenerButton, { x: 2, y: 2 })

    const dataPickerPopup = editor.renderedDOM.queryByTestId(DataPickerPopupTestId)
    expect(dataPickerPopup).not.toBeNull()

    const options = getRenderedOptions(editor)

    expect(options).toEqual([
      'titles', // the name of the object
      'actualStringTitle', // the actual string prop
      'aNumber', // the rest of the props
      'aBoolean',
    ])
  })

  it('array with matching elements has priority', async () => {
    const editor = await renderTestEditorWithModel(
      DataPickerProjectShell(`
      function Title({ text }) {
        const content = 'Content'
      
        return <h2 data-uid='aam'>{text}</h2>
      }
      
      var Playground = ({ style }) => {
        const titles = {
          ['also JS']: 2,
          one: false,
        }
      
        const titleIdeas = ['Chapter One']
      
        return (
          <div style={style} data-uid='root'>
            <Title
              text={'a'}
              data-uid='bd'
              style={{
                width: 134,
                height: 28,
                position: 'absolute',
                left: 160,
                top: 75,
              }}
            />
          </div>
        )
      }`),
      'await-first-dom-report',
    )
    await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/bd')])

    const dataPickerOpenerButton = editor.renderedDOM.getByTestId(DataPickerPopupButtonTestId)
    await mouseClickAtPoint(dataPickerOpenerButton, { x: 2, y: 2 })

    const dataPickerPopup = editor.renderedDOM.queryByTestId(DataPickerPopupTestId)
    expect(dataPickerPopup).not.toBeNull()

    const options = getRenderedOptions(editor)

    expect(options).toEqual([
      'titleIdeas', // the array is moved up
      'titleIdeas[0]', // <- the array element
      'titles', // an object with non-matching props
      'also JS',
      'one',
    ])
  })

  it('object with matching elements has priority', async () => {
    const editor = await renderTestEditorWithModel(
      DataPickerProjectShell(`
      function Title({ text }) {
        const content = 'Content'
      
        return <h2 data-uid='aam'>{text}</h2>
      }
      
      var Playground = ({ style }) => {
        const nums = [1, 2, 3, 4, 5]
        
        const titles = {
          ['also JS']: 2,
          one: "Hi",
        }
      
      
        return (
          <div style={style} data-uid='root'>
            <Title
              text={'a'}
              data-uid='bd'
              style={{
                width: 134,
                height: 28,
                position: 'absolute',
                left: 160,
                top: 75,
              }}
            />
          </div>
        )
      }`),
      'await-first-dom-report',
    )
    await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/bd')])

    const dataPickerOpenerButton = editor.renderedDOM.getByTestId(DataPickerPopupButtonTestId)
    await mouseClickAtPoint(dataPickerOpenerButton, { x: 2, y: 2 })

    const dataPickerPopup = editor.renderedDOM.queryByTestId(DataPickerPopupTestId)
    expect(dataPickerPopup).not.toBeNull()

    const options = getRenderedOptions(editor)

    expect(options).toEqual(['titles', 'one', 'also JS', 'nums', 'nums[0]'])
  })

  it('for jsx props react elements have highest priority, strings/numbers are next, and the rest is lower', async () => {
    const editor = await renderTestEditorWithModel(
      DataPickerProjectShell(
        `
      export function Title({ text }) {
        const content = 'Content'
      
        return <h2 data-uid='aam'>{text}</h2>
      }      
      
      var Playground = ({ style }) => {
        const nums = [1, 2, 3, 4, 5]
        
        const titleString = 'This is the title'
        const titleNumber = 99999
        const titleJsx = <span>This is the title</span>
      
        return (
          <div style={style} data-uid='root'>
            <Title
              text={<span>a</span>}
              data-uid='bd'
              style={{
                width: 134,
                height: 28,
                position: 'absolute',
                left: 160,
                top: 75,
              }}
            />
          </div>
        )
      }`,
        `{
          '/utopia/storyboard': {
            Title: {
              component: Title,
              properties: {
                text: {
                  control: 'jsx',
                },
              },
              variants: [
                {
                  label: 'title',
                  imports: 'import { Title } from "/utopia/storyboard"',
                  code: '<Title />',
                },
              ],
            },
          },
        }`,
        `import { Title } from './storyboard'`,
      ),
      'await-first-dom-report',
    )
    await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/bd')])

    const dataPickerOpenerButton = editor.renderedDOM.getByTestId(DataPickerPopupButtonTestId)
    await mouseClickAtPoint(dataPickerOpenerButton, { x: 2, y: 2 })

    const dataPickerPopup = editor.renderedDOM.queryByTestId(DataPickerPopupTestId)
    expect(dataPickerPopup).not.toBeNull()

    const options = getRenderedOptions(editor)

    expect(options).toEqual(['titleJsx', 'titleString', 'titleNumber', 'nums', 'nums[0]'])
  })

  it('picking data for the children prop', async () => {
    const editor = await renderTestEditorWithModel(
      DataPickerProjectShell(
        `export function Link({ href, children }) {
      return (
        <a href={href} data-uid='a-root'>
          {children}
        </a>
      )
    }
    
    var Playground = ({ style }) => {
      const alternateBookInfo = {
        title: 'Frankenstein',
        published: 'August 1866',
        description: 'Short, fun read',
        likes: 66,
      }
    
      return (
        <div style={style} data-uid='root'>
          <Link href='/new' data-uid='link'>
            <code>TODO</code>
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
          variants: [],
        },
      },
    }`,
        `import { Link } from './storyboard'`,
      ),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/link')])

    const dataPickerOpenerButton = editor.renderedDOM.getAllByTestId(DataPickerPopupButtonTestId)[0]
    await mouseClickAtPoint(dataPickerOpenerButton, { x: 2, y: 2 })

    const dataPickerPopup = editor.renderedDOM.queryByTestId(DataPickerPopupTestId)
    expect(dataPickerPopup).not.toBeNull()

    const options = getRenderedOptions(editor)

    expect(options).toEqual(['alternateBookInfo', 'title', 'published', 'description', 'likes'])

    const theScene = editor.renderedDOM.getByTestId('scene')
    let currentOption = editor.renderedDOM.getByTestId(VariableFromScopeOptionTestId('0-0'))
    await mouseClickAtPoint(currentOption, { x: 2, y: 2 })
    expect(within(theScene).queryByText('Frankenstein')).not.toBeNull()

    const childrenOfLink = MetadataUtils.getChildrenOrdered(
      editor.getEditorState().editor.jsxMetadata,
      editor.getEditorState().editor.elementPathTree,
      EP.fromString('sb/scene/pg:root/link'),
    )

    expect(childrenOfLink).toHaveLength(1)
    const possibleElement = childrenOfLink[0].element
    if (isRight(possibleElement)) {
      const element = possibleElement.value
      if (isJSElementAccess(element)) {
        if (isJSIdentifier(element.onValue)) {
          expect(element.onValue.name).toEqual('alternateBookInfo')
          if (isJSExpressionValue(element.element)) {
            expect(element.element.value).toEqual('title')
          } else {
            throw new Error(`Expected JSExpressionValue for element but got: ${element.element}`)
          }
        } else {
          throw new Error(`Expected JSIdentifier for onValue but got: ${element.onValue}`)
        }
      } else {
        throw new Error(`Expected JSElementAccess but got: ${element}`)
      }
    } else {
      throw new Error(`Unexpected value: ${possibleElement.value}`)
    }
  })
})

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

const project = DataPickerProjectShell(`
function Title({ text }) {
  const content = 'Content'

  return <h2 data-uid='0cd'>{text}</h2>
}

var Playground = ({ style }) => {
  const titleToo = 'Title too'

  const alternateTitle = 'Alternate title'

  const titles = {
    one: "The First Title",
    ['also JS']: 'Sweet',
  }

  const titleIdeas = [
    'Chapter One',
  ]

  return (
    <div style={style} data-uid='root'>
      <Title text='The Title' data-uid='title' />
    </div>
  )
}`)

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

const projectWithNumberInputControlDescription = DataPickerProjectShell(
  `export function Counter({ count }) {
  const content = 'Content'

  return <h2 data-uid='a9c'>{count}</h2>
}

var Playground = () => {
  const titleToo = 'Title too'
  const titleIdeas = ['Chapter One']
  const currentCount = 12

  return (
    <div data-uid='root'>
      <Counter count={''} data-uid='counter' />
    </div>
  )
}`,
  `{
  '/utopia/storyboard': {
    Counter: {
      component: Counter,
      properties: {
        count: {
          control: 'number-input',
        },
      },
      variants: [],
    },
  },
}`,
  `import { Counter } from './storyboard'`,
)

const projectWithObjectsAndArrays = DataPickerProjectShell(
  `export function TableOfContents({ titles }) {
  const content = 'Content'

  return (
    <>
      {titles.map((t) => (
        <h2 data-uid='a9c'>{t}</h2>
      ))}
    </>
  )
}

export function BookDetail({ book }) {
  const content = 'Content'

  return (
    <div>
      <h1>{book.title}</h1>
      <code>{book.published}</code>
      <p>{book.description}</p>
      <p>Likes: {book.likes}</p>
    </div>
  )
}

var Playground = () => {
  const titleToo = 'Title too'
  const currentCount = 12
  const titleIdeas = ['Chapter One', 'Chapter Two']

  const bookInfo = {
    title: 'Moby Dick',
    published: 'August 1888',
    description: 'An oldie but goldie',
    likes: 33,
  }

  return (
    <div data-uid='root'>
      <TableOfContents titles={[]} data-uid='toc' />
      <BookDetail book={{}} data-uid='bd' />
    </div>
  )
}`,
  `{
  '/utopia/storyboard': {
    TableOfContents: {
      component: TableOfContents,
      properties: {
        titles: {
          control: 'array',
          propertyControl: { control: 'string-input' },
        },
      },
      variants: [],
    },
    BookDetail: {
      component: BookDetail,
      properties: {
        book: {
          control: 'object',
          object: {
            title: { control: 'string-input' },
            published: { control: 'string-input' },
            description: { control: 'string-input' },
            likes: { control: 'number-input' },
          },
        },
      },
      variants: [],
    },
  },
}`,
  `import { TableOfContents, BookDetail } from './storyboard'`,
)

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
