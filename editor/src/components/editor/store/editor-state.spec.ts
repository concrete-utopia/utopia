import {
  createEditorState,
  EditorState,
  modifyUnderlyingTarget,
  StoryboardFilePath,
} from './editor-state'
import {
  defaultProjectContentsForNormalising,
  getTextFileByPath,
} from '../../custom-code/code-file.test-utils'
import {
  emptyComments,
  jsxAttributeValue,
  jsxElement,
  JSXElement,
  setJSXAttributesAttribute,
} from '../../../core/shared/element-template'
import { printCode, printCodeOptions } from '../../../core/workers/parser-printer/parser-printer'
import {
  importAlias,
  Imports,
  isParseSuccess,
  parseSuccess,
  ParseSuccess,
} from '../../../core/shared/project-file-types'
import { addImport, emptyImports } from '../../../core/workers/common/project-file-utils'
import { omit } from '../../../core/shared/object-utils'
import * as EP from '../../../core/shared/element-path'

function getCodeForFile(actualResult: EditorState, filename: string): string {
  const codeFile = getTextFileByPath(actualResult.projectContents, filename)
  const parsed = codeFile.fileContents.parsed
  if (isParseSuccess(parsed)) {
    return printCode(
      filename,
      printCodeOptions(false, true, false, true),
      parsed.imports,
      parsed.topLevelElements,
      parsed.jsxFactoryFunction,
      parsed.exportsDetail,
    )
  } else {
    throw new Error('No parsed version of the file.')
  }
}

describe('modifyUnderlyingTarget', () => {
  const startingEditorModel = {
    ...createEditorState(() => {}),
    projectContents: defaultProjectContentsForNormalising(),
  }
  it('changes something in the same file', () => {
    const pathToElement = EP.fromString('app-outer-div/card-instance')
    const actualResult = modifyUnderlyingTarget(
      pathToElement,
      '/src/app.js',
      startingEditorModel,
      (element: JSXElement) => {
        const updatedAttributes = setJSXAttributesAttribute(
          element.props,
          'data-thing',
          jsxAttributeValue('a thing', emptyComments),
        )
        return jsxElement(element.name, element.uid, updatedAttributes, element.children)
      },
    )
    const resultingCode = getCodeForFile(actualResult, '/src/app.js')
    expect(resultingCode).toMatchInlineSnapshot(`
      "import * as React from 'react'
      import { Card } from '/src/card.js'
      export var App = (props) => {
        return (
          <div
            style={{
              position: 'relative',
              width: '100%',
              height: '100%',
              backgroundColor: '#FFFFFF',
            }}
          >
            <Card
              style={{
                position: 'absolute',
                left: 0,
                top: 0,
                width: 200,
                height: 300,
              }}
              data-thing='a thing'
            />
          </div>
        )
      }
      "
    `)
  })
  it('changes something in the imports of the same file', () => {
    const pathToElement = EP.fromString('card-outer-div/card-inner-div')
    const actualResult = modifyUnderlyingTarget(
      pathToElement,
      '/src/card.js',
      startingEditorModel,
      (element: JSXElement) => element,
      (success: ParseSuccess) => {
        return parseSuccess(
          omit<string, Imports>(['utopia-api'], success.imports),
          success.topLevelElements,
          success.highlightBounds,
          success.jsxFactoryFunction,
          success.combinedTopLevelArbitraryBlock,
          success.exportsDetail,
        )
      },
    )
    const codeFile = getTextFileByPath(actualResult.projectContents, '/src/card.js')
    const parsed = codeFile.fileContents.parsed
    if (isParseSuccess(parsed)) {
      expect(parsed.imports).toEqual(
        addImport(
          '',
          'non-existant-dummy-library',
          null,
          [importAlias('Spring')],
          null,
          addImport('', 'react', null, [], 'React', emptyImports()),
        ),
      )
    } else {
      throw new Error('No parsed version of the file.')
    }
  })
  it('changes something in an underlying file', () => {
    const pathToElement = EP.fromString(
      'storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div/card-inner-div',
    )
    const actualResult = modifyUnderlyingTarget(
      pathToElement,
      StoryboardFilePath,
      startingEditorModel,
      (element: JSXElement) => {
        const updatedAttributes = setJSXAttributesAttribute(
          element.props,
          'data-thing',
          jsxAttributeValue('a thing', emptyComments),
        )
        return jsxElement(element.name, element.uid, updatedAttributes, element.children)
      },
    )
    const resultingCode = getCodeForFile(actualResult, '/src/card.js')
    expect(resultingCode).toMatchInlineSnapshot(`
      "import * as React from 'react'
      import { Spring } from 'non-existant-dummy-library'
      export var Card = (props) => {
        return (
          <div style={{ ...props.style }}>
            <div
              style={{
                position: 'absolute',
                left: 0,
                top: 0,
                width: 50,
                height: 50,
                backgroundColor: 'red',
              }}
              data-thing='a thing'
            />
            <Spring
              data-testid='spring'
              style={{
                position: 'absolute',
                left: 100,
                top: 200,
                width: 50,
                height: 50,
                backgroundColor: 'blue',
              }}
            />
          </div>
        )
      }
      "
    `)
  })
  it('tries to change something in a nonsense template path', () => {
    const pathToElement = EP.fromString('moon-palace/living-room')
    const modifyCall = () =>
      modifyUnderlyingTarget(
        pathToElement,
        '/src/app.js',
        startingEditorModel,
        (element: JSXElement) => {
          const updatedAttributes = setJSXAttributesAttribute(
            element.props,
            'data-thing',
            jsxAttributeValue('a thing', emptyComments),
          )
          return jsxElement(element.name, element.uid, updatedAttributes, element.children)
        },
      )
    expect(modifyCall).toThrowError(`Did not find element to transform moon-palace/living-room`)
  })
  it('tries to change something in a nonsense file path', () => {
    const pathToElement = EP.fromString('app-outer-div/card-instance')
    const modifyCall = () =>
      modifyUnderlyingTarget(
        pathToElement,
        '/src/kitchen.js',
        startingEditorModel,
        (element: JSXElement) => {
          const updatedAttributes = setJSXAttributesAttribute(
            element.props,
            'data-thing',
            jsxAttributeValue('a thing', emptyComments),
          )
          return jsxElement(element.name, element.uid, updatedAttributes, element.children)
        },
      )
    expect(modifyCall).toThrowError(`Could not proceed past /src/kitchen.js.`)
  })
})
