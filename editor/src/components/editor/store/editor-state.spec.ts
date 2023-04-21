import {
  createEditorState,
  defaultModifyParseSuccess,
  EditorState,
  modifyUnderlyingTargetElement,
  StoryboardFilePath,
} from './editor-state'
import {
  defaultProjectContentsForNormalising,
  getTextFileByPath,
} from '../../custom-code/code-file.test-utils'
import {
  emptyComments,
  isJSXConditionalExpression,
  jsExpressionValue,
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
  RevisionsState,
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
    const actualResult = modifyUnderlyingTargetElement(
      pathToElement,
      '/src/app.js',
      startingEditorModel,
      (element) => {
        if (isJSXConditionalExpression(element)) {
          return element
        }
        const updatedAttributes = setJSXAttributesAttribute(
          element.props,
          'data-thing',
          jsExpressionValue('a thing', emptyComments),
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
    const actualResult = modifyUnderlyingTargetElement(
      pathToElement,
      '/src/card.js',
      startingEditorModel,
      (element) => element,
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
    const actualResult = modifyUnderlyingTargetElement(
      pathToElement,
      StoryboardFilePath,
      startingEditorModel,
      (element) => {
        if (isJSXConditionalExpression(element)) {
          return element
        }
        const updatedAttributes = setJSXAttributesAttribute(
          element.props,
          'data-thing',
          jsExpressionValue('a thing', emptyComments),
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
      modifyUnderlyingTargetElement(
        pathToElement,
        '/src/app.js',
        startingEditorModel,
        (element) => {
          if (isJSXConditionalExpression(element)) {
            return element
          }
          const updatedAttributes = setJSXAttributesAttribute(
            element.props,
            'data-thing',
            jsExpressionValue('a thing', emptyComments),
          )
          return jsxElement(element.name, element.uid, updatedAttributes, element.children)
        },
      )
    expect(modifyCall).toThrowError(`Did not find element to transform moon-palace/living-room`)
  })
  it('tries to change something in a nonsense file path', () => {
    const pathToElement = EP.fromString('app-outer-div/card-instance')
    const modifyCall = () =>
      modifyUnderlyingTargetElement(
        pathToElement,
        '/src/kitchen.js',
        startingEditorModel,
        (element) => {
          if (isJSXConditionalExpression(element)) {
            return element
          }
          const updatedAttributes = setJSXAttributesAttribute(
            element.props,
            'data-thing',
            jsExpressionValue('a thing', emptyComments),
          )
          return jsxElement(element.name, element.uid, updatedAttributes, element.children)
        },
      )
    expect(modifyCall).toThrowError(`Could not proceed past /src/kitchen.js.`)
  })
})

describe('Revision state management', () => {
  const startingEditorModel = {
    ...createEditorState(() => {}),
    projectContents: defaultProjectContentsForNormalising(),
  }
  it('changes something in a file sets revision state to PARSED_AHEAD', () => {
    const pathToElement = EP.fromString('app-outer-div/card-instance')
    const actualResult = modifyUnderlyingTargetElement(
      pathToElement,
      '/src/app.js',
      startingEditorModel,
      (element) => {
        if (isJSXConditionalExpression(element)) {
          return element
        }
        const updatedAttributes = setJSXAttributesAttribute(
          element.props,
          'data-thing',
          jsExpressionValue('a thing', emptyComments),
        )
        return jsxElement(element.name, element.uid, updatedAttributes, element.children)
      },
    )
    const resultingFile = getTextFileByPath(actualResult.projectContents, '/src/app.js')
    expect(resultingFile.fileContents.revisionsState).toEqual('PARSED_AHEAD')
  })
  it('updating RevisionsState.ParsedAheadNeedsReparsing to ParsedAhead keeps ParsedAheadNeedsReparsing', () => {
    const pathToElement = EP.fromString('app-outer-div/card-instance')

    // This is just initialization, make /src/app.js PARSED_AHEAD
    const actualResult = modifyUnderlyingTargetElement(
      pathToElement,
      '/src/app.js',
      startingEditorModel,
      (element) => {
        if (isJSXConditionalExpression(element)) {
          return element
        }
        const updatedAttributes = setJSXAttributesAttribute(
          element.props,
          'data-thing',
          jsExpressionValue('a thing', emptyComments),
        )
        return jsxElement(element.name, element.uid, updatedAttributes, element.children)
      },
      defaultModifyParseSuccess,
      RevisionsState.ParsedAheadNeedsReparsing,
    )
    const resultingFile = getTextFileByPath(actualResult.projectContents, '/src/app.js')

    // This is the tested feature, RevisionsState.ParsedAheadNeedsReparsing should be kept even if
    // it is tried to be updated to RevisionsState.ParsedAhead
    const actualResult2 = modifyUnderlyingTargetElement(
      pathToElement,
      '/src/app.js',
      actualResult,
      (element) => {
        if (isJSXConditionalExpression(element)) {
          return element
        }
        const updatedAttributes = setJSXAttributesAttribute(
          element.props,
          'data-thing',
          jsExpressionValue('a thing', emptyComments),
        )
        return jsxElement(element.name, element.uid, updatedAttributes, element.children)
      },
      defaultModifyParseSuccess,
      'PARSED_AHEAD',
    )
    const resultingFile2 = getTextFileByPath(actualResult2.projectContents, '/src/app.js')
    expect(resultingFile2.fileContents.revisionsState).toEqual(
      RevisionsState.ParsedAheadNeedsReparsing,
    )
  })
})
