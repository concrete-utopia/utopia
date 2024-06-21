import type { EditorState } from './editor-state'
import {
  createEditorState,
  defaultModifyParseSuccess,
  modifyUnderlyingTargetElement,
  removeElementAtPath,
  StoryboardFilePath,
} from './editor-state'
import {
  defaultProjectContentsForNormalising,
  printParsedCodeForFile,
} from '../../custom-code/code-file.test-utils'
import {
  emptyComments,
  isJSXConditionalExpression,
  isJSXFragment,
  jsExpressionValue,
  jsxElement,
  JSXElement,
  setJSXAttributesAttribute,
  utopiaJSXComponent,
} from '../../../core/shared/element-template'
import { printCode, printCodeOptions } from '../../../core/workers/parser-printer/parser-printer'
import type { Imports, ParseSuccess } from '../../../core/shared/project-file-types'
import { importDetails } from '../../../core/shared/project-file-types'
import {
  importAlias,
  isParseSuccess,
  parseSuccess,
  RevisionsState,
} from '../../../core/shared/project-file-types'
import { addImport, emptyImports } from '../../../core/workers/common/project-file-utils'
import { omit } from '../../../core/shared/object-utils'
import * as EP from '../../../core/shared/element-path'
import { sampleJsxComponents } from '../../../core/model/test-ui-js-file.test-utils'
import { getTextFileByPath } from '../../assets'

describe('modifyUnderlyingTarget', () => {
  const startingEditorModel = {
    ...createEditorState(() => {}),
    projectContents: defaultProjectContentsForNormalising(),
  }
  it('changes something in the same file', () => {
    const pathToElement = EP.fromString('app-outer-div/card-instance')
    const actualResult = modifyUnderlyingTargetElement(
      pathToElement,
      startingEditorModel,
      (element) => {
        if (isJSXConditionalExpression(element) || isJSXFragment(element)) {
          return element
        }
        const updatedAttributes = setJSXAttributesAttribute(
          element.props,
          'data-thing',
          jsExpressionValue('a thing', emptyComments),
          'include-in-printing',
        )
        return jsxElement(element.name, element.uid, updatedAttributes, element.children)
      },
    )
    const resultingCode = printParsedCodeForFile(actualResult, '/src/app.js', 'dont-strip')
    expect(resultingCode).toMatchInlineSnapshot(`
      "import * as React from 'react'
      import { Card } from '/src/card.js'
      export var App = (props) => {
        return (
          <div
            data-uid='app-outer-div'
            style={{
              position: 'relative',
              width: '100%',
              height: '100%',
              backgroundColor: '#FFFFFF',
            }}
          >
            <Card
              data-uid='card-instance'
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
          success.fullHighlightBounds,
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
          addImport('', 'react', null, [], 'React', emptyImports()).imports,
        ).imports,
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
      startingEditorModel,
      (element) => {
        if (isJSXConditionalExpression(element) || isJSXFragment(element)) {
          return element
        }
        const updatedAttributes = setJSXAttributesAttribute(
          element.props,
          'data-thing',
          jsExpressionValue('a thing', emptyComments),
          'include-in-printing',
        )
        return jsxElement(element.name, element.uid, updatedAttributes, element.children)
      },
    )
    const resultingCode = printParsedCodeForFile(actualResult, '/src/card.js', 'dont-strip')
    expect(resultingCode).toMatchInlineSnapshot(`
      "import * as React from 'react'
      import { Spring } from 'non-existant-dummy-library'
      export var Card = (props) => {
        return (
          <div
            data-uid='card-outer-div'
            style={{ ...props.style }}
          >
            <div
              data-uid='card-inner-div'
              data-testid='card-inner-div'
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
              data-uid='card-inner-spring'
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
  it('tries to change something with a nonsense element path', () => {
    const pathToElement = EP.fromString('moon-palace/living-room')
    const modifyCall = () =>
      modifyUnderlyingTargetElement(pathToElement, startingEditorModel, (element) => {
        if (isJSXConditionalExpression(element) || isJSXFragment(element)) {
          return element
        }
        const updatedAttributes = setJSXAttributesAttribute(
          element.props,
          'data-thing',
          jsExpressionValue('a thing', emptyComments),
          'include-in-printing',
        )
        return jsxElement(element.name, element.uid, updatedAttributes, element.children)
      })
    expect(modifyCall).toThrowError(`Could not find element with path moon-palace/living-room`)
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
      startingEditorModel,
      (element) => {
        if (isJSXConditionalExpression(element) || isJSXFragment(element)) {
          return element
        }
        const updatedAttributes = setJSXAttributesAttribute(
          element.props,
          'data-thing',
          jsExpressionValue('a thing', emptyComments),
          'include-in-printing',
        )
        return jsxElement(element.name, element.uid, updatedAttributes, element.children)
      },
    )
    const resultingFile = getTextFileByPath(actualResult.projectContents, '/src/app.js')
    expect(resultingFile.fileContents.revisionsState).toEqual('PARSED_AHEAD')
  })
})

describe('removeElementAtPath', () => {
  it('should remove the elements imports', () => {
    const removeElementResult = removeElementAtPath(
      EP.fromString('aaa/mycomponent'),
      sampleJsxComponents,
      { 'my-component': importDetails(null, [importAlias('MyComponent')], null) },
    )
    expect(removeElementResult.imports['my-component']).toEqual(undefined)
  })

  it('should remove correct import', () => {
    const removeElementResult = removeElementAtPath(
      EP.fromString('aaa/mycomponent'),
      sampleJsxComponents,
      { 'my-component': importDetails('OtherImport', [importAlias('MyComponent')], null) },
    )
    expect(removeElementResult.imports['my-component']).toEqual(
      importDetails('OtherImport', [], null),
    )
  })

  it('shouldnt remove an import if another component is using it', () => {
    const removeElementResult = removeElementAtPath(EP.fromString('aaa/ggg'), sampleJsxComponents, {
      'utopia-api': importDetails(null, [importAlias('View')], null),
    })
    expect(removeElementResult.imports['utopia-api']).toEqual(
      importDetails(null, [importAlias('View')], null),
    )
  })
})
