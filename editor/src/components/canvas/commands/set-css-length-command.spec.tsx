import type { CSSObject } from '@emotion/styled'
import { isJSXElement } from '../../../core/shared/element-template'
import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import * as EP from '../../../core/shared/element-path'
import { getNumberPropertyFromProps } from '../../../core/shared/jsx-attributes'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { complexDefaultProjectPreParsed } from '../../../sample-projects/sample-project-utils.test-utils'
import { styleStringInArray } from '../../../utils/common-constants'
import type { EditorState, EditorStorePatched } from '../../editor/store/editor-state'
import { withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import type { ParsedCSSProperties } from '../../inspector/common/css-utils'
import { cssPixelLength } from '../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import {
  DefaultStartingFeatureSwitches,
  renderTestEditorWithCode,
  renderTestEditorWithModel,
} from '../ui-jsx.test-utils'
import { updateEditorStateWithPatches } from './commands'
import type { SetCssLengthProperty } from './set-css-length-command'
import {
  runSetCssLengthProperty,
  setCssLengthProperty,
  setExplicitCssValue,
  setValueKeepingOriginalUnit,
} from './set-css-length-command'

describe('setCssLengthProperty', () => {
  it('works for height style prop', async () => {
    const renderResult = await renderTestEditorWithModel(
      complexDefaultProjectPreParsed(),
      'dont-await-first-dom-report',
      DefaultStartingFeatureSwitches,
      createBuiltInDependenciesList(null),
    )

    const cardInstancePath = EP.elementPath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', 'card-instance'],
    ])

    const valueToSet = 321
    const setCSSPropertyCommand = setCssLengthProperty(
      'always',
      cardInstancePath,
      stylePropPathMappingFn('height', styleStringInArray),
      setValueKeepingOriginalUnit(valueToSet, 400),
      null,
    )

    const result = runSetCssLengthProperty(
      renderResult.getEditorState().editor,
      setCSSPropertyCommand,
      'end-interaction',
    )

    const patchedEditor = updateEditorStateWithPatches(
      renderResult.getEditorState().editor,
      result.editorStatePatches,
    )
    const updatedHeightProp = withUnderlyingTargetFromEditorState(
      cardInstancePath,
      patchedEditor,
      null,
      (success, element, underlyingTarget, underlyingFilePath) => {
        if (isJSXElement(element)) {
          return getNumberPropertyFromProps(
            element.props,
            stylePropPathMappingFn('height', styleStringInArray),
          )
        } else {
          return null
        }
      },
    )

    expect(updatedHeightProp).toEqual(valueToSet)
  })

  it('Setting width removes min-width and max-width props', async () => {
    const editor = await renderTestEditorWithCode(
      projectWithChildStyle('row', { minWidth: 50, maxWidth: 150, width: 50, height: 60 }),
      'await-first-dom-report',
    )

    const targetPath = EP.fromString('sb/parent/child')

    const command = setCssLengthProperty(
      'always',
      targetPath,
      stylePropPathMappingFn('width', styleStringInArray),
      setExplicitCssValue(cssPixelLength(400)),
      'row',
    )

    const result = runCommandUpdateEditor(editor.getEditorState(), command)
    expect(getStylePropForElement(result, targetPath, 'width')).toBe(400)
    expect(getStylePropForElement(result, targetPath, 'minWidth')).toBeNull()
    expect(getStylePropForElement(result, targetPath, 'maxWidth')).toBeNull()
  })

  it('Setting height does not remove min-width and max-width props', async () => {
    const editor = await renderTestEditorWithCode(
      projectWithChildStyle('row', { minWidth: 50, maxWidth: 150, width: 50, height: 60 }),
      'await-first-dom-report',
    )

    const targetPath = EP.fromString('sb/parent/child')

    const command = setCssLengthProperty(
      'always',
      targetPath,
      stylePropPathMappingFn('height', styleStringInArray),
      setExplicitCssValue(cssPixelLength(400)),
      'row',
    )

    const result = runCommandUpdateEditor(editor.getEditorState(), command)
    expect(getStylePropForElement(result, targetPath, 'height')).toBe(400)
    expect(getStylePropForElement(result, targetPath, 'minWidth')).toBe(50)
    expect(getStylePropForElement(result, targetPath, 'maxWidth')).toBe(150)
  })

  it('Setting width removes flex props if the parent is a horizontal flex', async () => {
    const editor = await renderTestEditorWithCode(
      projectWithChildStyle('row', {
        minWidth: 50,
        maxWidth: 150,
        width: 50,
        height: 60,
        flexGrow: 1,
        flexShrink: 1,
        flexBasis: 200,
        flex: 1,
      }),
      'await-first-dom-report',
    )

    const targetPath = EP.fromString('sb/parent/child')

    const command = setCssLengthProperty(
      'always',
      targetPath,
      stylePropPathMappingFn('width', styleStringInArray),
      setExplicitCssValue(cssPixelLength(400)),
      'row',
    )

    const result = runCommandUpdateEditor(editor.getEditorState(), command)
    expect(getStylePropForElement(result, targetPath, 'width')).toBe(400)
    expect(getStylePropForElement(result, targetPath, 'minWidth')).toBeNull()
    expect(getStylePropForElement(result, targetPath, 'maxWidth')).toBeNull()
    expect(getStylePropForElement(result, targetPath, 'flex')).toBeNull()
    expect(getStylePropForElement(result, targetPath, 'flexGrow')).toBeNull()
    expect(getStylePropForElement(result, targetPath, 'flexShrink')).toBeNull()
    expect(getStylePropForElement(result, targetPath, 'flexBasis')).toBeNull()
  })

  it('Setting width keeps flex props if the parent is a vertical flex', async () => {
    const editor = await renderTestEditorWithCode(
      projectWithChildStyle('column', {
        minWidth: 50,
        maxWidth: 150,
        width: 50,
        height: 60,
        flexGrow: 1,
        flexShrink: 1,
        flexBasis: 200,
        flex: 1,
      }),
      'await-first-dom-report',
    )

    const targetPath = EP.fromString('sb/parent/child')

    const command = setCssLengthProperty(
      'always',
      targetPath,
      stylePropPathMappingFn('width', styleStringInArray),
      setExplicitCssValue(cssPixelLength(400)),
      'column',
    )

    const result = runCommandUpdateEditor(editor.getEditorState(), command)
    expect(getStylePropForElement(result, targetPath, 'width')).toBe(400)
    expect(getStylePropForElement(result, targetPath, 'minWidth')).toBeNull()
    expect(getStylePropForElement(result, targetPath, 'maxWidth')).toBeNull()
    expect(getStylePropForElement(result, targetPath, 'flex')).not.toBeNull()
    expect(getStylePropForElement(result, targetPath, 'flexGrow')).not.toBeNull()
    expect(getStylePropForElement(result, targetPath, 'flexShrink')).not.toBeNull()
    expect(getStylePropForElement(result, targetPath, 'flexBasis')).not.toBeNull()
  })
})

function projectWithChildStyle(
  parentFlexDirection: 'row' | 'column',
  childStyleProps: CSSObject,
): string {
  return `import * as React from 'react'
      import { Storyboard } from 'utopia-api'
      
      export var storyboard = (
        <Storyboard data-uid='sb'>
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 133,
              top: 228,
              width: 342,
              height: 368,
              display: 'flex',
              flexDirection: ${JSON.stringify(parentFlexDirection)}
            }}
            data-uid='parent'
          >
            <div
              style={${JSON.stringify(childStyleProps)}}
              data-uid='child'
            />
          </div>
        </Storyboard>
      )
      `
}

function runCommandUpdateEditor(
  store: EditorStorePatched,
  command: SetCssLengthProperty,
): EditorState {
  const result = runSetCssLengthProperty(store.editor, command, 'end-interaction')

  return updateEditorStateWithPatches(store.editor, result.editorStatePatches)
}

function getStylePropForElement(
  editor: EditorState,
  elementPath: ElementPath,
  styleProp: keyof ParsedCSSProperties,
): number | null {
  return withUnderlyingTargetFromEditorState(
    elementPath,
    editor,
    null,
    (success, element, underlyingTarget, underlyingFilePath) => {
      if (isJSXElement(element)) {
        return getNumberPropertyFromProps(
          element.props,
          stylePropPathMappingFn(styleProp, styleStringInArray),
        )
      } else {
        return null
      }
    },
  )
}
