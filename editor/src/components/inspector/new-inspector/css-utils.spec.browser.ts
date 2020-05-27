import {
  renderTestEditorWithCode,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  TestScenePath,
} from '../../canvas/ui-jsx-test-utils'
import * as TP from '../../../core/shared/template-path'
import * as PP from '../../../core/shared/property-path'
import { toggleProperty } from '../../editor/actions/actions'
import { toggleStyleProp, toggleBorder, toggleShadow } from './css-utils'
import * as Prettier from 'prettier'
import { PrettierConfig } from '../../../core/workers/parser-printer/prettier-utils'
import { BakedInStoryboardUID } from '../../../core/model/scene-utils'

function makeTestProjectCodeWithSnippetWithUtopiaUtilsImport(snippet: string) {
  const code = `/** @jsx jsx */
  import * as React from 'react'
  import { Scene, Storyboard, UtopiaUtils, View, jsx } from 'utopia-api'
  export var App = (props) => {
    return (
${snippet}
    )
  }
  export var storyboard = (props) => {
    return (
      <Storyboard data-uid={'${BakedInStoryboardUID}'}>
        <Scene
          style={{ left: 0, top: 0, width: 400, height: 400 }}
          component={App}
          layout={{ layoutSystem: 'pinSystem' }}
          props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
          data-uid={'scene-aaa'}
        />
      </Storyboard>
    )
  }
  `
  return Prettier.format(code, PrettierConfig)
}

describe('toggle style prop', () => {
  it('disables border, sets border to none from solid', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View
            style={{
              backgroundColor: '#DDDDDD',
              boxShadow: UtopiaUtils.shadowAndBorder({
                borderStyle: 'solid',
                borderWidth: '1px',
                borderColor: '#000',
              }),
            }}
            layout={{ layoutSystem: 'pinSystem', left: 52, top: 61, width: 256, height: 202 }}
            data-uid={'bbb'}
          />
        </View>
      `),
    )

    await renderResult.dispatch(
      [
        toggleProperty(
          TP.instancePath(TestScenePath, ['aaa', 'bbb']),
          toggleStyleProp(PP.create(['style', 'boxShadow']), toggleBorder),
        ),
      ],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippetWithUtopiaUtilsImport(`
        <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View
            style={{
              backgroundColor: '#DDDDDD',
              boxShadow: UtopiaUtils.shadowAndBorder({
                borderStyle: 'none',
                borderWidth: '1px',
                borderColor: '#000',
              }),
            }}
            layout={{ layoutSystem: 'pinSystem', left: 52, top: 61, width: 256, height: 202 }}
            data-uid={'bbb'}
          />
        </View>
      `),
    )
  })
  it('enables border, sets it to solid from none', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View
            style={{
              backgroundColor: '#DDDDDD',
              boxShadow: UtopiaUtils.shadowAndBorder({
                borderStyle: 'none',
                borderWidth: '1px',
                borderColor: '#000',
              }),
            }}
            layout={{ layoutSystem: 'pinSystem', left: 52, top: 61, width: 256, height: 202 }}
            data-uid={'bbb'}
          />
        </View>
      `),
    )

    await renderResult.dispatch(
      [
        toggleProperty(
          TP.instancePath(TestScenePath, ['aaa', 'bbb']),
          toggleStyleProp(PP.create(['style', 'boxShadow']), toggleBorder),
        ),
      ],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippetWithUtopiaUtilsImport(`
        <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View
            style={{
              backgroundColor: '#DDDDDD',
              boxShadow: UtopiaUtils.shadowAndBorder({
                borderStyle: 'solid',
                borderWidth: '1px',
                borderColor: '#000',
              }),
            }}
            layout={{ layoutSystem: 'pinSystem', left: 52, top: 61, width: 256, height: 202 }}
            data-uid={'bbb'}
          />
        </View>
      `),
    )
  })
  it('adds border when style prop is missing', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
        <View
          layout={{ layoutSystem: 'pinSystem', left: 52, top: 61, width: 256, height: 202 }}
          data-uid={'bbb'}
        />
      </View>
      `),
    )

    await renderResult.dispatch(
      [
        toggleProperty(
          TP.instancePath(TestScenePath, ['aaa', 'bbb']),
          toggleStyleProp(PP.create(['style', 'boxShadow']), toggleBorder),
        ),
      ],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippetWithUtopiaUtilsImport(`
        <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View
            layout={{ layoutSystem: 'pinSystem', left: 52, top: 61, width: 256, height: 202 }}
            data-uid={'bbb'}
            style={{
              boxShadow: UtopiaUtils.shadowAndBorder({
                borderStyle: 'solid',
                borderWidth: '1px',
                borderColor: '#000',
              }),
            }}
          />
        </View>
      `),
    )
  })
  it('adds border when it has only shadow set', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
        <View
          style={{
            backgroundColor: '#DDDDDD',
            boxShadow: UtopiaUtils.shadowAndBorder({ boxShadow: '0px 0px #000, 0px 0px #000' }),
          }}
          layout={{ layoutSystem: 'pinSystem', left: 52, top: 61, width: 256, height: 202 }}
          data-uid={'bbb'}
        />
      </View>
      `),
    )

    await renderResult.dispatch(
      [
        toggleProperty(
          TP.instancePath(TestScenePath, ['aaa', 'bbb']),
          toggleStyleProp(PP.create(['style', 'boxShadow']), toggleBorder),
        ),
      ],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippetWithUtopiaUtilsImport(`
        <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View
            style={{
              backgroundColor: '#DDDDDD',
              boxShadow: UtopiaUtils.shadowAndBorder({
                borderStyle: 'solid',
                borderWidth: '1px',
                borderColor: '#000',
                boxShadow: '0px 0px #000, 0px 0px #000',
              }),
            }}
            layout={{ layoutSystem: 'pinSystem', left: 52, top: 61, width: 256, height: 202 }}
            data-uid={'bbb'}
          />
        </View>
      `),
    )
  })
  it('toggles shadow, comments out boxshadow property values', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View
            style={{
              backgroundColor: '#DDDDDD',
              boxShadow: UtopiaUtils.shadowAndBorder({
                borderStyle: 'solid',
                borderWidth: '1px',
                borderColor: '#000',
                boxShadow: '0px 0px #000, 0px 0px #000',
              }),
            }}
            layout={{ layoutSystem: 'pinSystem', left: 52, top: 61, width: 256, height: 202 }}
            data-uid={'bbb'}
          />
        </View>
      `),
    )

    await renderResult.dispatch(
      [
        toggleProperty(
          TP.instancePath(TestScenePath, ['aaa', 'bbb']),
          toggleStyleProp(PP.create(['style', 'boxShadow']), toggleShadow),
        ),
      ],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippetWithUtopiaUtilsImport(`
        <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View
            style={{
              backgroundColor: '#DDDDDD',
              boxShadow: UtopiaUtils.shadowAndBorder({
                borderStyle: 'solid',
                borderWidth: '1px',
                borderColor: '#000',
                boxShadow: '/*0px 0px #000*/ /*0px 0px #000*/',
              }),
            }}
            layout={{ layoutSystem: 'pinSystem', left: 52, top: 61, width: 256, height: 202 }}
            data-uid={'bbb'}
          />
        </View>
      `),
    )
  })
  it('toggles shadow, after toggle shadow is visible', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View
            style={{
              backgroundColor: '#DDDDDD',
              boxShadow: UtopiaUtils.shadowAndBorder({
                borderStyle: 'solid',
                borderWidth: '1px',
                borderColor: '#000',
                boxShadow: '/*0px 0px #000*/ /*0px 0px #000*/',
              }),
            }}
            layout={{ layoutSystem: 'pinSystem', left: 52, top: 61, width: 256, height: 202 }}
            data-uid={'bbb'}
          />
        </View>
      `),
    )

    await renderResult.dispatch(
      [
        toggleProperty(
          TP.instancePath(TestScenePath, ['aaa', 'bbb']),
          toggleStyleProp(PP.create(['style', 'boxShadow']), toggleShadow),
        ),
      ],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippetWithUtopiaUtilsImport(`
        <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View
            style={{
              backgroundColor: '#DDDDDD',
              boxShadow: UtopiaUtils.shadowAndBorder({
                borderStyle: 'solid',
                borderWidth: '1px',
                borderColor: '#000',
                boxShadow: '0px 0px #000, 0px 0px #000',
              }),
            }}
            layout={{ layoutSystem: 'pinSystem', left: 52, top: 61, width: 256, height: 202 }}
            data-uid={'bbb'}
          />
        </View>
      `),
    )
  })
})
