import * as PP from '../../../core/shared/property-path'
import * as TP from '../../../core/shared/template-path'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../../canvas/ui-jsx-test-utils'
import { toggleProperty } from '../../editor/actions/actions'
import { toggleBorder, toggleShadow, toggleStyleProp } from './css-utils'

describe('toggle style prop', () => {
  it('disables border, sets border to none from solid', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View
          style={{ backgroundColor: '#DDDDDD', border: '1px solid #000' }}
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
          toggleStyleProp(PP.create(['style', 'border']), toggleBorder),
        ),
      ],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View
          style={{ backgroundColor: '#DDDDDD', border: '1px #000' }}
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
          style={{ backgroundColor: '#DDDDDD', border: '1px #000' }}
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
          toggleStyleProp(PP.create(['style', 'border']), toggleBorder),
        ),
      ],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View
          style={{ backgroundColor: '#DDDDDD', border: '1px solid #000' }}
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
          toggleStyleProp(PP.create(['style', 'border']), toggleBorder),
        ),
      ],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View
            layout={{ layoutSystem: 'pinSystem', left: 52, top: 61, width: 256, height: 202 }}
            data-uid={'bbb'}
            style={{ border: '1px solid #000' }}
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
            style={{ backgroundColor: '#DDDDDD', boxShadow: '0px 0px #000, 0px 0px #000' }}
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
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#DDDDDD', boxShadow: '/*0px 0px #000*/ /*0px 0px #000*/' }}
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
            style={{ backgroundColor: '#DDDDDD', boxShadow: '/*0px 0px #000*/ /*0px 0px #000*/' }}
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
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#DDDDDD', boxShadow: '0px 0px #000, 0px 0px #000' }}
            layout={{ layoutSystem: 'pinSystem', left: 52, top: 61, width: 256, height: 202 }}
            data-uid={'bbb'}
          />
        </View>
      `),
    )
  })
})
