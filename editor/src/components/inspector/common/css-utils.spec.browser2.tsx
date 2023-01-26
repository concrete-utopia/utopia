import * as PP from '../../../core/shared/property-path'
import * as EP from '../../../core/shared/element-path'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../../canvas/ui-jsx.test-utils'
import { toggleProperty } from '../../editor/actions/action-creators'
import { toggleBorder, toggleShadow, toggleStylePropPath } from './css-utils'

describe('toggle style prop', () => {
  it('disables border, sets border to none from solid', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...props.style }} data-uid='aaa'>
          <View
          style={{ backgroundColor: '#DDDDDD', border: '1px solid #000', position: 'absolute', left: 52, top: 61, width: 256, height: 202 }}
            data-uid='bbb'
          />
        </View>
      `),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [
        toggleProperty(
          EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
          toggleStylePropPath(PP.create('style', 'border'), toggleBorder),
        ),
      ],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...props.style }} data-uid='aaa'>
          <View
          style={{ backgroundColor: '#DDDDDD', border: '1px #000', position: 'absolute', left: 52, top: 61, width: 256, height: 202 }}
            data-uid='bbb'
          />
        </View>
      `),
    )
  })

  it('enables border, sets it to solid from none', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...props.style }} data-uid='aaa'>
          <View
          style={{ backgroundColor: '#DDDDDD', border: '1px #000', position: 'absolute', left: 52, top: 61, width: 256, height: 202 }}
            data-uid='bbb'
          />
        </View>
      `),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [
        toggleProperty(
          EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
          toggleStylePropPath(PP.create('style', 'border'), toggleBorder),
        ),
      ],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...props.style }} data-uid='aaa'>
          <View
          style={{ backgroundColor: '#DDDDDD', border: '1px solid #000', position: 'absolute', left: 52, top: 61, width: 256, height: 202 }}
            data-uid='bbb'
          />
        </View>
      `),
    )
  })
  it('adds border when style prop is missing', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ position: 'absolute',left: 52, top: 61, width: 256, height: 202 }}
          data-uid='bbb'
        />
      </View>
      `),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [
        toggleProperty(
          EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
          toggleStylePropPath(PP.create('style', 'border'), toggleBorder),
        ),
      ],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{
              position: 'absolute',
              left: 52,
              top: 61,
              width: 256,
              height: 202,
              border: '1px solid #000',
            }}
            data-uid='bbb'
          />
        </View>
      `),
    )
  })

  it('toggles shadow, comments out boxshadow property values', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#DDDDDD', boxShadow: '0px 0px #000, 0px 0px #000', position: 'absolute', left: 52, top: 61, width: 256, height: 202 }}
            data-uid='bbb'
          />
        </View>
      `),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [
        toggleProperty(
          EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
          toggleStylePropPath(PP.create('style', 'boxShadow'), toggleShadow),
        ),
      ],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#DDDDDD', boxShadow: '/*0px 0px #000*/ /*0px 0px #000*/', position: 'absolute', left: 52, top: 61, width: 256, height: 202 }}
            data-uid='bbb'
          />
        </View>
      `),
    )
  })
  it('toggles shadow, after toggle shadow is visible', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#DDDDDD', boxShadow: '/*0px 0px #000*/ /*0px 0px #000*/', position: 'absolute', left: 52, top: 61, width: 256, height: 202 }}
            data-uid='bbb'
          />
        </View>
      `),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [
        toggleProperty(
          EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
          toggleStylePropPath(PP.create('style', 'boxShadow'), toggleShadow),
        ),
      ],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#DDDDDD', boxShadow: '0px 0px #000, 0px 0px #000', position: 'absolute', left: 52, top: 61, width: 256, height: 202 }}
            data-uid='bbb'
          />
        </View>
      `),
    )
  })
})
