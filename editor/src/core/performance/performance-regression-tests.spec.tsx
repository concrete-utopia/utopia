import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  getPrintedUiJsCode,
  TestScenePath,
} from '../../components/canvas/ui-jsx.test-utils'
import { selectComponents, setProp_UNSAFE } from '../../components/editor/actions/action-creators'
import * as TP from '../shared/template-path'
import * as PP from '../shared/property-path'
import { jsxAttributeValue } from '../shared/element-template'

describe('React Render Count Tests - ', () => {
  it('Clicking on opacity slider', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#DDDDDD', opacity: 1 }}
          layout={{ layoutSystem: 'pinSystem', left: 52, top: 61, width: 256, height: 202 }}
          data-uid={'bbb'}
        />
      </View>
      `),
    )
    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa'])], false)],
      false,
    )
    expect(renderResult.renderedDOM.getByText('Opacity')).toBeDefined()

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const renderCountBefore = renderResult.getNumberOfRenders()

    await renderResult.dispatch(
      [
        setProp_UNSAFE(
          TP.instancePath(TestScenePath, ['aaa', 'bbb']),
          PP.create(['style', 'opacity']),
          jsxAttributeValue(0.3),
        ),
      ],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#DDDDDD', opacity: 0.3 }}
          layout={{ layoutSystem: 'pinSystem', left: 52, top: 61, width: 256, height: 202 }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )

    const renderCountAfter = renderResult.getNumberOfRenders()
    expect(renderCountAfter - renderCountBefore).toBeGreaterThan(475) // if this breaks, GREAT NEWS but update the test please :)
    expect(renderCountAfter - renderCountBefore).toBeLessThan(485)
  })

  it('Changing the selected view', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#DDDDDD', opacity: 1 }}
          layout={{ layoutSystem: 'pinSystem', left: 52, top: 61, width: 256, height: 202 }}
          data-uid={'bbb'}
        />
        <View
          style={{ backgroundColor: '#DDDDDD', opacity: 1 }}
          layout={{ layoutSystem: 'pinSystem', left: 152, top: 161, width: 256, height: 202 }}
          data-uid={'ccc'}
        />
      </View>
      `),
    )
    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa'])], false)],
      false,
    )
    expect(renderResult.renderedDOM.getByText('Opacity')).toBeDefined()

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const renderCountBefore = renderResult.getNumberOfRenders()

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa', 'ccc'])], false)],
      false,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#DDDDDD', opacity: 1 }}
          layout={{ layoutSystem: 'pinSystem', left: 52, top: 61, width: 256, height: 202 }}
          data-uid={'bbb'}
        />
        <View
          style={{ backgroundColor: '#DDDDDD', opacity: 1 }}
          layout={{ layoutSystem: 'pinSystem', left: 152, top: 161, width: 256, height: 202 }}
          data-uid={'ccc'}
        />
      </View>`,
      ),
    )

    const renderCountAfter = renderResult.getNumberOfRenders()
    expect(renderCountAfter - renderCountBefore).toBeGreaterThanOrEqual(450) // if this breaks, GREAT NEWS but update the test please :)
    expect(renderCountAfter - renderCountBefore).toBeLessThan(460)
  })
})
