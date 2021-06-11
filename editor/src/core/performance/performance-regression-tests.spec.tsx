import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  getPrintedUiJsCode,
  TestScenePath,
} from '../../components/canvas/ui-jsx.test-utils'
import { selectComponents, setProp_UNSAFE } from '../../components/editor/actions/action-creators'
import * as EP from '../shared/element-path'
import * as PP from '../shared/property-path'
import { jsxAttributeValue } from '../shared/element-template'
import { emptyComments } from '../workers/parser-printer/parser-printer-comments'

describe('React Render Count Tests - ', () => {
  it('Clicking on opacity slider', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#DDDDDD', position: 'absolute', opacity: 1, left: 52, top: 61, width: 256, height: 202 }}
          data-uid='bbb'
        />
      </View>
      `),
    )
    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa'])], false)],
      false,
    )
    expect(renderResult.renderedDOM.getByText('Opacity')).toBeDefined()

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const renderCountBefore = renderResult.getNumberOfRenders()

    await renderResult.dispatch(
      [
        setProp_UNSAFE(
          EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
          PP.create(['style', 'opacity']),
          jsxAttributeValue(0.3, emptyComments),
        ),
      ],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#DDDDDD', position: 'absolute', opacity: 0.3, left: 52, top: 61, width: 256, height: 202 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )

    const renderCountAfter = renderResult.getNumberOfRenders()
    expect(renderCountAfter - renderCountBefore).toBeGreaterThan(280) // if this breaks, GREAT NEWS but update the test please :)
    expect(renderCountAfter - renderCountBefore).toBeLessThan(300)
  })

  it('Changing the selected view', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#DDDDDD', position: 'absolute', opacity: 1, left: 52, top: 61, width: 256, height: 202 }}
          data-uid='bbb'
        />
        <View
          style={{ backgroundColor: '#DDDDDD', position: 'absolute', opacity: 1, left: 152, top: 161, width: 256, height: 202 }}
          data-uid='ccc'
        />
      </View>
      `),
    )
    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa'])], false)],
      false,
    )
    expect(renderResult.renderedDOM.getByText('Opacity')).toBeDefined()

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const renderCountBefore = renderResult.getNumberOfRenders()

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc'])], false)],
      false,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#DDDDDD', position: 'absolute', opacity: 1, left: 52, top: 61, width: 256, height: 202 }}
          data-uid='bbb'
        />
        <View
          style={{ backgroundColor: '#DDDDDD', position: 'absolute', opacity: 1, left: 152, top: 161, width: 256, height: 202 }}
          data-uid='ccc'
        />
      </View>`,
      ),
    )

    const renderCountAfter = renderResult.getNumberOfRenders()
    expect(renderCountAfter - renderCountBefore).toBeGreaterThanOrEqual(460) // if this breaks, GREAT NEWS but update the test please :)
    expect(renderCountAfter - renderCountBefore).toBeLessThan(470)
  })
})
