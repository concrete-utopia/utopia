import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  getPrintedUiJsCode,
  TestScenePath,
  makeTestProjectCodeWithComponentInnards,
} from '../../components/canvas/ui-jsx.test-utils'
import { selectComponents, setProp_UNSAFE } from '../../components/editor/actions/action-creators'
import * as EP from '../shared/element-path'
import * as PP from '../shared/property-path'
import { emptyComments, jsExpressionValue } from '../shared/element-template'

jest.mock('../../components/canvas/controls/outline-utils', () => ({
  isZeroSizedElement: () => false, // in test environment elements have no size
}))

describe('React Render Count Tests -', () => {
  it('Clicking on opacity slider with a simple project', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='parent'>
        <View
          style={{ backgroundColor: '#DDDDDD', position: 'absolute', opacity: 1, left: 52, top: 61, width: 256, height: 202 }}
          data-uid='bbb'
        />
      </View>
      `),
      'dont-await-first-dom-report',
    )
    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['parent'])], false)],
      false,
    )
    expect(renderResult.renderedDOM.getByText('Opacity')).toBeDefined()

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['parent', 'bbb'])], false)],
      false,
    )

    const renderCountBefore = renderResult.getNumberOfRenders()
    renderResult.clearRenderInfo()

    await renderResult.dispatch(
      [
        setProp_UNSAFE(
          EP.appendNewElementPath(TestScenePath, ['parent', 'bbb']),
          PP.create('style', 'opacity'),
          jsExpressionValue(0.3, emptyComments),
        ),
      ],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='parent'>
        <View
          style={{ backgroundColor: '#DDDDDD', position: 'absolute', opacity: 0.3, left: 52, top: 61, width: 256, height: 202 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )

    const renderCountAfter = renderResult.getNumberOfRenders()
    // if this breaks, GREAT NEWS but update the test please :)
    expect(renderCountAfter - renderCountBefore).toMatchInlineSnapshot(`815`)
    expect(renderResult.getRenderInfo()).toMatchSnapshot()
  })

  it('Clicking on opacity slider with a less simple project', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithComponentInnards(`
      const [i, setI] = React.useState(0)
      return (
        <View style={{ ...props.style }} data-uid='parent'>
          <View
            style={{ backgroundColor: '#DDDDDD', position: 'absolute', opacity: 1, left: 52, top: 61, width: 256, height: 202 }}
            data-uid='child'
            onMouseDown={() => setI((current) => current + 1)}
          />
        </View>
      )
      `),
      'dont-await-first-dom-report',
    )
    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['parent'])], false)],
      false,
    )
    expect(renderResult.renderedDOM.getByText('Opacity')).toBeDefined()

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['parent', 'child'])], false)],
      false,
    )

    const renderCountBefore = renderResult.getNumberOfRenders()
    renderResult.clearRenderInfo()

    await renderResult.dispatch(
      [
        setProp_UNSAFE(
          EP.appendNewElementPath(TestScenePath, ['parent', 'child']),
          PP.create('style', 'opacity'),
          jsExpressionValue(0.3, emptyComments),
        ),
      ],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithComponentInnards(`
      const [i, setI] = React.useState(0)
      return (
        <View style={{ ...props.style }} data-uid='parent'>
          <View
            style={{ backgroundColor: '#DDDDDD', position: 'absolute', opacity: 0.3, left: 52, top: 61, width: 256, height: 202 }}
            data-uid='child'
            onMouseDown={() => setI((current) => current + 1)}
          />
        </View>
      )
      `),
    )

    const renderCountAfter = renderResult.getNumberOfRenders()
    // if this breaks, GREAT NEWS but update the test please :)
    expect(renderCountAfter - renderCountBefore).toMatchInlineSnapshot(`1163`)
    expect(renderResult.getRenderInfo()).toMatchSnapshot()
  })

  it('Changing the selected view with a simple project', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='parent'>
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
      'dont-await-first-dom-report',
    )
    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['parent'])], false)],
      false,
    )
    expect(renderResult.renderedDOM.getByText('Opacity')).toBeDefined()

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['parent', 'bbb'])], false)],
      false,
    )

    const renderCountBefore = renderResult.getNumberOfRenders()
    renderResult.clearRenderInfo()

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['parent', 'ccc'])], false)],
      false,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='parent'>
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
    // if this breaks, GREAT NEWS but update the test please :)
    expect(renderCountAfter - renderCountBefore).toMatchInlineSnapshot(`537`)
    expect(renderResult.getRenderInfo()).toMatchSnapshot()
  })

  it('Changing the selected view with a less simple project', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithComponentInnards(`
      const [i, setI] = React.useState(0)
      return (
        <View style={{ ...props.style }} data-uid='parent'>
          <View
            style={{ backgroundColor: '#DDDDDD', position: 'absolute', opacity: 1, left: 52, top: 61, width: 256, height: 202 }}
            data-uid='bbb'
            onMouseDown={() => setI((current) => current + 1)}
          />
          <View
            style={{ backgroundColor: '#DDDDDD', position: 'absolute', opacity: 1, left: 152, top: 161, width: 256, height: 202 }}
            data-uid='ccc'
            onMouseDown={() => setI((current) => current + 1)}
          />
        </View>
      )
      `),
      'dont-await-first-dom-report',
    )
    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['parent'])], false)],
      false,
    )
    expect(renderResult.renderedDOM.getByText('Opacity')).toBeDefined()

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['parent', 'bbb'])], false)],
      false,
    )

    const renderCountBefore = renderResult.getNumberOfRenders()
    renderResult.clearRenderInfo()

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['parent', 'ccc'])], false)],
      false,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithComponentInnards(`
      const [i, setI] = React.useState(0)
      return (
        <View style={{ ...props.style }} data-uid='parent'>
          <View
            style={{ backgroundColor: '#DDDDDD', position: 'absolute', opacity: 1, left: 52, top: 61, width: 256, height: 202 }}
            data-uid='bbb'
            onMouseDown={() => setI((current) => current + 1)}
          />
          <View
            style={{ backgroundColor: '#DDDDDD', position: 'absolute', opacity: 1, left: 152, top: 161, width: 256, height: 202 }}
            data-uid='ccc'
            onMouseDown={() => setI((current) => current + 1)}
          />
        </View>
      )
      `),
    )

    const renderCountAfter = renderResult.getNumberOfRenders()
    // if this breaks, GREAT NEWS but update the test please :)
    expect(renderCountAfter - renderCountBefore).toMatchInlineSnapshot(`710`)
    expect(renderResult.getRenderInfo()).toMatchSnapshot()
  })
})
