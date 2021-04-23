import { canvasPoint, canvasRectangle } from '../../core/shared/math-utils'
import * as TP from '../../core/shared/template-path'
import { SaveDOMReport } from '../editor/action-types'
import { setCanvasFrames } from '../editor/actions/action-creators'
import CanvasActions from './canvas-actions'
import { pinFrameChange } from './canvas-types'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
  TestStoryboardPath,
} from './ui-jsx.test-utils'

describe('Dom-walker Caching', () => {
  async function prepareTestProject() {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} layout={{ layoutSystem: 'pinSystem' }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256, height: 202 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid='bbb'
        />
      </View>
      `),
    )
    // unfortunately we have to dispatch a non-action to allow the dom-walker to run for a second time.
    // It needs to run for a second time to "settle".
    await renderResult.dispatch([CanvasActions.scrollCanvas(canvasPoint({ x: 0, y: 0 }))], true)
    return renderResult
  }

  it('runs the dom walker for a fresh canvas', async () => {
    const renderResult = await prepareTestProject()

    const saveDomReportActions = renderResult
      .getRecordedActions()
      .filter((action): action is SaveDOMReport => action.action === 'SAVE_DOM_REPORT')

    expect(saveDomReportActions.length).toBe(2)
    expect(saveDomReportActions[0].cachedTreeRoots).toEqual([])
    expect(saveDomReportActions[1].cachedTreeRoots).toEqual([])
  })

  it('returns cached metadata for scroll', async () => {
    const renderResult = await prepareTestProject()

    renderResult.clearRecordedActions()

    await renderResult.dispatch([CanvasActions.scrollCanvas(canvasPoint({ x: 20, y: 30 }))], true)

    const saveDomReportActions = renderResult
      .getRecordedActions()
      .filter((action): action is SaveDOMReport => action.action === 'SAVE_DOM_REPORT')

    expect(saveDomReportActions.length).toBe(1)
    expect(saveDomReportActions[0].cachedTreeRoots).toEqual([TestStoryboardPath])
  })

  it('resizing an element invalidates the cache', async () => {
    const renderResult = await prepareTestProject()

    renderResult.clearRecordedActions()

    const pinChange = pinFrameChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    const saveDomReportActions = renderResult
      .getRecordedActions()
      .filter((action): action is SaveDOMReport => action.action === 'SAVE_DOM_REPORT')

    expect(saveDomReportActions.length).toBe(1)
    expect(saveDomReportActions[0].cachedTreeRoots).not.toEqual([])
  })
})
