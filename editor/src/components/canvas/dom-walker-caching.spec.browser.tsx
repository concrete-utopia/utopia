import { canvasPoint, canvasRectangle } from '../../core/shared/math-utils'
import * as EP from '../../core/shared/element-path'
import { createComplexDefaultProjectContents } from '../../sample-projects/sample-project-utils'
import { contentsToTree } from '../assets'
import { SaveDOMReport } from '../editor/action-types'
import { setCanvasFrames } from '../editor/actions/action-creators'
import CanvasActions from './canvas-actions'
import { pinFrameChange } from './canvas-types'
import { renderTestEditorWithProjectContent } from './ui-jsx.test-utils'
import { act } from 'react-test-renderer'

describe('Dom-walker Caching', () => {
  async function prepareTestProject() {
    const projectContents = createComplexDefaultProjectContents()
    const projectContentsTreeRoot = contentsToTree(projectContents)

    const renderResult = await renderTestEditorWithProjectContent(projectContentsTreeRoot)
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

    expect(saveDomReportActions.length).toBe(3)
    expect(saveDomReportActions[0].cachedPaths).toEqual([])
    expect(saveDomReportActions[1].cachedPaths).toEqual([])
    expect(saveDomReportActions[2].cachedPaths).toEqual([EP.fromString(':storyboard-entity')])
  })

  it('returns cached metadata for scroll', async () => {
    const renderResult = await prepareTestProject()

    renderResult.clearRecordedActions()

    await renderResult.dispatch([CanvasActions.scrollCanvas(canvasPoint({ x: 20, y: 30 }))], true)

    const saveDomReportActions = renderResult
      .getRecordedActions()
      .filter((action): action is SaveDOMReport => action.action === 'SAVE_DOM_REPORT')

    expect(saveDomReportActions.length).toBe(1)
    expect(saveDomReportActions[0].cachedPaths).toEqual([EP.fromString(':storyboard-entity')])
  })

  it('resizing an out-of-file element invalidates the cache for only that scene', async () => {
    const renderResult = await prepareTestProject()

    renderResult.clearRecordedActions()

    const pinChange = pinFrameChange(
      EP.fromString('storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance'),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
    )

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

      await domFinished
      await dispatchDone
    })

    const saveDomReportActions = renderResult
      .getRecordedActions()
      .filter((action): action is SaveDOMReport => action.action === 'SAVE_DOM_REPORT')

    expect(saveDomReportActions.length).toBe(2)
    expect(saveDomReportActions[0].cachedPaths).toEqual([EP.fromString(':storyboard-entity')])
    expect(saveDomReportActions[1].cachedPaths).toEqual([
      EP.fromString(':storyboard-entity/scene-2-entity/same-file-app-entity:same-file-app-div'),
      EP.fromString(':storyboard-entity/scene-2-entity/same-file-app-entity'),
      EP.fromString(':storyboard-entity/scene-2-entity'),
    ])
  })

  it('resizing an in-file element invalidates the cache for only that scene', async () => {
    const renderResult = await prepareTestProject()

    renderResult.clearRecordedActions()

    const pinChange = pinFrameChange(
      EP.fromString('storyboard-entity/scene-2-entity/same-file-app-entity:same-file-app-div'),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
    )

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

      await domFinished
      await dispatchDone
    })

    const saveDomReportActions = renderResult
      .getRecordedActions()
      .filter((action): action is SaveDOMReport => action.action === 'SAVE_DOM_REPORT')

    expect(saveDomReportActions.length).toBe(2)
    expect(saveDomReportActions[0].cachedPaths).toEqual([EP.fromString(':storyboard-entity')])
    expect(saveDomReportActions[1].cachedPaths).toEqual([
      EP.fromString(':storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance'),
      EP.fromString(':storyboard-entity/scene-1-entity/app-entity:app-outer-div'),
      EP.fromString(':storyboard-entity/scene-1-entity/app-entity'),
      EP.fromString(':storyboard-entity/scene-1-entity'),
    ])
  })
})
