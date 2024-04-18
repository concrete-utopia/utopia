import { canvasPoint, canvasRectangle } from '../../core/shared/math-utils'
import * as EP from '../../core/shared/element-path'
import { createComplexDefaultProjectContents } from '../../sample-projects/sample-project-utils'
import { contentsToTree } from '../assets'
import type { SaveDOMReport } from '../editor/action-types'
import { setCanvasFrames } from '../editor/actions/action-creators'
import CanvasActions from './canvas-actions'
import { pinFrameChange } from './canvas-types'
import { renderTestEditorWithProjectContent } from './ui-jsx.test-utils'
import { act } from '@testing-library/react'
import { wait } from '../../utils/utils.test-utils'
import { simpleStringifyActions } from '../editor/actions/action-utils'

describe('Dom-walker Caching', () => {
  async function prepareTestProject() {
    const projectContents = createComplexDefaultProjectContents()
    const projectContentsTreeRoot = contentsToTree(projectContents)

    const renderResult = await renderTestEditorWithProjectContent(
      projectContentsTreeRoot,
      'await-first-dom-report',
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
    expect(saveDomReportActions[0].cachedPaths).toEqual([])
    expect(saveDomReportActions[1].cachedPaths).toEqual([])
  })

  it('returns cached metadata for zoom', async () => {
    const renderResult = await prepareTestProject()

    renderResult.clearRecordedActions()

    await renderResult.dispatch([CanvasActions.zoom(2)], true)

    const saveDomReportActions = renderResult
      .getRecordedActions()
      .filter((action): action is SaveDOMReport => action.action === 'SAVE_DOM_REPORT')

    expect(saveDomReportActions.length).toBe(0)
  })

  // FIXME The below two tests have become flaky because of the UPDATE_PROJECT_SERVER_STATE action
  it('resizing an out-of-file element invalidates the cache for only that scene', async () => {
    const renderResult = await prepareTestProject()

    renderResult.clearRecordedActions()

    const pinChange1 = pinFrameChange(
      EP.fromString('storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance'),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
    )
    const pinChange2 = pinFrameChange(
      EP.fromString('storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance'),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 101 }),
    )

    await act(async () => {
      await renderResult.dispatch(
        [
          {
            action: 'SET_ELEMENTS_TO_RERENDER',
            value: [
              EP.fromString(
                'storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance',
              ),
            ],
          },
        ],
        true,
      )
    })

    await act(async () => {
      await renderResult.dispatch([setCanvasFrames([pinChange1], false)], true)
      // Gives a chance for any resize/mutation observers to fire.
      await wait(20)
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()

      await dispatchDone
    })
    await act(async () => {
      await renderResult.dispatch([setCanvasFrames([pinChange2], false)], true)
      // Gives a chance for any resize/mutation observers to fire.
      await wait(20)
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()

      await dispatchDone
    })

    expect(simpleStringifyActions(renderResult.getRecordedActions())).toEqual(`[
  SET_ELEMENTS_TO_RERENDER,
  SET_CANVAS_FRAMES,
  SAVE_DOM_REPORT,
  MERGE_WITH_PREV_UNDO: [
    UPDATE_FROM_WORKER
  ],
  SAVE_DOM_REPORT,
  UPDATE_TOP_LEVEL_ELEMENTS_FROM_COLLABORATION_UPDATE,
  UPDATE_EXPORTS_DETAIL_FROM_COLLABORATION_UPDATE,
  UPDATE_IMPORTS_FROM_COLLABORATION_UPDATE,
  SET_CANVAS_FRAMES,
  SAVE_DOM_REPORT,
  MERGE_WITH_PREV_UNDO: [
    UPDATE_FROM_WORKER
  ],
  SAVE_DOM_REPORT,
  UPDATE_TOP_LEVEL_ELEMENTS_FROM_COLLABORATION_UPDATE,
  UPDATE_EXPORTS_DETAIL_FROM_COLLABORATION_UPDATE,
  UPDATE_IMPORTS_FROM_COLLABORATION_UPDATE
]`)

    const saveDomReportActions = renderResult
      .getRecordedActions()
      .filter((action): action is SaveDOMReport => action.action === 'SAVE_DOM_REPORT')

    expect(saveDomReportActions.length).toBe(4)

    expect(saveDomReportActions[1].invalidatedPaths).toEqual([
      'storyboard-entity',
      'storyboard-entity/scene-1-entity/app-entity:app-outer-div',
      'storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div',
      'storyboard-entity/scene-2-entity/same-file-app-entity:same-file-app-div',
    ])
    expect(saveDomReportActions[1].cachedPaths).toEqual([])

    expect(saveDomReportActions[2].invalidatedPaths).toEqual([
      'storyboard-entity/scene-1-entity',
      'storyboard-entity',
      'storyboard-entity/scene-1-entity/app-entity:app-outer-div',
      'storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div',
      'storyboard-entity/scene-2-entity/same-file-app-entity:same-file-app-div',
    ])
    expect(saveDomReportActions[2].cachedPaths).toEqual([
      EP.fromString('storyboard-entity/scene-2-entity/same-file-app-entity:same-file-app-div'),
      EP.fromString('storyboard-entity/scene-2-entity/same-file-app-entity'),
      EP.fromString('storyboard-entity/scene-2-entity'),
    ])

    expect(saveDomReportActions[3].invalidatedPaths).toEqual([
      'storyboard-entity',
      'storyboard-entity/scene-1-entity/app-entity:app-outer-div',
      'storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div',
      'storyboard-entity/scene-2-entity/same-file-app-entity:same-file-app-div',
    ])
    expect(saveDomReportActions[3].cachedPaths).toEqual([])
  })

  it('resizing an in-file element invalidates the cache for only that scene', async () => {
    const renderResult = await prepareTestProject()

    renderResult.clearRecordedActions()

    const pinChange1 = pinFrameChange(
      EP.fromString('storyboard-entity/scene-2-entity/same-file-app-entity:same-file-app-div'),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
    )
    const pinChange2 = pinFrameChange(
      EP.fromString('storyboard-entity/scene-2-entity/same-file-app-entity:same-file-app-div'),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 101 }),
    )

    await act(async () => {
      await renderResult.dispatch(
        [
          {
            action: 'SET_ELEMENTS_TO_RERENDER',
            value: [
              EP.fromString(
                'storyboard-entity/scene-2-entity/same-file-app-entity:same-file-app-div',
              ),
            ],
          },
        ],
        false,
      )
    })

    await act(async () => {
      await renderResult.dispatch([setCanvasFrames([pinChange1], false)], true)
      // Gives a chance for any resize/mutation observers to fire.
      await wait(20)
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()

      await dispatchDone
    })

    await act(async () => {
      await renderResult.dispatch([setCanvasFrames([pinChange2], false)], true)
      // Gives a chance for any resize/mutation observers to fire.
      await wait(20)
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()

      await dispatchDone
    })

    const saveDomReportActions = renderResult
      .getRecordedActions()
      .filter((action): action is SaveDOMReport => action.action === 'SAVE_DOM_REPORT')

    expect(simpleStringifyActions(renderResult.getRecordedActions())).toEqual(`[
  SET_ELEMENTS_TO_RERENDER,
  SET_CANVAS_FRAMES,
  SAVE_DOM_REPORT,
  MERGE_WITH_PREV_UNDO: [
    UPDATE_FROM_WORKER
  ],
  SAVE_DOM_REPORT,
  UPDATE_TOP_LEVEL_ELEMENTS_FROM_COLLABORATION_UPDATE,
  UPDATE_EXPORTS_DETAIL_FROM_COLLABORATION_UPDATE,
  UPDATE_IMPORTS_FROM_COLLABORATION_UPDATE,
  SET_CANVAS_FRAMES,
  SAVE_DOM_REPORT,
  MERGE_WITH_PREV_UNDO: [
    UPDATE_FROM_WORKER
  ],
  SAVE_DOM_REPORT,
  UPDATE_TOP_LEVEL_ELEMENTS_FROM_COLLABORATION_UPDATE,
  UPDATE_EXPORTS_DETAIL_FROM_COLLABORATION_UPDATE,
  UPDATE_IMPORTS_FROM_COLLABORATION_UPDATE
]`)

    expect(saveDomReportActions.length).toBe(4)

    expect(saveDomReportActions[1].invalidatedPaths).toEqual([
      'storyboard-entity',
      'storyboard-entity/scene-1-entity/app-entity:app-outer-div',
      'storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div',
      'storyboard-entity/scene-2-entity/same-file-app-entity:same-file-app-div',
    ])
    expect(saveDomReportActions[1].cachedPaths).toEqual([])

    expect(saveDomReportActions[2].invalidatedPaths).toEqual([
      'storyboard-entity/scene-2-entity',
      'storyboard-entity',
      'storyboard-entity/scene-1-entity/app-entity:app-outer-div',
      'storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div',
      'storyboard-entity/scene-2-entity/same-file-app-entity:same-file-app-div',
    ])
    expect(saveDomReportActions[2].cachedPaths).toEqual([
      EP.fromString('storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance'),
      EP.fromString('storyboard-entity/scene-1-entity/app-entity:app-outer-div'),
      EP.fromString('storyboard-entity/scene-1-entity/app-entity'),
      EP.fromString('storyboard-entity/scene-1-entity'),
    ])

    expect(saveDomReportActions[3].invalidatedPaths).toEqual([
      'storyboard-entity',
      'storyboard-entity/scene-1-entity/app-entity:app-outer-div',
      'storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div',
      'storyboard-entity/scene-2-entity/same-file-app-entity:same-file-app-div',
    ])
    expect(saveDomReportActions[3].cachedPaths).toEqual([])
  })
})
