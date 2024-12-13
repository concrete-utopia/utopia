import React, { useRef } from 'react'
import * as ReactDOM from 'react-dom'
import CanvasActions from '../../components/canvas/canvas-actions'
import type { DebugDispatch } from '../../components/editor/action-types'
import { DispatchPriority, EditorAction } from '../../components/editor/action-types'
import {
  clearSelection,
  deleteView,
  selectComponents,
  setFocusedElement,
  setProp_UNSAFE,
  switchEditorMode,
  unsetProperty,
  updateEditorMode,
} from '../../components/editor/actions/action-creators'
import {
  Substores,
  useEditorState,
  useRefEditorState,
} from '../../components/editor/store/store-hook'
import {
  canvasPoint,
  CanvasRectangle,
  CanvasVector,
  isInfinityRectangle,
  windowPoint,
  zeroPoint,
  zeroRectangle,
} from '../shared/math-utils'
import { CanvasContainerID } from '../../components/canvas/canvas-types'
import { MetadataUtils } from './element-metadata-utils'
import * as EP from '../shared/element-path'
import * as PP from '../shared/property-path'
import { EditorModes } from '../../components/editor/editor-modes'
import {
  useCalculateHighlightedViews,
  useGetSelectableViewsForSelectMode,
} from '../../components/canvas/controls/select-mode/select-mode-hooks'
import { CanvasControlsContainerID } from '../../components/canvas/controls/new-canvas-controls'
import { forceNotNull } from '../shared/optional-utils'
import { ElementPathArrayKeepDeepEquality } from '../../utils/deep-equality-instances'
import { NavigatorContainerId } from '../../components/navigator/navigator'
import { emptyComments, jsExpressionValue } from '../shared/element-template'
import { last } from '../shared/array-utils'
import { load } from '../../components/editor/actions/actions'
import type { ProjectContentTreeRoot } from '../../components/assets'
import type {
  EditorStorePatched,
  PersistentModel,
} from '../../components/editor/store/editor-state'
import { regularNavigatorEntryOptic } from '../../components/editor/store/editor-state'
import { CURRENT_PROJECT_VERSION } from '../../components/editor/actions/migrations/migrations'
import type { BuiltInDependencies } from '../es-modules/package-manager/built-in-dependencies-list'
import { LargeProjectContents } from '../../test-cases/large-project'
import { v4 as UUID } from 'uuid'
import { SmallSingleDivProjectContents } from '../../test-cases/simple-single-div-project'
import { useDispatch } from '../../components/editor/store/dispatch-context'
import { Optic } from '../shared/optics/optics'
import { ElementPath } from '../shared/project-file-types'
import { fromField, traverseArray } from '../shared/optics/optic-creators'
import { toArrayOf } from '../shared/optics/optic-utilities'

let NumberOfIterations = 5
if (window != null) {
  // we are exposing this function on window so it can be called from Puppeteer
  ;(window as any).SetPerformanceScriptNumberOfIterations = (value: number) => {
    NumberOfIterations = value
    return NumberOfIterations
  }
}

export function wait(timeout: number): Promise<void> {
  return new Promise((resolve) => {
    setTimeout(resolve, timeout)
  })
}

function markStart(prefix: string, framesPassed: number): void {
  performance.mark(`${prefix}_start_${framesPassed}`)
}

function markEnd(prefix: string, framesPassed: number): void {
  performance.mark(`${prefix}_end_${framesPassed}`)
}

function measureStep(prefix: string, framesPassed: number): void {
  performance.measure(
    `${prefix}_step_${framesPassed}`,
    `${prefix}_start_${framesPassed}`,
    `${prefix}_end_${framesPassed}`,
  )
}

const CANVAS_POPULATE_WAIT_TIME_MS = 20 * 1000

async function loadProject(
  dispatch: DebugDispatch,
  builtInDependencies: BuiltInDependencies,
  projectContents: ProjectContentTreeRoot,
  projectId: string | null,
): Promise<boolean> {
  const persistentModel: PersistentModel = {
    appID: null,
    forkedFromProjectId: null,
    projectVersion: CURRENT_PROJECT_VERSION,
    projectDescription: 'Performance Test Project',
    projectContents: projectContents,
    exportsInfo: [],
    lastUsedFont: null,
    hiddenInstances: [],
    codeEditorErrors: {
      buildErrors: {},
      lintErrors: {},
      componentDescriptorErrors: {},
    },
    fileBrowser: {
      minimised: false,
    },
    dependencyList: {
      minimised: false,
    },
    projectSettings: {
      minimised: false,
    },
    navigator: {
      minimised: false,
    },
    githubSettings: {
      targetRepository: null,
      originCommit: null,
      branchName: null,
      pendingCommit: null,
      branchLoaded: false,
    },
    colorSwatches: [],
  }

  const projectIdToUse = forceNotNull('project does not have an ID', projectId)

  // Load the project itself.
  await load(dispatch, persistentModel, 'Test', projectIdToUse, builtInDependencies, false)

  // Wait for the editor to stabilise, ensuring that the canvas can render for example.
  const startWaitingTime = Date.now()
  let editorReady: boolean = false
  let canvasPopulated: boolean = false
  while (startWaitingTime + CANVAS_POPULATE_WAIT_TIME_MS > Date.now() && !editorReady) {
    // Check canvas has been populated.
    if (!canvasPopulated) {
      const canvasContainerElement = document.getElementById(CanvasContainerID)
      if (canvasContainerElement != null) {
        if (canvasContainerElement.children.length > 0) {
          canvasPopulated = true
        }
      }
    }

    // Select _something_ to trigger the code editor.
    //if (codeEditorLoaded) {
    const itemLabelContainer = document.querySelector(`div[class~="item-label-container"]`)
    if (itemLabelContainer != null) {
      if (itemLabelContainer instanceof HTMLElement) {
        itemLabelContainer.click()
      }
    }
    //}

    // Appears the code editor can't be relied on to load enough of the time for
    // this check to not break everything.
    editorReady = canvasPopulated

    if (!editorReady) {
      await wait(500)
    }
  }

  // Give the editor a little bit of an extra window of time just in case.
  if (editorReady) {
    await wait(2000)
  }
  return editorReady
}

export function useTriggerScrollPerformanceTest(): () => void {
  const dispatch = useDispatch() as DebugDispatch
  const builtInDependencies = useEditorState(
    Substores.restOfStore,
    (store) => store.builtInDependencies,
    'useTriggerScrollPerformanceTest builtInDependencies',
  )
  const projectId = useEditorState(
    Substores.fullStore,
    (store) => store.editor.id,
    'useTriggerScrollPerformanceTest id',
  )
  const allPaths = useRefEditorState((store) =>
    MetadataUtils.getAllPaths(store.editor.jsxMetadata, store.editor.elementPathTree),
  )
  const trigger = React.useCallback(async () => {
    const editorReady = await loadProject(
      dispatch,
      builtInDependencies,
      LargeProjectContents,
      projectId,
    )
    if (!editorReady) {
      console.info('SCROLL_TEST_ERROR - editor not ready')
      return
    }

    if (allPaths.current.length === 0) {
      console.info('SCROLL_TEST_ERROR - allPaths.current.length === 0')
      return
    }

    const targetPath = [...allPaths.current].sort(
      (a, b) => EP.toString(b).length - EP.toString(a).length,
    )[0]

    await dispatch([selectComponents([targetPath!], false)]).entireUpdateFinished

    let framesPassed = 0
    async function step() {
      markStart('scroll', framesPassed)
      await dispatch([CanvasActions.scrollCanvas(canvasPoint({ x: -5, y: -1 }))])
        .entireUpdateFinished
      markEnd('scroll', framesPassed)
      measureStep('scroll', framesPassed)
      if (framesPassed < NumberOfIterations) {
        framesPassed++
        requestAnimationFrame(step)
      } else {
        console.info('SCROLL_TEST_FINISHED')
      }
    }
    requestAnimationFrame(step)
  }, [dispatch, builtInDependencies, projectId, allPaths])
  return trigger
}

function useTriggerHighlightPerformanceTest(key: 'regular' | 'all-elements'): () => void {
  const allPaths = useRefEditorState((store) =>
    MetadataUtils.getAllPaths(store.editor.jsxMetadata, store.editor.elementPathTree),
  )
  const getHighlightableViews = useGetSelectableViewsForSelectMode()
  const calculateHighlightedViews = useCalculateHighlightedViews(true, getHighlightableViews)
  const dispatch = useDispatch() as DebugDispatch
  const builtInDependencies = useEditorState(
    Substores.restOfStore,
    (store) => store.builtInDependencies,
    'useTriggerHighlightPerformanceTest builtInDependencies',
  )
  const projectId = useEditorState(
    Substores.fullStore,
    (store) => store.editor.id,
    'useTriggerScrollPerformanceTest id',
  )
  const trigger = React.useCallback(async () => {
    const allCapsKey = key.toLocaleUpperCase()
    const editorReady = await loadProject(
      dispatch,
      builtInDependencies,
      LargeProjectContents,
      projectId,
    )
    if (!editorReady) {
      console.info(`HIGHLIGHT_${allCapsKey}_TEST_ERROR - editor not ready`)
      return
    }
    if (allPaths.current.length === 0) {
      console.info(`HIGHLIGHT_${allCapsKey}_TEST_ERROR_NO_PATHS`)
      return
    }

    const targetPath = allPaths.current[0]

    const htmlElement = document.querySelector(`*[data-path^="${EP.toString(targetPath)}"]`)
    if (htmlElement == null) {
      console.info(`HIGHLIGHT_${allCapsKey}_TEST_ERROR_NO_ELEMENT`)
      return
    }

    const elementBounds = htmlElement.getBoundingClientRect()

    let framesPassed = 0
    async function step() {
      markStart(`highlight_${key}`, framesPassed)

      calculateHighlightedViews(
        windowPoint({ x: elementBounds.left + 10, y: elementBounds.top + 10 }),
        key === 'all-elements',
      )

      // Clear the highlight before the next run
      calculateHighlightedViews(
        windowPoint({ x: elementBounds.left - 100, y: elementBounds.top - 100 }),
        key === 'all-elements',
      )
      markEnd(`highlight_${key}`, framesPassed)
      measureStep(`highlight_${key}`, framesPassed)

      if (framesPassed < NumberOfIterations) {
        framesPassed++
        requestAnimationFrame(step)
      } else {
        console.info(`HIGHLIGHT_${allCapsKey}_TEST_FINISHED`)
      }
    }
    requestAnimationFrame(step)
  }, [key, dispatch, builtInDependencies, projectId, allPaths, calculateHighlightedViews])

  return trigger
}

export const useTriggerRegularHighlightPerformanceTest = () =>
  useTriggerHighlightPerformanceTest('regular')

export const useTriggerAllElementsHighlightPerformanceTest = () =>
  useTriggerHighlightPerformanceTest('all-elements')

export function useTriggerSelectionPerformanceTest(): () => void {
  const dispatch = useDispatch() as DebugDispatch
  const allPaths = useRefEditorState((store) =>
    MetadataUtils.getAllPaths(store.editor.jsxMetadata, store.editor.elementPathTree),
  )
  const selectedViews = useRefEditorState((store) => store.editor.selectedViews)
  const builtInDependencies = useEditorState(
    Substores.restOfStore,
    (store) => store.builtInDependencies,
    'useTriggerSelectionPerformanceTest builtInDependencies',
  )
  const projectId = useEditorState(
    Substores.fullStore,
    (store) => store.editor.id,
    'useTriggerScrollPerformanceTest id',
  )
  const trigger = React.useCallback(async () => {
    const editorReady = await loadProject(
      dispatch,
      builtInDependencies,
      LargeProjectContents,
      projectId,
    )
    if (!editorReady) {
      console.info('SELECT_TEST_ERROR - editor not ready')
      return
    }
    const targetPath = [...allPaths.current].sort(
      (a, b) => EP.toString(b).length - EP.toString(a).length,
    )[0]
    // Determine where the events should be fired.
    const controlsContainerElement = forceNotNull(
      'Container controls element should exist.',
      document.getElementById(CanvasControlsContainerID),
    )
    const canvasContainerElement = forceNotNull(
      'Canvas container element should exist.',
      document.getElementById(CanvasContainerID),
    )
    const canvasContainerBounds = canvasContainerElement.getBoundingClientRect()
    const navigatorElement = forceNotNull(
      'Navigator element should exist.',
      document.getElementById(NavigatorContainerId),
    )
    const navigatorBounds = navigatorElement.getBoundingClientRect()

    const targetElement = forceNotNull(
      'Target element should exist.',
      document.querySelector(`*[data-path^="${EP.toString(targetPath)}"]`),
    )
    const originalTargetBounds = targetElement.getBoundingClientRect()
    const leftToTarget =
      canvasContainerBounds.left + navigatorBounds.width - originalTargetBounds.left + 100
    const topToTarget = canvasContainerBounds.top - originalTargetBounds.top + 100
    await dispatch(
      [CanvasActions.positionCanvas(canvasPoint({ x: leftToTarget, y: topToTarget }))],
      'everyone',
    ).entireUpdateFinished
    const targetBounds = targetElement.getBoundingClientRect()
    if (allPaths.current.length === 0) {
      console.info('SELECT_TEST_ERROR - allPaths.current.length === 0')
      return
    }

    let framesPassed = 0
    async function step() {
      markStart('select', framesPassed)
      controlsContainerElement.dispatchEvent(
        new MouseEvent('mousedown', {
          detail: 1,
          bubbles: true,
          cancelable: true,
          metaKey: true,
          clientX: targetBounds.left + 5,
          clientY: targetBounds.top + 5,
          buttons: 1,
        }),
      )
      function isTargetSelected(): boolean {
        return ElementPathArrayKeepDeepEquality([targetPath], selectedViews.current).areEqual
      }
      const startingTime = Date.now()
      while (!isTargetSelected() && Date.now() < startingTime + 3000) {
        await wait(5)
      }
      if (!isTargetSelected()) {
        throw new Error(`Element never ended up being selected.`)
      }
      controlsContainerElement.dispatchEvent(
        new MouseEvent('pointerup', {
          detail: 1,
          bubbles: true,
          cancelable: true,
          metaKey: true,
          clientX: targetBounds.left + 5,
          clientY: targetBounds.top + 5,
          buttons: 1,
        }),
      )
      controlsContainerElement.dispatchEvent(
        new MouseEvent('mouseup', {
          detail: 1,
          bubbles: true,
          cancelable: true,
          metaKey: true,
          clientX: targetBounds.left + 5,
          clientY: targetBounds.top + 5,
          buttons: 1,
        }),
      )
      markEnd('select', framesPassed)
      measureStep('select', framesPassed)

      markStart('select_deselect', framesPassed)
      await dispatch([clearSelection()]).entireUpdateFinished
      markEnd('select_deselect', framesPassed)
      measureStep('select_deselect', framesPassed)

      if (framesPassed < NumberOfIterations) {
        framesPassed++
        requestAnimationFrame(step)
      } else {
        console.info('SELECT_TEST_FINISHED')
      }
    }
    requestAnimationFrame(step)
  }, [dispatch, builtInDependencies, projectId, allPaths, selectedViews])
  return trigger
}

export const useTriggerAbsoluteMoveLargePerformanceTest = () =>
  useTriggerAbsoluteMovePerformanceTest(LargeProjectContents)

export const useTriggerAbsoluteMoveSmallPerformanceTest = () =>
  useTriggerAbsoluteMovePerformanceTest(SmallSingleDivProjectContents)

export function useTriggerAbsoluteMovePerformanceTest(
  projectContents: ProjectContentTreeRoot,
): () => void {
  const dispatch = useDispatch() as DebugDispatch
  const allPaths = useRefEditorState((store) =>
    MetadataUtils.getAllPaths(store.editor.jsxMetadata, store.editor.elementPathTree),
  )
  const metadata = useRefEditorState(React.useCallback((store) => store.editor.jsxMetadata, []))
  const builtInDependencies = useEditorState(
    Substores.restOfStore,
    (store) => store.builtInDependencies,
    'useTriggerAbsoluteMovePerformanceTest builtInDependencies',
  )
  const projectId = useEditorState(
    Substores.fullStore,
    (store) => store.editor.id,
    'useTriggerScrollPerformanceTest id',
  )
  const trigger = React.useCallback(async () => {
    const editorReady = await loadProject(dispatch, builtInDependencies, projectContents, projectId)
    if (!editorReady) {
      console.info('ABSOLUTE_MOVE_TEST_ERROR - editor not ready')
      return
    }
    const initialTargetPath = [...allPaths.current].sort(
      (a, b) => EP.toString(b).length - EP.toString(a).length,
    )[0]
    // This is very particularly tied to the test project in LargeProjectContents, we _really_ need
    // to pick the right element because our changes can cause other elements to end up on top of the
    // target we want.
    const parentParentPath = EP.parentPath(EP.parentPath(initialTargetPath))
    const grandChildrenPaths = allPaths.current.filter((path) => {
      return EP.pathsEqual(parentParentPath, EP.parentPath(EP.parentPath(path)))
    })
    if (grandChildrenPaths.length === 0) {
      console.info('ABSOLUTE_MOVE_TEST_ERROR - grandChildrenPaths.length === 0')
      return
    }
    const targetPath = forceNotNull('Invalid array.', last(grandChildrenPaths))

    // Delete the other children that just get in the way.
    const parentPath = EP.parentPath(targetPath)
    const siblingPaths = allPaths.current.filter(
      (path) => EP.isChildOf(path, parentPath) && !EP.pathsEqual(path, targetPath),
    )
    await dispatch(
      siblingPaths.map((path) => deleteView(path)),
      'everyone',
    ).entireUpdateFinished
    // Focus the target so that we can edit the child div inside it.
    await dispatch([setFocusedElement(targetPath)], 'everyone').entireUpdateFinished
    const childTargetPath = allPaths.current.find((path) => EP.isChildOf(path, targetPath))
    if (childTargetPath == null) {
      console.info('ABSOLUTE_MOVE_TEST_ERROR - childTargetPath == null')
      return
    }
    const childMetadata = MetadataUtils.findElementByElementPath(metadata.current, childTargetPath)
    if (
      childMetadata == null ||
      childMetadata.globalFrame == null ||
      isInfinityRectangle(childMetadata.globalFrame) ||
      childMetadata.specialSizeMeasurements.coordinateSystemBounds == null
    ) {
      console.info('ABSOLUTE_MOVE_TEST_ERROR - child metadata does not exist')
      return
    }
    const childStyleValue = {
      position: 'absolute',
      left:
        childMetadata.globalFrame.x -
        childMetadata.specialSizeMeasurements.coordinateSystemBounds.x,
      top:
        childMetadata.globalFrame.y -
        childMetadata.specialSizeMeasurements.coordinateSystemBounds.y,
      width: childMetadata.globalFrame.width,
      height: childMetadata.globalFrame.height,
    }

    // Determine where the events should be fired.
    const controlsContainerElement = forceNotNull(
      'Container controls element should exist.',
      document.getElementById(CanvasControlsContainerID),
    )
    const canvasContainerElement = forceNotNull(
      'Canvas container element should exist.',
      document.getElementById(CanvasContainerID),
    )
    const canvasContainerBounds = canvasContainerElement.getBoundingClientRect()
    const navigatorElement = forceNotNull(
      'Navigator element should exist.',
      document.getElementById(NavigatorContainerId),
    )
    const navigatorBounds = navigatorElement.getBoundingClientRect()

    const targetElement = forceNotNull(
      'Target element should exist.',
      document.querySelector(`*[data-path^="${EP.toString(childTargetPath)}"]`),
    )
    const originalTargetBounds = targetElement.getBoundingClientRect()
    const leftToTarget =
      canvasContainerBounds.left + navigatorBounds.width - originalTargetBounds.left + 100
    const topToTarget = canvasContainerBounds.top - originalTargetBounds.top + 100
    await dispatch(
      [
        updateEditorMode(EditorModes.selectMode(null, false, 'none')),
        CanvasActions.positionCanvas(canvasPoint({ x: leftToTarget, y: topToTarget })),
      ],
      'everyone',
    ).entireUpdateFinished
    const targetBounds = targetElement.getBoundingClientRect()

    let framesPassed = 0
    async function step() {
      // Make the div inside the target absolute positioned and ensure it is selected.
      await dispatch(
        [
          selectComponents([childTargetPath!], false),
          setProp_UNSAFE(
            childTargetPath!,
            PP.create('style'),
            jsExpressionValue(childStyleValue, emptyComments),
          ),
        ],
        'everyone',
      ).entireUpdateFinished
      markStart('absolute_move_interaction', framesPassed)

      // Move it down and to the right.
      controlsContainerElement.dispatchEvent(
        new MouseEvent('mousedown', {
          detail: 1,
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: targetBounds.left + 20,
          clientY: targetBounds.top + 20,
          buttons: 1,
        }),
      )
      await wait(0)

      // Mouse move and performance marks for that.
      markStart('absolute_move_move', framesPassed)
      for (let moveCount = 1; moveCount <= 1; moveCount++) {
        controlsContainerElement.dispatchEvent(
          new MouseEvent('mousemove', {
            detail: 1,
            bubbles: true,
            cancelable: true,
            metaKey: false,
            clientX: targetBounds.left + (20 + 10 + moveCount * 3),
            clientY: targetBounds.top + (20 + 10 + moveCount * 4),
            buttons: 1,
          }),
        )
        const newBounds = targetElement.getBoundingClientRect()
        if (newBounds.left !== targetBounds.left + 10 + moveCount * 3) {
          console.info(
            'ABSOLUTE_MOVE_TEST_ERROR - newBounds.left !== targetBounds.left + 10 + moveCount * 3',
          )
          return
        }
        await wait(0)
      }
      markEnd('absolute_move_move', framesPassed)
      measureStep('absolute_move_move', framesPassed)

      controlsContainerElement.dispatchEvent(
        new MouseEvent('mouseup', {
          detail: 1,
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: targetBounds.left + 50,
          clientY: targetBounds.top + 60,
          buttons: 1,
        }),
      )
      await wait(0)
      markEnd('absolute_move_interaction', framesPassed)
      measureStep('absolute_move_interaction', framesPassed)

      if (framesPassed < NumberOfIterations) {
        framesPassed++
        requestAnimationFrame(step)
      } else {
        console.info('ABSOLUTE_MOVE_TEST_FINISHED')
      }
    }
    requestAnimationFrame(step)
  }, [dispatch, builtInDependencies, projectContents, projectId, allPaths, metadata])
  return trigger
}

export function useTriggerSelectionChangePerformanceTest(): () => void {
  const projectContents = LargeProjectContents
  const dispatch = useDispatch() as DebugDispatch
  const allPaths = useRefEditorState((store) =>
    MetadataUtils.getAllPaths(store.editor.jsxMetadata, store.editor.elementPathTree),
  )
  const metadata = useRefEditorState(React.useCallback((store) => store.editor.jsxMetadata, []))
  const builtInDependencies = useEditorState(
    Substores.restOfStore,
    (store) => store.builtInDependencies,
    'useTriggerSelectionChangePerformanceTest builtInDependencies',
  )
  const projectId = useEditorState(
    Substores.fullStore,
    (store) => store.editor.id,
    'useTriggerScrollPerformanceTest id',
  )
  const trigger = React.useCallback(async () => {
    const editorReady = await loadProject(dispatch, builtInDependencies, projectContents, projectId)
    if (!editorReady) {
      console.info('SELECTION_CHANGE_TEST_ERROR - editor not ready')
      return
    }
    const initialTargetPath = [...allPaths.current].sort(
      (a, b) => EP.toString(b).length - EP.toString(a).length,
    )[0]
    // This is very particularly tied to the test project in LargeProjectContents, we _really_ need
    // to pick the right element because our changes can cause other elements to end up on top of the
    // target we want.
    const parentParentPath = EP.parentPath(EP.parentPath(initialTargetPath))
    const grandChildrenPaths = allPaths.current.filter((path) => {
      return EP.pathsEqual(parentParentPath, EP.parentPath(EP.parentPath(path)))
    })
    if (grandChildrenPaths.length === 0) {
      console.info('SELECTION_CHANGE_TEST_ERROR - grandChildrenPaths.length === 0')
      return
    }
    const targetPath = forceNotNull('Invalid array.', last(grandChildrenPaths))

    // Delete the other children that just get in the way.
    const parentPath = EP.parentPath(targetPath)
    const siblingPaths = allPaths.current.filter(
      (path) => EP.isChildOf(path, parentPath) && !EP.pathsEqual(path, targetPath),
    )
    await dispatch(
      siblingPaths.map((path) => deleteView(path)),
      'everyone',
    ).entireUpdateFinished
    // Focus the target so that we can edit the child div inside it.
    await dispatch([setFocusedElement(targetPath)], 'everyone').entireUpdateFinished

    const childTargetPath = allPaths.current.find((path) => EP.isChildOf(path, targetPath))
    if (childTargetPath == null) {
      console.info('SELECTION_CHANGE_TEST_ERROR - child target path does not exist')
      return
    }
    const childMetadata = MetadataUtils.findElementByElementPath(metadata.current, childTargetPath)
    if (
      childMetadata == null ||
      childMetadata.globalFrame == null ||
      isInfinityRectangle(childMetadata.globalFrame) ||
      childMetadata.specialSizeMeasurements.coordinateSystemBounds == null
    ) {
      console.info('SELECTION_CHANGE_TEST_ERROR - child metadata does not exist')
      return
    }
    const childStyleValue = {
      position: 'absolute',
      left:
        childMetadata.globalFrame.x -
        childMetadata.specialSizeMeasurements.coordinateSystemBounds.x,
      top:
        childMetadata.globalFrame.y -
        childMetadata.specialSizeMeasurements.coordinateSystemBounds.y,
      width: childMetadata.globalFrame.width,
      height: childMetadata.globalFrame.height,
    }

    // Determine where the events should be fired.
    const controlsContainerElement = forceNotNull(
      'Container controls element should exist.',
      document.getElementById(CanvasControlsContainerID),
    )
    const canvasContainerElement = forceNotNull(
      'Canvas container element should exist.',
      document.getElementById(CanvasContainerID),
    )
    const canvasContainerBounds = canvasContainerElement.getBoundingClientRect()
    const navigatorElement = forceNotNull(
      'Navigator element should exist.',
      document.getElementById(NavigatorContainerId),
    )
    const navigatorBounds = navigatorElement.getBoundingClientRect()

    const targetElement = forceNotNull(
      'Target element should exist.',
      document.querySelector(`*[data-path^="${EP.toString(childTargetPath)}"]`),
    )
    const originalTargetBounds = targetElement.getBoundingClientRect()
    const leftToTarget =
      canvasContainerBounds.left + navigatorBounds.width - originalTargetBounds.left + 100
    const topToTarget = canvasContainerBounds.top - originalTargetBounds.top + 100
    await dispatch(
      [
        updateEditorMode(EditorModes.selectMode(null, false, 'none')),
        CanvasActions.positionCanvas(canvasPoint({ x: leftToTarget, y: topToTarget })),
      ],
      'everyone',
    ).entireUpdateFinished
    const targetBounds = targetElement.getBoundingClientRect()

    let framesPassed = 0
    async function step() {
      // Make the div inside the target absolute positioned and ensure it is selected.
      await dispatch(
        [
          selectComponents([childTargetPath!], false),
          setProp_UNSAFE(
            childTargetPath!,
            PP.create('style'),
            jsExpressionValue(childStyleValue, emptyComments),
          ),
        ],
        'everyone',
      ).entireUpdateFinished

      // Mouse move and performance marks for that.
      markStart('selection_change', framesPassed)
      // Select something else.
      controlsContainerElement.dispatchEvent(
        new MouseEvent('mousedown', {
          detail: 1,
          bubbles: true,
          cancelable: true,
          metaKey: true,
          clientX: targetBounds.left + 200,
          clientY: targetBounds.top,
          buttons: 1,
        }),
      )
      markEnd('selection_change', framesPassed)
      measureStep('selection_change', framesPassed)

      // Mouse up to finish.
      controlsContainerElement.dispatchEvent(
        new MouseEvent('mouseup', {
          detail: 1,
          bubbles: true,
          cancelable: true,
          metaKey: true,
          clientX: targetBounds.left + 200,
          clientY: targetBounds.top,
          buttons: 1,
        }),
      )

      if (framesPassed < NumberOfIterations) {
        framesPassed++
        requestAnimationFrame(step)
      } else {
        console.info('SELECTION_CHANGE_TEST_FINISHED')
      }
    }
    requestAnimationFrame(step)
  }, [dispatch, builtInDependencies, projectContents, projectId, allPaths, metadata])
  return trigger
}
