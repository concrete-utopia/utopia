import React, { useRef } from 'react'
import * as ReactDOM from 'react-dom'
import CanvasActions from '../../components/canvas/canvas-actions'
import { DebugDispatch, DispatchPriority, EditorAction } from '../../components/editor/action-types'
import {
  clearSelection,
  deleteView,
  selectComponents,
  setFocusedElement,
  setProp_UNSAFE,
  switchEditorMode,
  unsetProperty,
} from '../../components/editor/actions/action-creators'
import { useEditorState, useRefEditorState } from '../../components/editor/store/store-hook'
import {
  canvasPoint,
  CanvasRectangle,
  CanvasVector,
  windowPoint,
  zeroPoint,
  zeroRectangle,
} from '../shared/math-utils'
import {
  CanvasContainerID,
  resizeDragState,
  updateResizeDragState,
} from '../../components/canvas/canvas-types'
import { MetadataUtils } from './element-metadata-utils'
import { getOriginalFrames } from '../../components/canvas/canvas-utils'
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
import { emptyComments, jsxAttributeValue } from '../shared/element-template'
import { isFeatureEnabled, setFeatureEnabled } from '../../utils/feature-switches'
import { last } from '../shared/array-utils'

export function wait(timeout: number): Promise<void> {
  return new Promise((resolve) => {
    setTimeout(resolve, timeout)
  })
}

const NumberOfIterations = 100

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

export function useTriggerScrollPerformanceTest(): () => void {
  const dispatch = useEditorState(
    (store) => store.dispatch as DebugDispatch,
    'useTriggerScrollPerformanceTest dispatch',
  )
  const allPaths = useRefEditorState((store) => store.derived.navigatorTargets)
  const trigger = React.useCallback(async () => {
    if (allPaths.current.length === 0) {
      console.info('SELECT_TEST_ERROR')
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
  }, [dispatch, allPaths])
  return trigger
}

export function useTriggerResizePerformanceTest(): () => void {
  const dispatch = useEditorState(
    (store) => store.dispatch as DebugDispatch,
    'useTriggerResizePerformanceTest dispatch',
  )
  const metadata = useRefEditorState((store) => store.editor.jsxMetadata)
  const selectedViews = useRefEditorState((store) => store.editor.selectedViews)
  const trigger = React.useCallback(async () => {
    if (selectedViews.current.length === 0) {
      console.info('RESIZE_TEST_MISSING_SELECTEDVIEW')
      return
    }
    await dispatch([switchEditorMode(EditorModes.selectMode())]).entireUpdateFinished

    const target = selectedViews.current[0]
    const targetFrame = MetadataUtils.findElementByElementPath(metadata.current, target)
      ?.globalFrame
    const targetStartPoint =
      targetFrame != null
        ? ({
            x: targetFrame.x + targetFrame.width,
            y: targetFrame.y + targetFrame.height,
          } as CanvasVector)
        : (zeroPoint as CanvasVector)
    const originalFrames = getOriginalFrames(selectedViews.current, metadata.current)

    let framesPassed = 0
    async function step() {
      markStart('resize', framesPassed)
      const dragState = updateResizeDragState(
        resizeDragState(
          targetFrame ?? (zeroRectangle as CanvasRectangle),
          originalFrames,
          { x: 1, y: 1 },
          { x: 1, y: 1 },
          metadata.current,
          [target],
          false,
          [],
        ),
        targetStartPoint,
        { x: framesPassed % 100, y: framesPassed % 100 } as CanvasVector,
        'width',
        true,
        false,
        false,
      )
      await dispatch([CanvasActions.createDragState(dragState)]).entireUpdateFinished
      markEnd('resize', framesPassed)
      measureStep('resize', framesPassed)
      if (framesPassed < NumberOfIterations) {
        framesPassed++
        requestAnimationFrame(step)
      } else {
        await dispatch([CanvasActions.clearDragState(false)]).entireUpdateFinished
        console.info('RESIZE_TEST_FINISHED')
      }
    }
    requestAnimationFrame(step)
  }, [dispatch, metadata, selectedViews])
  return trigger
}

function useTriggerHighlightPerformanceTest(key: 'regular' | 'all-elements'): () => void {
  const allPaths = useRefEditorState((store) => store.derived.navigatorTargets)
  const getHighlightableViews = useGetSelectableViewsForSelectMode()
  const calculateHighlightedViews = useCalculateHighlightedViews(true, getHighlightableViews)
  const trigger = React.useCallback(async () => {
    const allCapsKey = key.toLocaleUpperCase()
    if (allPaths.current.length === 0) {
      console.info(`HIGHLIGHT_${allCapsKey}_TEST_ERROR_NO_PATHS`)
      return
    }

    const targetPath = allPaths.current[0]

    const htmlElement = document.querySelector(`*[data-path*="${EP.toString(targetPath)}"]`)
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
  }, [allPaths, calculateHighlightedViews, key])

  return trigger
}

export const useTriggerRegularHighlightPerformanceTest = () =>
  useTriggerHighlightPerformanceTest('regular')

export const useTriggerAllElementsHighlightPerformanceTest = () =>
  useTriggerHighlightPerformanceTest('all-elements')

export function useTriggerSelectionPerformanceTest(): () => void {
  const dispatch = useEditorState(
    (store) => store.dispatch as DebugDispatch,
    'useTriggerSelectionPerformanceTest dispatch',
  )
  const allPaths = useRefEditorState((store) => store.derived.navigatorTargets)
  const selectedViews = useRefEditorState((store) => store.editor.selectedViews)
  const trigger = React.useCallback(async () => {
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
      document.querySelector(`*[data-path*="${EP.toString(targetPath)}"]`),
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
      console.info('SELECT_TEST_ERROR')
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
  }, [dispatch, allPaths, selectedViews])
  return trigger
}

export function useTriggerAbsoluteMovePerformanceTest(): () => void {
  const dispatch = useEditorState(
    React.useCallback((store) => store.dispatch as DebugDispatch, []),
    'useTriggerAbsoluteMovePerformanceTest dispatch',
  )
  const allPaths = useRefEditorState(
    React.useCallback((store) => store.derived.navigatorTargets, []),
  )
  const metadata = useRefEditorState(React.useCallback((store) => store.editor.jsxMetadata, []))
  const selectedViews = useRefEditorState(
    React.useCallback((store) => store.editor.selectedViews, []),
  )
  const trigger = React.useCallback(async () => {
    const initialTargetPath = [...allPaths.current].sort(
      (a, b) => EP.toString(b).length - EP.toString(a).length,
    )[0]
    // This is very particularly tied to the test project, we _really_ need to pick the
    // right element because our changes can cause other elements to end up on top of the
    // target we want.
    const parentParentPath = EP.parentPath(EP.parentPath(initialTargetPath))
    const grandChildrenPaths = allPaths.current.filter((path) => {
      return EP.pathsEqual(parentParentPath, EP.parentPath(EP.parentPath(path)))
    })
    if (grandChildrenPaths.length === 0) {
      console.info('ABSOLUTE_MOVE_TEST_ERROR')
      return
    }
    const targetPath = forceNotNull('Invalid array.', last(grandChildrenPaths))

    // Switch Canvas Strategies on.
    const strategiesCurrentlyEnabled = isFeatureEnabled('Canvas Strategies')
    setFeatureEnabled('Canvas Strategies', true)
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
      console.info('ABSOLUTE_MOVE_TEST_ERROR')
      return
    }
    const childMetadata = MetadataUtils.findElementByElementPath(metadata.current, childTargetPath)
    if (
      childMetadata == null ||
      childMetadata.globalFrame == null ||
      childMetadata.specialSizeMeasurements.coordinateSystemBounds == null
    ) {
      console.info('ABSOLUTE_MOVE_TEST_ERROR')
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
      document.querySelector(`*[data-path*="${EP.toString(childTargetPath)}"]`),
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

    let framesPassed = 0
    async function step() {
      // Make the div inside the target absolute positioned and ensure it is selected.
      await dispatch(
        [
          selectComponents([childTargetPath!], false),
          setProp_UNSAFE(
            childTargetPath!,
            PP.create(['style']),
            jsxAttributeValue(childStyleValue, emptyComments),
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
            clientX: targetBounds.left + (20 + moveCount * 3),
            clientY: targetBounds.top + (20 + moveCount * 4),
            buttons: 1,
          }),
        )
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
        // Potentially turn off Canvas Strategies.
        setFeatureEnabled('Canvas Strategies', strategiesCurrentlyEnabled)
        // Reset the position.
        await dispatch([unsetProperty(childTargetPath!, PP.create(['style']))], 'everyone')
          .entireUpdateFinished
        // Unfocus the target.
        await dispatch([setFocusedElement(null)], 'everyone').entireUpdateFinished

        console.info('ABSOLUTE_MOVE_TEST_FINISHED')
      }
    }
    requestAnimationFrame(step)
  }, [dispatch, allPaths, metadata])
  return trigger
}

export function useTriggerBaselinePerformanceTest(): () => void {
  const dispatch = useEditorState(
    (store) => store.dispatch as DebugDispatch,
    'useTriggerSelectionPerformanceTest dispatch',
  )

  const trigger = React.useCallback(async () => {
    let framesPassed = 0
    async function step() {
      markStart('baseline', framesPassed)
      for (let i = 0; i < 3000; i++) {
        await dispatch([]).entireUpdateFinished
      }
      markEnd('baseline', framesPassed)
      measureStep('baseline', framesPassed)

      if (framesPassed < NumberOfIterations) {
        framesPassed++
        requestAnimationFrame(step)
      } else {
        requestAnimationFrame(() => console.info('BASELINE_TEST_FINISHED'))
      }
    }
    requestAnimationFrame(step)
  }, [dispatch])

  return trigger
}
