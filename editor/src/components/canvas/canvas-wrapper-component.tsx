import React from 'react'
import * as EP from '../../core/shared/element-path'
import { EditorCanvas } from '../../templates/editor-canvas'
import { ReactErrorOverlay } from '../../third-party/react-error-overlay/react-error-overlay'
import { setFocus } from '../common/actions'
import {
  clearHighlightedViews,
  clearHoveredViews,
  clearSelection,
  openCodeEditorFile,
  selectComponents,
  setHighlightedViews,
  setHoveredViews,
  setSafeMode,
  switchEditorMode,
} from '../editor/actions/action-creators'
import {
  createCanvasModelKILLME,
  getAllCodeEditorErrors,
  getOpenUIJSFile,
  getOpenUIJSFileKey,
  parseFailureAsErrorMessages,
  NavigatorWidthAtom,
  CanvasSizeAtom,
} from '../editor/store/editor-state'
import { Substores, useEditorState } from '../editor/store/store-hook'
import ErrorOverlay from '../../third-party/react-error-overlay/components/ErrorOverlay'
import CloseButton from '../../third-party/react-error-overlay/components/CloseButton'
import { fastForEach, NO_OP } from '../../core/shared/utils'
import Footer from '../../third-party/react-error-overlay/components/Footer'
import Header from '../../third-party/react-error-overlay/components/Header'
import { FlexColumn, Button, UtopiaTheme, FlexRow, useColorTheme } from '../../uuiui'
import { useReadOnlyRuntimeErrors } from '../../core/shared/runtime-report-logs'
import StackFrame from '../../third-party/react-error-overlay/utils/stack-frame'
import {
  AlwaysTrue,
  usePubSubAtomReadOnly,
  usePubSubAtomWriteOnly,
} from '../../core/shared/atom-with-pub-sub'
import { ErrorMessage } from '../../core/shared/error-messages'
import CanvasActions from './canvas-actions'
import { EditorModes, isSelectModeWithArea } from '../editor/editor-modes'
import { CanvasStrategyPicker } from './controls/select-mode/canvas-strategy-picker'
import { StrategyIndicator } from './controls/select-mode/strategy-indicator'
import { CanvasToolbar } from '../editor/canvas-toolbar'
import { useDispatch } from '../editor/store/dispatch-context'
import {
  CanvasPoint,
  CanvasRectangle,
  canvasPoint,
  canvasRectangle,
  isFiniteRectangle,
  rectContainsPoint,
  rectangleContainsRectangle,
  rectanglesOverlap,
  windowPoint,
} from '../../core/shared/math-utils'
import { ElementPath } from '../../core/shared/project-file-types'
import { windowToCanvasCoordinates } from './dom-lookup'
import { when } from '../../utils/react-conditionals'
import { EditorAction } from '../editor/action-types'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../core/shared/array-utils'
import { ElementInstanceMetadata } from '../../core/shared/element-template'

export const CanvasWrapperTestId = 'canvas-wrapper'

export function filterOldPasses(errorMessages: Array<ErrorMessage>): Array<ErrorMessage> {
  let passTimes: { [key: string]: number } = {}
  fastForEach(errorMessages, (errorMessage) => {
    if (errorMessage.passTime != null) {
      if (errorMessage.source in passTimes) {
        const existingPassCount = passTimes[errorMessage.source]
        if (errorMessage.passTime > existingPassCount) {
          passTimes[errorMessage.source] = errorMessage.passTime
        }
      } else {
        passTimes[errorMessage.source] = errorMessage.passTime
      }
    }
  })
  return errorMessages.filter((errorMessage) => {
    if (errorMessage.passTime == null) {
      return true
    } else {
      return passTimes[errorMessage.source] === errorMessage.passTime
    }
  })
}

export const CanvasWrapperComponent = React.memo(() => {
  const dispatch = useDispatch()
  const { editorState, derivedState, userState } = useEditorState(
    Substores.fullStore,
    (store) => ({
      editorState: store.editor,
      derivedState: store.derived,
      userState: store.userState,
    }),
    'CanvasWrapperComponent',
  )

  const fatalErrors = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return getAllCodeEditorErrors(store.editor.codeEditorErrors, 'fatal', true)
    },
    'CanvasWrapperComponent fatalErrors',
  )

  const safeMode = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return store.editor.safeMode
    },
    'CanvasWrapperComponent safeMode',
  )

  const isNavigatorOverCanvas = useEditorState(
    Substores.restOfEditor,
    (store) => !store.editor.navigator.minimised,
    'CanvasWrapperComponent isOverlappingWithNavigator',
  )

  const updateCanvasSize = usePubSubAtomWriteOnly(CanvasSizeAtom)
  const navigatorWidth = usePubSubAtomReadOnly(NavigatorWidthAtom, AlwaysTrue)

  const actualNavigatorWidth = React.useMemo(() => {
    if (!isNavigatorOverCanvas) {
      return 0
    }
    return navigatorWidth
  }, [navigatorWidth, isNavigatorOverCanvas])

  const ref = React.useRef<HTMLDivElement | null>(null)

  const [mousePoint, setMousePoint] = React.useState<CanvasPoint | null>(null)
  const [selectionAreaStart, setSelectionAreaStart] = React.useState<CanvasPoint | null>(null)
  const [elementsUnderSelectionArea, setElementsUnderSelectionArea] = React.useState<ElementPath[]>(
    [],
  )

  const mode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode,
    'CanvasWrapperComponent mode',
  )
  const metadata = useEditorState(
    Substores.metadata,
    (store) => store.editor.jsxMetadata,
    'CanvasWrapperComponent metadata',
  )
  const canvasScale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'CanvasWrapperComponent canvasScale',
  )
  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.realCanvasOffset,
    'CanvasWrapperComponent canvasOffset',
  )
  const highlightedViews = useEditorState(
    Substores.highlightedHoveredViews,
    (store) => store.editor.highlightedViews,
    'CanvasWrapperComponent highlightedViews',
  )

  const selectableElements = React.useMemo(() => {
    return mapDropNulls((path) => {
      return MetadataUtils.findElementByElementPath(metadata, path)
    }, MetadataUtils.getAllCanvasSelectablePathsUnordered(metadata))
  }, [metadata])

  const mousePointOnCanvas = React.useMemo(() => {
    if (mousePoint == null) {
      return null
    }
    return windowToCanvasCoordinates(canvasScale, canvasOffset, windowPoint(mousePoint))
      .canvasPositionRounded
  }, [mousePoint, canvasScale, canvasOffset])

  const mouseIsOverStoryboardOrEmptyScene = React.useMemo(() => {
    function isUnderMouse(e: ElementInstanceMetadata) {
      return mousePointOnCanvas == null
        ? false
        : e.globalFrame != null &&
            isFiniteRectangle(e.globalFrame) &&
            rectContainsPoint(e.globalFrame, mousePointOnCanvas)
    }

    const nonSelectableElementsPossiblyUnderMouse = Object.values(metadata).filter(
      (e) =>
        !selectableElements.some((other) => EP.pathsEqual(other.elementPath, e.elementPath)) &&
        !MetadataUtils.isProbablySceneFromMetadata(e),
    )

    const elementsPossiblyUnderMouse = [
      ...selectableElements,
      ...nonSelectableElementsPossiblyUnderMouse,
    ]

    return !elementsPossiblyUnderMouse.some(isUnderMouse)
  }, [mousePointOnCanvas, selectableElements, metadata])

  const selectionArea = React.useMemo((): CanvasRectangle | null => {
    if (selectionAreaStart == null || mousePoint == null) {
      return null
    }
    return canvasRectangle({
      x: Math.min(selectionAreaStart.x, mousePoint.x),
      y: Math.min(selectionAreaStart.y, mousePoint.y),
      width:
        Math.max(selectionAreaStart.x, mousePoint.x) - Math.min(selectionAreaStart.x, mousePoint.x),
      height:
        Math.max(selectionAreaStart.y, mousePoint.y) - Math.min(selectionAreaStart.y, mousePoint.y),
    })
  }, [mousePoint, selectionAreaStart])

  const canSelectArea = React.useMemo(() => {
    if (selectionArea != null || isSelectModeWithArea(mode)) {
      return true
    }
    if (mode.type !== 'select') {
      return false
    }
    if (!mouseIsOverStoryboardOrEmptyScene) {
      return false
    }
    return true
  }, [mode, mouseIsOverStoryboardOrEmptyScene, selectionArea])

  const canvasSelectionArea = React.useMemo(() => {
    const refRect = ref.current?.getBoundingClientRect() ?? null
    if (selectionArea == null || refRect == null) {
      return null
    }

    return canvasRectangle({
      x: selectionArea.x - refRect.x - actualNavigatorWidth,
      y: selectionArea.y - refRect.y,
      width: selectionArea.width,
      height: selectionArea.height,
    })
  }, [selectionArea, actualNavigatorWidth])

  function onMouseMove(e: React.MouseEvent) {
    setMousePoint(canvasPoint({ x: e.clientX, y: e.clientY }))
  }

  function isValidMouseEventForSelectionArea(e: React.MouseEvent): boolean {
    return e.button === 0 && !(e.shiftKey || e.metaKey || e.ctrlKey || e.altKey)
  }

  const onMouseDown = React.useCallback(
    (e: React.MouseEvent) => {
      if (isValidMouseEventForSelectionArea(e)) {
        if (canSelectArea && selectionAreaStart == null) {
          setSelectionAreaStart(mousePoint)
          setElementsUnderSelectionArea([])
          dispatch([switchEditorMode(EditorModes.selectMode(null, true)), clearSelection()])
        }
      }
    },
    [canSelectArea, mousePoint, dispatch, selectionAreaStart],
  )

  const clearAndGetActions = React.useCallback((): EditorAction[] => {
    if (mode.type !== 'select') {
      return []
    }
    return [
      switchEditorMode(EditorModes.selectMode()),
      clearHoveredViews(),
      clearHighlightedViews(),
    ]
  }, [mode.type])

  const onMouseUp = React.useCallback(
    (e: React.MouseEvent) => {
      setSelectionAreaStart(null)
      let actions: EditorAction[] = clearAndGetActions()
      if (
        selectionAreaStart != null &&
        highlightedViews.length > 0 &&
        isValidMouseEventForSelectionArea(e)
      ) {
        actions.unshift(selectComponents(highlightedViews, false))
      }
      dispatch(actions)
    },
    [dispatch, selectionAreaStart, highlightedViews, clearAndGetActions],
  )

  React.useEffect(() => {
    if (selectionArea == null || mousePoint == null || selectionAreaStart == null) {
      return
    }
    let res: {
      path: ElementPath
      type: 'storyboard-child' | 'scene' | 'scene-child'
      fullyCovered: boolean
    }[] = []

    const topLeft = windowToCanvasCoordinates(
      canvasScale,
      canvasOffset,
      windowPoint({
        x: selectionArea.x,
        y: selectionArea.y,
      }),
    ).canvasPositionRounded
    const bottomRight = windowToCanvasCoordinates(
      canvasScale,
      canvasOffset,
      windowPoint({
        x: selectionArea.x + selectionArea.width,
        y: selectionArea.y + selectionArea.height,
      }),
    ).canvasPositionRounded

    const rect = canvasRectangle({
      x: topLeft.x,
      y: topLeft.y,
      width: bottomRight.x - topLeft.x,
      height: bottomRight.y - topLeft.y,
    })
    for (const element of Object.values(metadata)) {
      const isChildOfSceneRoot = MetadataUtils.isProbablyScene(
        metadata,
        EP.nthParentPath(element.elementPath, 3),
      )
      const isValidTarget =
        isChildOfSceneRoot || EP.isStoryboardPath(EP.parentPath(element.elementPath))
      const frame = element.globalFrame
      if (
        frame != null &&
        isFiniteRectangle(frame) &&
        rectanglesOverlap(frame, rect) &&
        isValidTarget
      ) {
        res.push({
          path: element.elementPath,
          type: isChildOfSceneRoot
            ? 'scene-child'
            : MetadataUtils.isProbablyScene(metadata, element.elementPath)
            ? 'scene'
            : 'storyboard-child',
          fullyCovered: rectangleContainsRectangle(rect, frame),
        })
      }
    }

    const thereAreStoryboardChildren = res.some((other) => other.type === 'storyboard-child')
    res = res.filter((e) => {
      // if the element is a schene child and there are storyboard children, skip it
      if (e.type === 'scene-child' && thereAreStoryboardChildren) {
        return false
      }
      // if the element is a scene, and the scene is not fully covered skip the scene
      if (e.type === 'scene' && !e.fullyCovered) {
        return false
      }
      // if a scene is fully covered, select just the scene and omit its children
      if (e.type === 'scene-child') {
        const parentScene = res.find(
          (other) => other.type === 'scene' && EP.isDescendantOf(e.path, other.path),
        )
        if (parentScene != null && parentScene.fullyCovered) {
          return false
        }
      }
      return true
    })

    setElementsUnderSelectionArea((old) => {
      const paths = res.map((e) => e.path)
      if (old.length !== res.length) {
        return paths
      }
      if (!old.every((a) => paths.some((b) => EP.pathsEqual(a, b)))) {
        return paths
      }
      return old
    })
  }, [selectionArea, metadata, canvasScale, canvasOffset, mousePoint, selectionAreaStart])

  React.useEffect(() => {
    if (mode.type !== 'select') {
      setSelectionAreaStart(null)
    }
  }, [mode.type])

  React.useEffect(() => {
    if (elementsUnderSelectionArea == null) {
      return
    }
    dispatch([
      setHighlightedViews(elementsUnderSelectionArea),
      setHoveredViews(elementsUnderSelectionArea),
    ])
  }, [dispatch, elementsUnderSelectionArea])

  return (
    <FlexColumn
      className='CanvasWrapperComponent'
      style={{
        position: 'relative',
        overflowX: 'hidden',
        justifyContent: 'stretch',
        alignItems: 'stretch',
        flexGrow: 1,
        height: '100%',
        // ^ prevents Monaco from pushing the inspector out
      }}
      ref={ref}
      data-testId={CanvasWrapperTestId}
      onMouseDown={onMouseDown}
      onMouseMove={onMouseMove}
      onMouseUp={onMouseUp}
    >
      {fatalErrors.length === 0 && !safeMode ? (
        <EditorCanvas
          userState={userState}
          editor={editorState}
          model={createCanvasModelKILLME(editorState, derivedState)}
          updateCanvasSize={updateCanvasSize}
          dispatch={dispatch}
        />
      ) : null}
      <FlexRow
        style={{
          position: 'absolute',
          width: '100%',
          height: '100%',
          transform: 'translateZ(0)', // to keep this from tarnishing canvas render performance, we force it to a new layer
          pointerEvents: 'none', // you need to re-enable pointerevents for the various overlays
        }}
      >
        <div
          style={{
            width: isNavigatorOverCanvas ? navigatorWidth : 0,
          }}
        />
        <FlexColumn
          style={{
            alignSelf: 'stretch',
            flexGrow: 1,
            position: 'relative',
            alignItems: 'flex-start',
            justifyContent: 'flex-start',
          }}
        >
          {when(canSelectArea, <AreaSelect rectangle={canvasSelectionArea} />)}
          <CanvasStrategyPicker />
          <StrategyIndicator />
          <CanvasToolbar />

          {/* The error overlays are deliberately the last here so they hide other canvas UI */}
          {safeMode ? <SafeModeErrorOverlay /> : <ErrorOverlayComponent />}
        </FlexColumn>
      </FlexRow>
    </FlexColumn>
  )
})

const AreaSelect = React.memo(({ rectangle }: { rectangle: CanvasRectangle | null }) => {
  const colorTheme = useColorTheme()
  if (rectangle == null) {
    return null
  }

  return (
    <div
      style={{
        border: `1px solid ${colorTheme.primary.value}`,
        background: colorTheme.primary10.value,
        position: 'absolute',
        width: rectangle.width,
        height: rectangle.height,
        left: rectangle.x,
        top: rectangle.y,
      }}
    />
  )
})

const ErrorOverlayComponent = React.memo(() => {
  const dispatch = useDispatch()
  const utopiaParserErrors = useEditorState(
    Substores.fullStore,
    (store) => {
      return parseFailureAsErrorMessages(
        getOpenUIJSFileKey(store.editor),
        getOpenUIJSFile(store.editor),
      )
    },
    'ErrorOverlayComponent utopiaParserErrors',
  )
  const fatalCodeEditorErrors = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return getAllCodeEditorErrors(store.editor.codeEditorErrors, 'error', true)
    },
    'ErrorOverlayComponent fatalCodeEditorErrors',
  )

  const runtimeErrors = useReadOnlyRuntimeErrors()

  const overlayErrors = React.useMemo(() => {
    return runtimeErrors.map((runtimeError) => {
      const stackFrames =
        runtimeError.error.stackFrames != null
          ? runtimeError.error.stackFrames
          : [
              new StackFrame(
                'WARNING: This error has no Stack Frames, it might be coming from Utopia itself!',
              ),
            ]
      return {
        error: runtimeError.error,
        unhandledRejection: false,
        contextSize: 3, // magic number from react-error-overlay
        stackFrames: stackFrames,
      }
    })
  }, [runtimeErrors])

  const lintErrors = fatalCodeEditorErrors.filter((e) => e.source === 'eslint')
  // we start with the lint errors, since those show up the fastest. any subsequent error will go below in the error screen
  const errorRecords = filterOldPasses([...lintErrors, ...utopiaParserErrors])

  const onOpenFile = React.useCallback(
    (path: string) => {
      dispatch([openCodeEditorFile(path, true), setFocus('codeEditor')])
    },
    [dispatch],
  )

  const overlayWillShow = errorRecords.length > 0 || overlayErrors.length > 0

  React.useEffect(() => {
    if (overlayWillShow) {
      // If this is showing, we need to clear any canvas drag state and apply the changes it would have resulted in,
      // since that might have been the cause of the error being thrown, as well as switching back to select mode
      setTimeout(() => {
        // wrapping in a setTimeout so we don't dispatch from inside React lifecycle
        dispatch([
          CanvasActions.clearDragState(true),
          CanvasActions.clearInteractionSession(true),
          switchEditorMode(EditorModes.selectMode()),
          clearHighlightedViews(),
        ])
      }, 0)
    }
  }, [dispatch, overlayWillShow])

  return (
    <ReactErrorOverlay
      currentBuildErrorRecords={errorRecords}
      currentRuntimeErrorRecords={overlayErrors}
      onOpenFile={onOpenFile}
      overlayOffset={0}
    />
  )
})

export const SafeModeErrorOverlay = React.memo(() => {
  const dispatch = useDispatch()
  const onTryAgain = React.useCallback(() => {
    dispatch([setSafeMode(false)], 'everyone')
  }, [dispatch])

  const wrapperStyle = {
    display: 'flex',
    flexDirection: 'column',
  } as const

  return (
    <ErrorOverlay shortcutHandler={NO_OP}>
      <CloseButton close={NO_OP} />
      <div style={wrapperStyle}>
        <Header headerText={'Recovered from crash'} />
        <div style={{ fontSize: '1.17em', fontWeight: 'bold' }}>
          We recovered your code after a serious crash.
        </div>
      </div>

      <div
        style={{
          display: 'flex',
          flexDirection: 'row',
        }}
      >
        <div
          style={{ display: 'flex', minWidth: 120, justifyContent: 'center', alignItems: 'center' }}
        >
          <Button
            primary={true}
            outline={false}
            highlight
            onClick={onTryAgain}
            style={{
              paddingLeft: 4,
              paddingRight: 4,
              width: 80,
              cursor: 'pointer',
              height: UtopiaTheme.layout.inputHeight.default,
            }}
          >
            Try again
          </Button>
        </div>
        <Footer
          line1='Warning: If the problem is not fixed it will cause another crash'
          line2='If your code is safe and you can reload the editor to try again.'
        />
      </div>
    </ErrorOverlay>
  )
})
