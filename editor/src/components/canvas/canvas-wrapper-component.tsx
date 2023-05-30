import React from 'react'
import * as EP from '../../core/shared/element-path'
import { EditorCanvas } from '../../templates/editor-canvas'
import { ReactErrorOverlay } from '../../third-party/react-error-overlay/react-error-overlay'
import { setFocus } from '../common/actions'
import {
  clearHighlightedViews,
  clearHoveredViews,
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
import { AlwaysTrue, usePubSubAtomReadOnly } from '../../core/shared/atom-with-pub-sub'
import { ErrorMessage } from '../../core/shared/error-messages'
import CanvasActions from './canvas-actions'
import { EditorModes } from '../editor/editor-modes'
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
  windowPoint,
} from '../../core/shared/math-utils'
import { ElementPath } from '../../core/shared/project-file-types'
import { windowToCanvasCoordinates } from './dom-lookup'
import { when } from '../../utils/react-conditionals'

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
    'ErrorOverlayComponent isOverlappingWithNavigator',
  )

  const navigatorWidth = usePubSubAtomReadOnly(NavigatorWidthAtom, AlwaysTrue)

  const ref = React.useRef<HTMLDivElement | null>(null)

  const [mouse, setMouse] = React.useState<CanvasPoint | null>(null)
  const [areaStart, setAreaStart] = React.useState<CanvasPoint | null>(null)

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
    'CanvasWrapperComponent canvas scale',
  )
  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.realCanvasOffset,
    'CanvasWrapperComponent canvas offset',
  )

  React.useEffect(() => {
    if (mode.type !== 'select') {
      setAreaStart(null)
    }
  }, [mode.type])

  const isOnEmpty = React.useMemo(() => {
    if (mouse == null) {
      return false
    }
    for (const element of Object.values(metadata)) {
      const point = windowToCanvasCoordinates(canvasScale, canvasOffset, windowPoint(mouse))
      if (
        element.globalFrame != null &&
        isFiniteRectangle(element.globalFrame) &&
        rectContainsPoint(element.globalFrame, point.canvasPositionRounded)
      ) {
        return false
      }
    }
    return true
  }, [mouse, metadata, canvasScale, canvasOffset])

  const areaSelect = React.useMemo((): CanvasRectangle | null => {
    if (areaStart == null || mouse == null || ref.current == null) {
      return null
    }
    const refRect = ref.current.getBoundingClientRect()
    return canvasRectangle({
      x: Math.min(areaStart.x, mouse.x) - refRect.x,
      y: Math.min(areaStart.y, mouse.y) - refRect.y,
      width: Math.max(areaStart.x, mouse.x) - Math.min(areaStart.x, mouse.x),
      height: Math.max(areaStart.y, mouse.y) - Math.min(areaStart.y, mouse.y),
    })
  }, [mouse, areaStart])

  function rectanglesOverlap(a: CanvasRectangle, b: CanvasRectangle): boolean {
    const xOverlap = a.x < b.x + b.width && a.x + a.width > b.x
    const yOverlap = a.y < b.y + b.height && a.y + a.height > b.y
    return xOverlap && yOverlap
  }

  const canSelectArea = React.useMemo(() => {
    if (areaSelect != null || (mode.type === 'select' && mode.area === true)) {
      return true
    }
    if (mode.type !== 'select') {
      return false
    }
    if (!isOnEmpty) {
      return false
    }
    return true
  }, [mode, isOnEmpty, areaSelect])

  const [underArea, setUnderArea] = React.useState<ElementPath[]>([])

  React.useEffect(() => {
    if (areaSelect == null || ref.current == null) {
      return
    }
    const refRect = ref.current.getBoundingClientRect()
    let res: ElementPath[] = []
    const topLeft = windowToCanvasCoordinates(
      canvasScale,
      canvasOffset,
      windowPoint({ x: areaSelect.x + refRect.x, y: areaSelect.y + refRect.y }),
    ).canvasPositionRounded
    const bottomRight = windowToCanvasCoordinates(
      canvasScale,
      canvasOffset,
      windowPoint({
        x: areaSelect.x + refRect.x + areaSelect.width,
        y: areaSelect.y + refRect.y + areaSelect.height,
      }),
    ).canvasPositionRounded
    const rect = canvasRectangle({
      x: topLeft.x,
      y: topLeft.y,
      width: bottomRight.x - topLeft.x,
      height: bottomRight.y - topLeft.y,
    })
    for (const element of Object.values(metadata)) {
      const frame = element.globalFrame
      if (frame != null && isFiniteRectangle(frame)) {
        if (
          rectanglesOverlap(frame, rect) &&
          EP.isStoryboardPath(EP.parentPath(element.elementPath))
        ) {
          res.push(element.elementPath)
        }
      }
    }
    setUnderArea((old) => {
      if (old.length !== res.length) {
        return res
      }
      if (!old.every((a) => res.some((b) => EP.pathsEqual(a, b)))) {
        return res
      }
      return old
    })
  }, [areaSelect, metadata, canvasScale, canvasOffset])

  const onMouseMove = React.useCallback((e: React.MouseEvent) => {
    setMouse(canvasPoint({ x: e.clientX, y: e.clientY }))
  }, [])

  const onMouseDown = React.useCallback(
    (_e: React.MouseEvent) => {
      if (canSelectArea) {
        setAreaStart(mouse)
        dispatch([switchEditorMode(EditorModes.selectMode(null, true))])
      }
    },
    [mouse, dispatch, canSelectArea],
  )
  const onMouseUp = React.useCallback(() => {
    if (areaStart != null) {
      if (underArea.length > 0) {
        dispatch([selectComponents(underArea, false), clearHoveredViews(), clearHighlightedViews()])
      }
      dispatch([switchEditorMode(EditorModes.selectMode())])
    }
    setAreaStart(null)
  }, [dispatch, areaStart, underArea])

  React.useEffect(() => {
    if (underArea == null) {
      return
    }
    dispatch([setHighlightedViews(underArea), setHoveredViews(underArea)])
  }, [dispatch, underArea])

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
      onMouseMove={onMouseMove}
      onMouseDown={onMouseDown}
      onMouseUp={onMouseUp}
    >
      {fatalErrors.length === 0 && !safeMode ? (
        <EditorCanvas
          userState={userState}
          editor={editorState}
          model={createCanvasModelKILLME(editorState, derivedState)}
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
          {when(canSelectArea, <AreaSelect rectangle={areaSelect} />)}
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
        background: colorTheme.primary30.value,
        opacity: 0.5,
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
