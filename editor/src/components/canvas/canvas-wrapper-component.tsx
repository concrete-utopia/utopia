import React from 'react'
import { EditorCanvas } from '../../templates/editor-canvas'
import { ReactErrorOverlay } from '../../third-party/react-error-overlay/react-error-overlay'
import { setFocus } from '../common/actions'
import {
  clearHighlightedViews,
  clearPostActionData,
  openCodeEditorFile,
  setSafeMode,
  switchEditorMode,
} from '../editor/actions/action-creators'
import {
  createCanvasModelKILLME,
  getAllCodeEditorErrors,
  LeftPanelWidthAtom,
  CanvasSizeAtom,
} from '../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import ErrorOverlay from '../../third-party/react-error-overlay/components/ErrorOverlay'
import CloseButton from '../../third-party/react-error-overlay/components/CloseButton'
import { fastForEach, NO_OP } from '../../core/shared/utils'
import Footer from '../../third-party/react-error-overlay/components/Footer'
import Header from '../../third-party/react-error-overlay/components/Header'
import { FlexColumn, Button, UtopiaTheme, FlexRow } from '../../uuiui'
import {
  AlwaysTrue,
  usePubSubAtomReadOnly,
  usePubSubAtomWriteOnly,
} from '../../core/shared/atom-with-pub-sub'
import type { ErrorMessage } from '../../core/shared/error-messages'
import CanvasActions from './canvas-actions'
import { EditorModes } from '../editor/editor-modes'
import { CanvasStrategyPicker } from './controls/select-mode/canvas-strategy-picker'
import { StrategyIndicator } from './controls/select-mode/strategy-indicator'
import { CanvasToolbar } from '../editor/canvas-toolbar'
import { useDispatch } from '../editor/store/dispatch-context'
import { shouldShowErrorOverlay } from './canvas-utils'
import { useErrorOverlayRecords } from '../../core/shared/runtime-report-logs'
import { FloatingPostActionMenu } from './controls/select-mode/post-action-menu'
import { FloatingPanelSizesAtom } from './floating-panels'
import { isFeatureEnabled } from '../../utils/feature-switches'

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

  const builtinDependencies = useEditorState(
    Substores.builtInDependencies,
    (store) => store.builtInDependencies,
    'CanvasWrapperComponent builtinDependencies',
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
    (store) => store.editor.leftMenu.expanded,
    'ErrorOverlayComponent isOverlappingWithNavigator',
  )

  const scale = useEditorState(Substores.canvas, (store) => store.editor.canvas.scale, 'scale')

  const leftPanelWidthAtom = usePubSubAtomReadOnly(LeftPanelWidthAtom, AlwaysTrue)
  const columnSize = usePubSubAtomReadOnly(FloatingPanelSizesAtom, AlwaysTrue)
  const leftPanelWidth = React.useMemo(
    () => (isNavigatorOverCanvas ? leftPanelWidthAtom + 10 : 0),
    [leftPanelWidthAtom, isNavigatorOverCanvas],
  )

  const codeEditorWidth = useEditorState(
    Substores.restOfEditor,
    (store) =>
      store.editor.interfaceDesigner.codePaneVisible
        ? store.editor.interfaceDesigner.codePaneWidth + 10
        : 0,
    'CanvasWrapperComponent codeEditorWidth',
  )

  const updateCanvasSize = usePubSubAtomWriteOnly(CanvasSizeAtom)

  const postActionSessionInProgress = useEditorState(
    Substores.postActionInteractionSession,
    (store) => store.postActionInteractionSession != null,
    'CanvasWrapperComponent postActionSessionInProgress',
  )
  const { errorRecords, overlayErrors } = useErrorOverlayRecords()
  const errorOverlayShown = shouldShowErrorOverlay(errorRecords, overlayErrors)
  const shouldDimErrorMessage = postActionSessionInProgress && errorOverlayShown

  const onOverlayClick = React.useCallback(() => {
    if (shouldDimErrorMessage) {
      dispatch([clearPostActionData()])
    }
  }, [dispatch, shouldDimErrorMessage])

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
    >
      {fatalErrors.length === 0 && !safeMode ? (
        <EditorCanvas
          userState={userState}
          editor={editorState}
          model={createCanvasModelKILLME(editorState, derivedState)}
          builtinDependencies={builtinDependencies}
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
          left: isFeatureEnabled('Draggable Floating Panels')
            ? columnSize.left
            : leftPanelWidth + codeEditorWidth,
        }}
      >
        <FlexRow
          style={{
            position: 'absolute',
            top: 0,
            alignItems: 'flex-start',
            margin: 10,
            gap: 10,
          }}
        >
          <CanvasToolbar />
          <CanvasStrategyPicker />
        </FlexRow>
        {/* The error overlays are deliberately the last here so they hide other canvas UI */}
        {safeMode ? <SafeModeErrorOverlay /> : <ErrorOverlayComponent />}
      </FlexRow>
      <FlexRow
        style={{
          position: 'absolute',
          width: '100%',
          height: '100%',
          transform: 'translateZ(0)', // to keep this from tarnishing canvas render performance, we force it to a new layer
          pointerEvents: errorOverlayShown ? 'initial' : 'none', // you need to re-enable pointerevents for the various overlays
          transformOrigin: 'left top',
        }}
        onClick={onOverlayClick}
      >
        <div
          style={{
            position: 'relative',
            width: '100%',
            height: '100%',
            pointerEvents: 'none',
            zoom: `${scale * 100}%`,
            background: `rgba(255, 255, 255, ${shouldDimErrorMessage ? 0.5 : 0})`,
          }}
        >
          <FloatingPostActionMenu errorOverlayShown={errorOverlayShown} />
        </div>
      </FlexRow>
    </FlexColumn>
  )
})

const ErrorOverlayComponent = React.memo(() => {
  const { errorRecords, overlayErrors } = useErrorOverlayRecords()
  const overlayWillShow = shouldShowErrorOverlay(errorRecords, overlayErrors)

  const dispatch = useDispatch()

  const onOpenFile = React.useCallback(
    (path: string) => {
      dispatch([openCodeEditorFile(path, true), setFocus('codeEditor')])
    },
    [dispatch],
  )

  const postActionSessionInProgressRef = useRefEditorState(
    (store) => store.postActionInteractionSession != null,
  )

  React.useEffect(() => {
    if (overlayWillShow) {
      if (postActionSessionInProgressRef.current) {
        return
      }

      // If this is showing, we need to clear any canvas drag state and apply the changes it would have resulted in,
      // since that might have been the cause of the error being thrown, as well as switching back to select mode
      setTimeout(() => {
        // wrapping in a setTimeout so we don't dispatch from inside React lifecycle

        dispatch([
          CanvasActions.clearInteractionSession(true),
          switchEditorMode(EditorModes.selectMode()),
          clearHighlightedViews(),
        ])
      }, 0)
    }
  }, [dispatch, overlayWillShow, postActionSessionInProgressRef])

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
