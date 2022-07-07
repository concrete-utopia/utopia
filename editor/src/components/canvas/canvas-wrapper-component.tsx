import React from 'react'
import { EditorCanvas } from '../../templates/editor-canvas'
import { ReactErrorOverlay } from '../../third-party/react-error-overlay/react-error-overlay'
import { setFocus } from '../common/actions'
import {
  clearHighlightedViews,
  openCodeEditorFile,
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
import { useEditorState } from '../editor/store/store-hook'
import ErrorOverlay from '../../third-party/react-error-overlay/components/ErrorOverlay'
import CloseButton from '../../third-party/react-error-overlay/components/CloseButton'
import { fastForEach, NO_OP } from '../../core/shared/utils'
import Footer from '../../third-party/react-error-overlay/components/Footer'
import Header from '../../third-party/react-error-overlay/components/Header'
import { FlexColumn, Button, UtopiaTheme, FlexRow } from '../../uuiui'
import { useReadOnlyRuntimeErrors } from '../../core/shared/runtime-report-logs'
import StackFrame from '../../third-party/react-error-overlay/utils/stack-frame'
import { ModeSelectButtons } from './mode-select-buttons'
import { AlwaysTrue, usePubSubAtomReadOnly } from '../../core/shared/atom-with-pub-sub'
import { ErrorMessage } from '../../core/shared/error-messages'
import CanvasActions from './canvas-actions'
import { EditorModes } from '../editor/editor-modes'
import { CanvasStrategyIndicator } from './controls/select-mode/canvas-strategy-indicator'
import { when } from '../../utils/react-conditionals'
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
  const { dispatch, editorState, derivedState } = useEditorState(
    (store) => ({
      dispatch: store.dispatch,
      editorState: store.editor,
      derivedState: store.derived,
    }),
    'CanvasWrapperComponent',
  )

  const fatalErrors = React.useMemo(() => {
    return getAllCodeEditorErrors(editorState, 'fatal', true)
  }, [editorState])

  const safeMode = useEditorState((store) => {
    return store.editor.safeMode
  }, 'CanvasWrapperComponent safeMode')

  const isNavigatorOverCanvas = useEditorState(
    (store) => !store.editor.navigator.minimised,
    'ErrorOverlayComponent isOverlappingWithNavigator',
  )

  const navigatorWidth = usePubSubAtomReadOnly(NavigatorWidthAtom, AlwaysTrue)

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
          }}
        >
          {safeMode ? <SafeModeErrorOverlay /> : <ErrorOverlayComponent />}
          <ModeSelectButtons />
          {when(isFeatureEnabled('Canvas Strategies'), <CanvasStrategyIndicator />)}
        </FlexColumn>
      </FlexRow>
    </FlexColumn>
  )
})

const ErrorOverlayComponent = React.memo(() => {
  const dispatch = useEditorState((store) => store.dispatch, 'ErrorOverlayComponent dispatch')
  const utopiaParserErrors = useEditorState((store) => {
    return parseFailureAsErrorMessages(
      getOpenUIJSFileKey(store.editor),
      getOpenUIJSFile(store.editor),
    )
  }, 'ErrorOverlayComponent utopiaParserErrors')
  const fatalCodeEditorErrors = useEditorState((store) => {
    return getAllCodeEditorErrors(store.editor, 'error', true)
  }, 'ErrorOverlayComponent fatalCodeEditorErrors')

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
  const dispatch = useEditorState((store) => store.dispatch, 'SafeModeErrorOverlay dispatch')
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
