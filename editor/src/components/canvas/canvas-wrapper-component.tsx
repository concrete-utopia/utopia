import * as React from 'react'
import { filterOldPasses } from '../../core/workers/ts/ts-worker'
import { EditorCanvas } from '../../templates/editor-canvas'
import { ReactErrorOverlay } from '../../third-party/react-error-overlay/react-error-overlay'
import { setFocus } from '../common/actions'
import { openCodeEditorFile, setSafeMode } from '../editor/actions/action-creators'
import {
  createCanvasModelKILLME,
  getAllCodeEditorErrors,
  getOpenUIJSFile,
  getOpenUIJSFileKey,
  LeftPaneDefaultWidth,
  parseFailureAsErrorMessages,
} from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
import ErrorOverlay from '../../third-party/react-error-overlay/components/ErrorOverlay'
import CloseButton from '../../third-party/react-error-overlay/components/CloseButton'
import { NO_OP } from '../../core/shared/utils'
import Footer from '../../third-party/react-error-overlay/components/Footer'
import Header from '../../third-party/react-error-overlay/components/Header'
import { FlexColumn, Button, UtopiaTheme } from '../../uuiui'
import { betterReactMemo } from '../../uuiui-deps'
import { useReadOnlyRuntimeErrors } from '../../core/shared/runtime-report-logs'
import StackFrame from '../../third-party/react-error-overlay/utils/stack-frame'
import { ModeSelectButtons } from './mode-select-buttons'

interface CanvasWrapperComponentProps {}

export const CanvasWrapperComponent = betterReactMemo(
  'CanvasWrapperComponent',
  (props: CanvasWrapperComponentProps) => {
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
        {safeMode ? <SafeModeErrorOverlay /> : <ErrorOverlayComponent />}
        <ModeSelectButtons />
      </FlexColumn>
    )
  },
)

interface ErrorOverlayComponentProps {}

const ErrorOverlayComponent = betterReactMemo(
  'ErrorOverlayComponent',
  (props: ErrorOverlayComponentProps) => {
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

    const isHiddenUnderNavigator = useEditorState(
      (store) => store.editor.navigator.position === 'right',
      'ErrorOverlayComponent isOverlappingWithNavigator',
    )

    return (
      <ReactErrorOverlay
        currentBuildErrorRecords={errorRecords}
        currentRuntimeErrorRecords={overlayErrors}
        onOpenFile={onOpenFile}
        overlayOffset={isHiddenUnderNavigator ? LeftPaneDefaultWidth : 0}
      />
    )
  },
)

export const SafeModeErrorOverlay = betterReactMemo('SafeModeErrorOverlay', () => {
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
