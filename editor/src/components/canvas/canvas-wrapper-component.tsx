import React from 'react'
import { usePubSubAtomWriteOnly } from '../../core/shared/atom-with-pub-sub'
import { useErrorOverlayRecords } from '../../core/shared/runtime-report-logs'
import { NO_OP } from '../../core/shared/utils'
import { EditorCanvas } from '../../templates/editor-canvas'
import CloseButton from '../../third-party/react-error-overlay/components/CloseButton'
import ErrorOverlay from '../../third-party/react-error-overlay/components/ErrorOverlay'
import Footer from '../../third-party/react-error-overlay/components/Footer'
import Header from '../../third-party/react-error-overlay/components/Header'
import { Button, FlexColumn, FlexRow, UtopiaTheme } from '../../uuiui'
import { clearPostActionData, setSafeMode } from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import {
  CanvasSizeAtom,
  createCanvasModelKILLME,
  getAllCodeEditorErrors,
} from '../editor/store/editor-state'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { shouldShowErrorOverlay } from './canvas-utils'
import { FloatingPostActionMenu } from './controls/select-mode/post-action-menu'

export const CanvasWrapperComponentId = 'canvas-wrapper-component'

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

  const scale = useEditorState(Substores.canvas, (store) => store.editor.canvas.scale, 'scale')

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
      id={CanvasWrapperComponentId}
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
          derived={derivedState}
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
