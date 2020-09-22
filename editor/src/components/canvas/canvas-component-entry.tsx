import * as React from 'react'
import { useEditorState } from '../editor/store/store-hook'
import {
  UiJsxCanvas,
  pickUiJsxCanvasProps,
  CanvasReactErrorCallback,
  CanvasErrorBoundary,
} from './ui-jsx-canvas'
import { betterReactMemo } from 'uuiui-deps'
import { saveDOMReport } from '../editor/actions/actions'
import { ElementInstanceMetadata } from '../../core/shared/element-template'
import { ConsoleLog } from '../editor/store/editor-state'
interface CanvasComponentEntryProps extends CanvasReactErrorCallback {
  clearConsoleLogs: () => void
  addToConsoleLogs: (log: ConsoleLog) => void
  canvasConsoleLogs: Array<ConsoleLog>
}

export const CanvasComponentEntry = betterReactMemo(
  'CanvasComponentEntry',
  (props: CanvasComponentEntryProps) => {
    const dispatch = useEditorState((store) => store.dispatch)
    const onDomReport = React.useCallback(
      (elementMetadata: Array<ElementInstanceMetadata>) => {
        dispatch([saveDOMReport(elementMetadata)])
      },
      [dispatch],
    )
    const { canvasProps } = useEditorState((store) => ({
      canvasProps: pickUiJsxCanvasProps(
        store.editor,
        store.derived,
        true,
        onDomReport,
        props.clearConsoleLogs,
        props.addToConsoleLogs,
        store.dispatch,
      ),
    }))

    if (canvasProps == null) {
      return null
    } else {
      return (
        <CanvasErrorBoundary
          fileCode={canvasProps.uiFileCode}
          filePath={canvasProps.uiFilePath}
          reportError={props.reportError}
        >
          <UiJsxCanvas {...canvasProps} clearErrors={props.clearErrors} />
        </CanvasErrorBoundary>
      )
    }
  },
)
