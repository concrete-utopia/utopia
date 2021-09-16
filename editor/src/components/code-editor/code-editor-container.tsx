import * as React from 'react'
import { betterReactMemo } from '../../uuiui-deps'
import { useEditorState } from '../editor/store/store-hook'
import { MONACO_EDITOR_IFRAME_BASE_URL } from '../../common/env-vars'
import { createIframeUrl } from '../../core/shared/utils'
import { getUnderlyingVSCodeBridgeID } from '../editor/store/editor-state'
import { VSCodeLoadingScreen } from './vscode-editor-loading-screen'
import { unless } from '../../utils/react-conditionals'

const VSCodeIframeContainer = betterReactMemo(
  'VSCodeIframeContainer',
  (props: { projectID: string; vscodeBridgeReady: boolean }) => {
    const projectID = props.projectID
    const baseIframeSrc = createIframeUrl(
      MONACO_EDITOR_IFRAME_BASE_URL,
      'vscode-editor-outer-iframe',
    )
    const url = new URL(baseIframeSrc)
    url.searchParams.append('project_id', projectID)

    return (
      <div
        style={{
          flex: 1,
        }}
      >
        {unless(props.vscodeBridgeReady, <VSCodeLoadingScreen />)}
        <iframe
          key={'vscode-editor'}
          id={'vscode-editor'}
          src={url.toString()}
          allow='autoplay'
          style={{
            width: '100%',
            height: '100%',
            backgroundColor: 'transparent',
            borderWidth: 0,
          }}
        />
      </div>
    )
  },
)

export const CodeEditorWrapper = betterReactMemo('CodeEditorWrapper', () => {
  const selectedProps = useEditorState((store) => {
    return {
      vscodeBridgeId: getUnderlyingVSCodeBridgeID(store.editor.vscodeBridgeId),
      vscodeBridgeReady: store.editor.vscodeBridgeReady,
    }
  }, 'CodeEditorWrapper')

  return (
    <VSCodeIframeContainer
      projectID={selectedProps.vscodeBridgeId}
      vscodeBridgeReady={selectedProps.vscodeBridgeReady}
    />
  )
})
