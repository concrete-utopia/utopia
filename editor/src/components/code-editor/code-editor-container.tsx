import * as React from 'react'
import { betterReactMemo } from '../../uuiui-deps'
import { useEditorState } from '../editor/store/store-hook'
import { MONACO_EDITOR_IFRAME_BASE_URL } from '../../common/env-vars'
import { createIframeUrl } from '../../core/shared/utils'
import { getUnderlyingVSCodeBridgeID } from '../editor/store/editor-state'
import { VSCodeLoadingScreen } from './vscode-editor-loading-screen'
import { when } from '../../utils/react-conditionals'
import { getEditorBranchNameFromURL, setBranchNameFromURL } from '../../utils/branches'

const VSCodeIframeContainer = betterReactMemo(
  'VSCodeIframeContainer',
  (props: { projectID: string; vscodeLoadingScreenVisible: boolean }) => {
    const projectID = props.projectID
    const baseIframeSrc = createIframeUrl(
      MONACO_EDITOR_IFRAME_BASE_URL,
      'vscode-editor-outer-iframe',
    )
    const url = new URL(baseIframeSrc)
    url.searchParams.append('project_id', projectID)

    setBranchNameFromURL(url.searchParams)
    return (
      <div
        style={{
          flex: 1,
        }}
      >
        {when(props.vscodeLoadingScreenVisible, <VSCodeLoadingScreen />)}
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
      vscodeLoadingScreenVisible: store.editor.vscodeLoadingScreenVisible,
    }
  }, 'CodeEditorWrapper')

  return (
    <VSCodeIframeContainer
      projectID={selectedProps.vscodeBridgeId}
      vscodeLoadingScreenVisible={selectedProps.vscodeLoadingScreenVisible}
    />
  )
})
