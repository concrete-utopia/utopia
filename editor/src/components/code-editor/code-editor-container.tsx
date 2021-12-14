import React from 'react'
import { betterReactMemo } from '../../uuiui-deps'
import { useEditorState } from '../editor/store/store-hook'
import { MONACO_EDITOR_IFRAME_BASE_URL } from '../../common/env-vars'
import { createIframeUrl } from '../../core/shared/utils'
import { getUnderlyingVSCodeBridgeID } from '../editor/store/editor-state'
import { VSCodeLoadingScreen } from './vscode-editor-loading-screen'
import { getEditorBranchNameFromURL, setBranchNameFromURL } from '../../utils/branches'

const VSCodeIframeContainer = betterReactMemo(
  'VSCodeIframeContainer',
  (props: { projectID: string }) => {
    const projectID = props.projectID
    const baseIframeSrc = createIframeUrl(
      MONACO_EDITOR_IFRAME_BASE_URL,
      'vscode-editor-outer-iframe',
    )
    // const url = new URL(baseIframeSrc)
    const url = new URL('http://localhost:8000/editor/vscode-outer/')
    url.searchParams.append('project_id', projectID)

    setBranchNameFromURL(url.searchParams)

    return (
      <div
        style={{
          flex: 1,
        }}
      >
        <VSCodeLoadingScreen />
        {/* for Karma tests, we skip creating this iframe to avoid hitting a 404 */}
        {window.KarmaTestEnvironment ? null : (
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
        )}
      </div>
    )
  },
)

export const CodeEditorWrapper = betterReactMemo('CodeEditorWrapper', () => {
  const selectedProps = useEditorState((store) => {
    return {
      vscodeBridgeId: getUnderlyingVSCodeBridgeID(store.editor.vscodeBridgeId),
    }
  }, 'CodeEditorWrapper')

  return <VSCodeIframeContainer projectID={selectedProps.vscodeBridgeId} />
})
