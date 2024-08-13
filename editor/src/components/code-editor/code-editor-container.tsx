import React from 'react'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { MONACO_EDITOR_IFRAME_BASE_URL } from '../../common/env-vars'
import { createIframeUrl } from '../../core/shared/utils'
import { getUnderlyingVSCodeBridgeID } from '../editor/store/editor-state'
import { VSCodeLoadingScreen } from './vscode-editor-loading-screen'
import { getEditorBranchNameFromURL, setBranchNameFromURL } from '../../utils/branches'
import { VSCODE_EDITOR_IFRAME_ID } from '../../core/vscode/vscode-bridge'

const VSCodeIframeContainer = React.memo((props: { vsCodeSessionID: string }) => {
  const vsCodeSessionID = props.vsCodeSessionID
  const baseIframeSrc = createIframeUrl(MONACO_EDITOR_IFRAME_BASE_URL, 'vscode-editor-iframe/')
  const url = new URL(baseIframeSrc)
  url.searchParams.append('vs_code_session_id', vsCodeSessionID)

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
          id={VSCODE_EDITOR_IFRAME_ID}
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
})

export const CodeEditorWrapper = React.memo(() => {
  const selectedProps = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return {
        vscodeBridgeId: getUnderlyingVSCodeBridgeID(),
      }
    },
    'CodeEditorWrapper',
  )

  return <VSCodeIframeContainer vsCodeSessionID={selectedProps.vscodeBridgeId} />
})
