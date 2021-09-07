import * as React from 'react'
import { betterReactMemo } from '../../uuiui-deps'
import { useEditorState } from '../editor/store/store-hook'
import { MONACO_EDITOR_IFRAME_BASE_URL } from '../../common/env-vars'
import { createIframeUrl } from '../../core/shared/utils'
import { getUnderlyingVSCodeBridgeID } from '../editor/store/editor-state'

const VSCodeIframeContainer = betterReactMemo(
  'VSCodeIframeContainer',
  (props: { projectID: string }) => {
    const projectID = props.projectID
    const baseIframeSrc = createIframeUrl(
      MONACO_EDITOR_IFRAME_BASE_URL,
      'vscode-editor-outer-iframe',
    )
    const url = new URL(baseIframeSrc)
    url.searchParams.append('project_id', projectID)

    return (
      <iframe
        key={'vscode-editor'}
        id={'vscode-editor'}
        src={url.toString()}
        allow='autoplay'
        style={{
          flex: 1,
          backgroundColor: 'transparent',
          borderWidth: 0,
        }}
      />
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
