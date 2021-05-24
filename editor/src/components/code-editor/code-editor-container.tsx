import * as React from 'react'
import { betterReactMemo } from '../../uuiui-deps'
import { useEditorState } from '../editor/store/store-hook'
import { MONACO_EDITOR_IFRAME_BASE_URL } from '../../common/env-vars'
import { createIframeUrl } from '../../core/shared/utils'
import { forceNotNull } from '../../core/shared/optional-utils'
import { reduxDevtoolsLogMessage } from '../../core/shared/redux-devtools'

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
      vscodeBridgeId: store.editor.vscodeBridgeId,
      vscodeBridgeReady: store.editor.vscodeBridgeReady,
    }
  }, 'CodeEditorWrapper')

  if (selectedProps.vscodeBridgeReady) {
    return (
      <VSCodeIframeContainer
        projectID={forceNotNull(
          'VSCode Bridge ID must be set when launching the code editor',
          selectedProps.vscodeBridgeId,
        )}
      />
    )
  } else {
    return <div>Loading...</div>
  }
})
