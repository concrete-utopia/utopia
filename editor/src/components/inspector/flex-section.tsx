import React from 'react'
import { useEditorState } from '../editor/store/store-hook'
import { FlexDirectionToggle } from './flex-direction-control'
import { detectFlexDirection } from './inspector-common'
import { NineBlockControl } from './nine-block-controls'

export const FlexSection = React.memo(() => {
  const metadata = useEditorState((store) => store.editor.jsxMetadata, 'FlexSection metadata')
  const selectedViews = useEditorState(
    (store) => store.editor.selectedViews,
    'FlexSection selectedViews',
  )

  const flexDirection = detectFlexDirection(metadata, selectedViews[0])

  return (
    <div>
      <FlexDirectionToggle flexDirection={flexDirection} />
      <NineBlockControl flexDirection={flexDirection} />
    </div>
  )
})
