import React from 'react'
import { useEditorState } from '../editor/store/store-hook'
import { FlexDirectionToggle } from './flex-direction-control'
import { selectedViewsSelector, metadataSelector } from './inpector-selectors'
import { detectFlexDirection } from './inspector-common'
import { NineBlockControl } from './nine-block-controls'

export const FlexSection = React.memo(() => {
  const flexDirection = useEditorState(
    (store) =>
      selectedViewsSelector(store).length === 0
        ? 'row'
        : detectFlexDirection(metadataSelector(store), selectedViewsSelector(store)),
    'FlexSection flexDirection',
  )

  return (
    <div>
      <FlexDirectionToggle flexDirection={flexDirection} />
      <NineBlockControl flexDirection={flexDirection} />
    </div>
  )
})
