import React from 'react'
import { useEditorState } from '../editor/store/store-hook'
import { AddRemoveLayouSystemControl } from './add-remove-layout-system-control'
import { FlexDirectionToggle } from './flex-direction-control'
import { selectedViewsSelector, metadataSelector } from './inpector-selectors'
import { detectFlexDirection } from './inspector-common'
import { NineBlockControl } from './nine-block-controls'

export const FlexSection = React.memo(() => {
  const flexDirection = useEditorState(
    (store) =>
      selectedViewsSelector(store).length === 0
        ? 'row'
        : detectFlexDirection(metadataSelector(store), selectedViewsSelector(store)[0]),
    'FlexSection flexDirection',
  )

  return (
    <div>
      <AddRemoveLayouSystemControl />
      <FlexDirectionToggle flexDirection={flexDirection} />
      <NineBlockControl flexDirection={flexDirection} />
    </div>
  )
})
