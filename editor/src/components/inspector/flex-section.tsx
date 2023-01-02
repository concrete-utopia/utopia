import React from 'react'
import { createSelector } from 'reselect'
import { when } from '../../utils/react-conditionals'
import { useEditorState } from '../editor/store/store-hook'
import { AddRemoveLayouSystemControl } from './add-remove-layout-system-control'
import { FlexDirectionToggle } from './flex-direction-control'
import { selectedViewsSelector, metadataSelector } from './inpector-selectors'
import {
  DefaultFlexDirection,
  detectAreElementsFlexContainers,
  detectFlexDirection,
} from './inspector-common'
import { NineBlockControl } from './nine-block-controls'

const areElementsFlexContainersSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  detectAreElementsFlexContainers,
)

export const FlexSection = React.memo(() => {
  const allElementsInFlexLayout = useEditorState(
    areElementsFlexContainersSelector,
    'FlexSection areAllElementsInFlexLayout',
  )

  return (
    <>
      {when(
        allElementsInFlexLayout,
        <div>
          <AddRemoveLayouSystemControl />
          <FlexDirectionToggle />
          <NineBlockControl />
        </div>,
      )}
    </>
  )
})
