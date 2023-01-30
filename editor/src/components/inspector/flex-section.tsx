import React from 'react'
import { createSelector } from 'reselect'
import { when } from '../../utils/react-conditionals'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { AddRemoveLayouSystemControl } from './add-remove-layout-system-control'
import { FlexDirectionToggle } from './flex-direction-control'
import { selectedViewsSelector, metadataSelector } from './inpector-selectors'
import { detectAreElementsFlexContainers } from './inspector-common'
import { NineBlockControl } from './nine-block-controls'
import { UIGridRow } from './widgets/ui-grid-row'
import { PaddingRow } from '../../components/inspector/sections/layout-section/layout-system-subsection/layout-system-controls'

const areElementsFlexContainersSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  detectAreElementsFlexContainers,
)

export const FlexSection = React.memo(() => {
  const allElementsInFlexLayout = useEditorState(
    Substores.metadata,
    areElementsFlexContainersSelector,
    'FlexSection areAllElementsInFlexLayout',
  )

  return (
    <div>
      <AddRemoveLayouSystemControl />
      {when(
        allElementsInFlexLayout,
        <>
          <UIGridRow padded variant='<-------------1fr------------->'>
            <FlexDirectionToggle />
          </UIGridRow>
          <UIGridRow padded variant='<-------------1fr------------->'>
            <NineBlockControl />
          </UIGridRow>
          <PaddingRow />
        </>,
      )}
    </div>
  )
})
