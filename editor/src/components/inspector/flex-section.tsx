import React from 'react'
import { createSelector } from 'reselect'
import { when } from '../../utils/react-conditionals'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { AddRemoveLayouSystemControl } from './add-remove-layout-system-control'
import { FlexDirectionToggle } from './flex-direction-control'
import { selectedViewsSelector, metadataSelector } from './inpector-selectors'
import {
  detectAreElementsFlexContainers,
  detectPackedSpacedSetting,
  PackedSpaced,
} from './inspector-common'
import { NineBlockControl } from './nine-block-controls'
import { UIGridRow } from './widgets/ui-grid-row'
import { PaddingRow } from '../../components/inspector/sections/layout-section/layout-system-subsection/layout-system-controls'
import { SpacedPackedControl } from './spaced-packed-control'
import { ThreeBarControl } from './sections/three-bar-control'
import { assertNever } from '../../core/shared/utils'

const areElementsFlexContainersSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  detectAreElementsFlexContainers,
)

const packedFlexSettingSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  detectPackedSpacedSetting,
)

export const FlexSection = React.memo(() => {
  const allElementsInFlexLayout = useEditorState(
    Substores.metadata,
    areElementsFlexContainersSelector,
    'FlexSection areAllElementsInFlexLayout',
  )

  const packedSpacedSetting =
    useEditorState(
      Substores.metadata,
      packedFlexSettingSelector,
      'FlexSection packedFlexSetting',
    ) ?? 'packed'

  function BarControl(setting: PackedSpaced) {
    switch (setting) {
      case 'packed':
        return <NineBlockControl />
      case 'spaced':
        return <ThreeBarControl />
      default:
        assertNever(setting)
    }
  }

  return (
    <div>
      <AddRemoveLayouSystemControl />
      {when(
        allElementsInFlexLayout,
        <>
          <UIGridRow padded variant='<-------------1fr------------->'>
            <FlexDirectionToggle />
            <SpacedPackedControl />
          </UIGridRow>
          <UIGridRow padded variant='<-------------1fr------------->'>
            {BarControl(packedSpacedSetting)}
          </UIGridRow>
          <PaddingRow />
        </>,
      )}
    </div>
  )
})
