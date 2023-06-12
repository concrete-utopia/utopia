import React from 'react'
import { emptySpecialSizeMeasurements } from '../../../../core/shared/element-template'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { fastForEach } from '../../../../core/shared/utils'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { SpecialSizeMeasurementsKeepDeepEquality } from '../../../editor/store/store-deep-equality-instances'
import { LayoutSubsection } from './self-layout-subsection/self-layout-subsection'

interface LayoutSectionProps {
  hasNonDefaultPositionAttributes: boolean
  aspectRatioLocked: boolean
  toggleAspectRatioLock: () => void
}

export const PositionSection = React.memo((props: LayoutSectionProps) => {
  const specialSizeMeasurements = useEditorState(
    Substores.metadata,
    (state) => {
      let foundSpecialSizeMeasurements = emptySpecialSizeMeasurements
      fastForEach(state.editor.selectedViews, (path) => {
        // TODO multiselect
        const jsxMetadata = state.editor.jsxMetadata
        const elementMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, path)
        if (elementMetadata != null) {
          foundSpecialSizeMeasurements = elementMetadata.specialSizeMeasurements
        }
      })
      return foundSpecialSizeMeasurements
    },
    'LayoutSection specialSizeMeasurements',
    (old, next) => SpecialSizeMeasurementsKeepDeepEquality()(old, next).areEqual,
  )

  return (
    <LayoutSubsection
      position={specialSizeMeasurements.position}
      parentLayoutSystem={specialSizeMeasurements.parentLayoutSystem}
      parentFlexDirection={specialSizeMeasurements.parentFlexDirection}
      aspectRatioLocked={props.aspectRatioLocked}
      toggleAspectRatioLock={props.toggleAspectRatioLock}
    />
  )
})
