import * as React from 'react'
import { LayoutSystemSubsection } from './layout-system-subsection/layout-system-subsection'
import { SelfLayoutSubsection } from './self-layout-subsection/self-layout-subsection'
import { emptySpecialSizeMeasurements } from '../../../../core/shared/element-template'
import { betterReactMemo } from '../../../../uuiui-deps'
import { useEditorState } from '../../../editor/store/store-hook'
import { fastForEach } from '../../../../core/shared/utils'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { SpecialSizeMeasurementsKeepDeepEquality } from '../../../editor/store/store-deep-equality-instances'

interface LayoutSectionProps {
  hasNonDefaultPositionAttributes: boolean
  aspectRatioLocked: boolean
  toggleAspectRatioLock: () => void
}

export const LayoutSection = betterReactMemo('LayoutSection', (props: LayoutSectionProps) => {
  const specialSizeMeasurements = useEditorState(
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
    'RenderedLayoutSection specialSizeMeasurements',
    (old, next) => SpecialSizeMeasurementsKeepDeepEquality()(old, next).areEqual,
  )

  return (
    <>
      <SelfLayoutSubsection
        position={specialSizeMeasurements.position}
        isChildOfFlexComponent={specialSizeMeasurements.parentLayoutSystem === 'flex'}
        parentFlexDirection={specialSizeMeasurements.parentFlexDirection}
        aspectRatioLocked={props.aspectRatioLocked}
        toggleAspectRatioLock={props.toggleAspectRatioLock}
      />
      <LayoutSystemSubsection specialSizeMeasurements={specialSizeMeasurements} />
    </>
  )
})
