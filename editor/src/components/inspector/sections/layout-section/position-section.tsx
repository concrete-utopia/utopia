import React from 'react'
import { emptySpecialSizeMeasurements } from '../../../../core/shared/element-template'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { fastForEach } from '../../../../core/shared/utils'
import * as EP from '../../../../core/shared/element-path'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { SpecialSizeMeasurementsKeepDeepEquality } from '../../../editor/store/store-deep-equality-instances'
import { LayoutSubsection } from './self-layout-subsection/self-layout-subsection'
import { setInspectorLayoutSectionHovered } from '../../../editor/actions/action-creators'
import { useDispatch } from '../../../editor/store/dispatch-context'

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

  const dispatch = useDispatch()

  return (
    <div
      onMouseOver={() => {
        dispatch([setInspectorLayoutSectionHovered(true)], 'everyone')
      }}
      onMouseOut={() => {
        dispatch([setInspectorLayoutSectionHovered(false)], 'everyone')
      }}
    >
      <LayoutSubsection
        position={specialSizeMeasurements.position}
        parentLayoutSystem={specialSizeMeasurements.parentLayoutSystem}
        parentFlexDirection={specialSizeMeasurements.parentFlexDirection}
        aspectRatioLocked={props.aspectRatioLocked}
        toggleAspectRatioLock={props.toggleAspectRatioLock}
      />
    </div>
  )
})
