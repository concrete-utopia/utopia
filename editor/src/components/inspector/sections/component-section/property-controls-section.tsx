import React from 'react'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { PropertyControls } from '../../../custom-code/internal-property-controls'
import { useEditorState } from '../../../editor/store/store-hook'
import { setCursorOverlay } from '../../../editor/actions/action-creators'
import { useKeepReferenceEqualityIfPossible } from '../../../../utils/react-performance'
import { useHiddenElements } from './hidden-controls-section'
import { FolderSection } from './folder-section'
import type { CSSCursor } from '../../../canvas/canvas-types'
import { UIGridRow } from '../../widgets/ui-grid-row'
import { VerySubdued } from '../../../../uuiui'
import { specialPropertiesToIgnore } from '../../../../core/property-controls/property-controls-utils'
import { useDispatch } from '../../../editor/store/dispatch-context'

interface PropertyControlsSectionProps {
  targets: ElementPath[]
  propertyControls: PropertyControls
  detectedPropsWithNoValue: string[]
  detectedPropsAndValuesWithoutControls: Record<string, unknown>
  propsWithControlsButNoValue: string[]
  isScene: boolean
}

export const PropertyControlsSection = React.memo((props: PropertyControlsSectionProps) => {
  const {
    propertyControls,
    detectedPropsWithNoValue,
    detectedPropsAndValuesWithoutControls,
    propsWithControlsButNoValue,
  } = props

  // Filter out these because we don't want to include them in the unused props.
  const filteredDetectedPropsWithNoValue = useKeepReferenceEqualityIfPossible(
    detectedPropsWithNoValue.filter((detectedPropWithNoValue) => {
      return !specialPropertiesToIgnore.includes(detectedPropWithNoValue)
    }),
  )

  const dispatch = useDispatch()
  const setGlobalCursor = React.useCallback(
    (cursor: CSSCursor | null) => {
      dispatch([setCursorOverlay(cursor)], 'everyone')
    },
    [dispatch],
  )

  const [visibleEmptyControls, showHiddenControl] = useHiddenElements()

  const rootFolder = (
    <FolderSection
      isRoot={true}
      indentationLevel={0}
      propertyControls={propertyControls}
      setGlobalCursor={setGlobalCursor}
      visibleEmptyControls={visibleEmptyControls}
      unsetPropNames={propsWithControlsButNoValue}
      showHiddenControl={showHiddenControl}
      detectedPropsAndValuesWithoutControls={detectedPropsAndValuesWithoutControls}
    />
  )

  return (
    <>
      {rootFolder}
      {/** props set on the component instance and props used inside the component code */}
      {filteredDetectedPropsWithNoValue.length > 0 ? (
        <UIGridRow padded tall={false} variant={'<-------------1fr------------->'}>
          <div>
            <VerySubdued>{`Unused props: ${filteredDetectedPropsWithNoValue.join(
              ', ',
            )}.`}</VerySubdued>
          </div>
        </UIGridRow>
      ) : null}
    </>
  )
})
