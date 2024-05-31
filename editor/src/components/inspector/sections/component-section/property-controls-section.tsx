import React from 'react'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { PropertyControls } from '../../../custom-code/internal-property-controls'
import { setCursorOverlay } from '../../../editor/actions/action-creators'
import { useKeepReferenceEqualityIfPossible } from '../../../../utils/react-performance'
import { useHiddenElements } from './hidden-controls-section'
import { FolderSection } from './folder-section'
import type { CSSCursor } from '../../../canvas/canvas-types'
import { UIGridRow } from '../../widgets/ui-grid-row'
import { Subdued, VerySubdued } from '../../../../uuiui'
import {
  AdvancedFolderLabel,
  isAdvancedFolderLabel,
  specialPropertiesToIgnore,
} from '../../../../core/property-controls/property-controls-utils'
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

  const propertiesWithFolders = synthesiseFolders(propertyControls)

  // unify the logic from the sub components
  const hasContent = React.useMemo(
    () =>
      (Object.keys(detectedPropsAndValuesWithoutControls).length > 0 &&
        Object.keys(detectedPropsAndValuesWithoutControls).some(
          (prop) => !specialPropertiesToIgnore.includes(prop),
        )) ||
      propertiesWithFolders.folders.length > 0 ||
      Object.keys(propertiesWithFolders.advanced).length > 0 ||
      filteredDetectedPropsWithNoValue.length > 0 ||
      Object.keys(propertiesWithFolders.uncategorized).length > 0 ||
      propsWithControlsButNoValue.length > 0,
    [
      detectedPropsAndValuesWithoutControls,
      propertiesWithFolders.folders,
      propertiesWithFolders.advanced,
      filteredDetectedPropsWithNoValue,
      propertiesWithFolders.uncategorized,
      propsWithControlsButNoValue.length,
    ],
  )

  return hasContent ? (
    <>
      <FolderSection
        isRoot={true}
        indentationLevel={0}
        propertyControls={propertiesWithFolders.uncategorized}
        setGlobalCursor={setGlobalCursor}
        visibleEmptyControls={visibleEmptyControls}
        unsetPropNames={propsWithControlsButNoValue}
        showHiddenControl={showHiddenControl}
        detectedPropsAndValuesWithoutControls={detectedPropsAndValuesWithoutControls}
      />
      {propertiesWithFolders.folders.map(({ name, controls }) => (
        <FolderSection
          key={name}
          isRoot={false}
          indentationLevel={2}
          propertyControls={controls}
          setGlobalCursor={setGlobalCursor}
          visibleEmptyControls={visibleEmptyControls}
          unsetPropNames={propsWithControlsButNoValue}
          showHiddenControl={showHiddenControl}
          detectedPropsAndValuesWithoutControls={detectedPropsAndValuesWithoutControls}
          title={name}
        />
      ))}
      {Object.keys(propertiesWithFolders.advanced).length === 0 ? null : (
        <FolderSection
          isRoot={false}
          indentationLevel={2}
          propertyControls={propertiesWithFolders.advanced}
          setGlobalCursor={setGlobalCursor}
          visibleEmptyControls={visibleEmptyControls}
          unsetPropNames={propsWithControlsButNoValue}
          showHiddenControl={showHiddenControl}
          detectedPropsAndValuesWithoutControls={detectedPropsAndValuesWithoutControls}
          title={AdvancedFolderLabel}
        />
      )}
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
  ) : (
    <UIGridRow padded tall={false} variant={'<-------------1fr------------->'}>
      <div>
        <Subdued>This component requires no configuration</Subdued>
      </div>
    </UIGridRow>
  )
})

interface SyntheticFoldersResult {
  uncategorized: PropertyControls
  advanced: PropertyControls
  folders: Array<{ name: string; controls: PropertyControls }>
}

function synthesiseFolders(controls: PropertyControls): SyntheticFoldersResult {
  const uncategorized: PropertyControls = {}
  const advanced: PropertyControls = {}
  const folders: Array<{ name: string; controls: PropertyControls }> = []

  Object.entries(controls).forEach(([prop, control]) => {
    if (control.folder == null) {
      uncategorized[prop] = control
      return
    }

    if (isAdvancedFolderLabel(control.folder)) {
      advanced[prop] = control
      return
    }

    const maybeExistingFolder = folders.find((f) => f.name === control.folder)

    if (maybeExistingFolder == null) {
      folders.push({ name: control.folder, controls: { [prop]: control } })
      return
    }

    maybeExistingFolder.controls[prop] = control
  })

  return { advanced, uncategorized, folders }
}
