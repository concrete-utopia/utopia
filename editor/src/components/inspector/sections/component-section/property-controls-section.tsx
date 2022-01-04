import React from 'react'
import { useContext } from 'use-context-selector'
import { UTOPIA_PATHS_KEY, UTOPIA_UIDS_KEY } from '../../../../core/model/utopia-constants'
import { eitherToMaybe, foldEither } from '../../../../core/shared/either'
import { getJSXAttribute } from '../../../../core/shared/element-template'
import { jsxSimpleAttributeToValue } from '../../../../core/shared/jsx-attributes'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { InspectorPropsContext, InspectorPropsContextData } from '../../common/property-path-hooks'
import * as EP from '../../../../core/shared/element-path'
import { ParseResult } from '../../../../utils/value-parser-utils'
import { ParsedPropertyControls } from '../../../../core/property-controls/property-controls-parser'
import { useEditorState } from '../../../editor/store/store-hook'
import { setCursorOverlay } from '../../../editor/actions/action-creators'
import { useKeepReferenceEqualityIfPossible } from '../../../../utils/react-performance'
import { useHiddenElements } from './hidden-controls-section'
import { ParseErrorControl } from './component-section'
import { FolderSection } from './folder-section'
import { CSSCursor } from '../../../canvas/canvas-types'
import { UIGridRow } from '../../widgets/ui-grid-row'
import { VerySubdued } from '../../../../uuiui'
import { specialPropertiesToIgnore } from '../../../../core/property-controls/property-controls-utils'

function useFilterPropsContext(paths: ElementPath[]): InspectorPropsContextData {
  const currentContext = useContext(InspectorPropsContext)
  const spiedProps = currentContext.spiedProps.filter((props) =>
    paths.some((path) => EP.toString(path) === props[UTOPIA_PATHS_KEY]),
  )
  const editedMultiSelectedProps = currentContext.editedMultiSelectedProps.filter((attributes) => {
    const dataUidAttribute = getJSXAttribute(attributes, UTOPIA_UIDS_KEY)
    if (dataUidAttribute != null) {
      const uid = eitherToMaybe(jsxSimpleAttributeToValue(dataUidAttribute))
      return paths.some((path) => EP.toUid(path) === uid)
    } else {
      return false
    }
  })

  return {
    ...currentContext,
    spiedProps,
    editedMultiSelectedProps,
    selectedViews: paths,
  }
}

interface PropertyControlsSectionProps {
  targets: ElementPath[]
  propertyControls: ParseResult<ParsedPropertyControls>
  detectedPropsWithNoValue: string[]
  detectedPropsAndValuesWithoutControls: Record<string, unknown>
  propsWithControlsButNoValue: string[]
  isScene: boolean
}

export const PropertyControlsSection = React.memo((props: PropertyControlsSectionProps) => {
  const {
    targets,
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

  const dispatch = useEditorState((state) => state.dispatch, 'ComponentSectionInner')
  const setGlobalCursor = React.useCallback(
    (cursor: CSSCursor | null) => {
      dispatch([setCursorOverlay(cursor)], 'everyone')
    },
    [dispatch],
  )

  const updatedContext = useKeepReferenceEqualityIfPossible(useFilterPropsContext(targets))
  const [visibleEmptyControls, showHiddenControl] = useHiddenElements()

  const rootFolder = foldEither(
    (rootParseError) => {
      return <ParseErrorControl parseError={rootParseError} />
    },
    (rootParseSuccess) => {
      return (
        <FolderSection
          isRoot={true}
          indentationLevel={0}
          parsedPropertyControls={rootParseSuccess}
          setGlobalCursor={setGlobalCursor}
          visibleEmptyControls={visibleEmptyControls}
          unsetPropNames={propsWithControlsButNoValue}
          showHiddenControl={showHiddenControl}
          detectedPropsAndValuesWithoutControls={detectedPropsAndValuesWithoutControls}
        />
      )
    },
    propertyControls,
  )

  return (
    <InspectorPropsContext.Provider value={updatedContext}>
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
    </InspectorPropsContext.Provider>
  )
})
