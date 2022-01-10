import React from 'react'
import { ControlDescription } from 'utopia-api/core'
import { right } from '../../../../core/shared/either'
import { objectMap } from '../../../../core/shared/object-utils'
import { PropertyPath } from '../../../../core/shared/project-file-types'
import { ParseResult } from '../../../../utils/value-parser-utils'
import { CSSCursor } from '../../../canvas/canvas-types'
import { UIGridRow } from '../../widgets/ui-grid-row'
import { FolderSection } from './folder-section'
import * as PP from '../../../../core/shared/property-path'
import { RowForControl } from './component-section'

type RowOrFolderWrapperProps = {
  propPath: PropertyPath
  isScene: boolean
  setGlobalCursor: (cursor: CSSCursor | null) => void
  controlDescription: ControlDescription
  indentationLevel: number
  visibleEmptyControls: string[]
  unsetPropNames: string[]
  showHiddenControl: (path: string) => void
  focusOnMount?: boolean
}

export const RowOrFolderWrapper = React.memo((props: RowOrFolderWrapperProps) => {
  switch (props.controlDescription.control) {
    case 'folder':
      return (
        <FolderSection
          isRoot={false}
          indentationLevel={props.indentationLevel}
          parsedPropertyControls={objectMap(
            (c): ParseResult<ControlDescription> => right(c), // this is not the nicest, but the Either type inference is a bit limited
            props.controlDescription.controls,
          )}
          setGlobalCursor={props.setGlobalCursor}
          title={props.controlDescription.label ?? PP.toString(props.propPath)}
          visibleEmptyControls={props.visibleEmptyControls}
          unsetPropNames={props.unsetPropNames}
          showHiddenControl={props.showHiddenControl}
          detectedPropsAndValuesWithoutControls={{}}
        />
      )
    default:
      return (
        <UIGridRow
          padded
          tall={false}
          style={{ paddingLeft: 0 }}
          variant='<-------------1fr------------->'
        >
          <RowForControl
            propPath={props.propPath}
            controlDescription={props.controlDescription}
            isScene={props.isScene}
            setGlobalCursor={props.setGlobalCursor}
            indentationLevel={props.indentationLevel}
            focusOnMount={props.focusOnMount ?? false}
          />
        </UIGridRow>
      )
  }
})
