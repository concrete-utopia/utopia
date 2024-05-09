import React from 'react'
import type { ControlDescription } from '../../../custom-code/internal-property-controls'
import type { PropertyPath } from '../../../../core/shared/project-file-types'
import type { CSSCursor } from '../../../canvas/canvas-types'
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
        showHiddenControl={props.showHiddenControl}
      />
    </UIGridRow>
  )

  // switch (props.controlDescription.control) {
  //   case 'folder':
  //     return (
  //       <FolderSection
  //         isRoot={false}
  //         indentationLevel={props.indentationLevel}
  //         propertyControls={props.controlDescription.controls}
  //         setGlobalCursor={props.setGlobalCursor}
  //         title={props.controlDescription.label ?? PP.toString(props.propPath)}
  //         visibleEmptyControls={props.visibleEmptyControls}
  //         unsetPropNames={props.unsetPropNames}
  //         showHiddenControl={props.showHiddenControl}
  //         detectedPropsAndValuesWithoutControls={{}}
  //       />
  //     )
  //   default:
  // }
})
