import React from 'react'
import type { ControlDescription } from '../../../custom-code/internal-property-controls'
import type { PropertyPath } from '../../../../core/shared/project-file-types'
import type { CSSCursor } from '../../../canvas/canvas-types'
import { UIGridRow } from '../../widgets/ui-grid-row'
import { RowForControl } from './component-section'

type RowOrFolderWrapperProps = {
  propPath: PropertyPath
  isScene: boolean
  setGlobalCursor: (cursor: CSSCursor | null) => void
  controlDescription: ControlDescription
  indentationLevel: number
  shouldIncreaseIdentation: boolean
  visibleEmptyControls: string[]
  unsetPropNames: string[]
  showHiddenControl: (path: string) => void
  focusOnMount?: boolean
}

export const RowOrFolderWrapper = React.memo((props: RowOrFolderWrapperProps) => {
  if (props.controlDescription.control === 'none') {
    return null
  }
  return (
    <UIGridRow padded={false} tall={false} variant='<-------------1fr------------->'>
      <RowForControl
        propPath={props.propPath}
        controlDescription={props.controlDescription}
        isScene={props.isScene}
        setGlobalCursor={props.setGlobalCursor}
        indentationLevel={props.indentationLevel}
        shouldIncreaseIdentation={props.shouldIncreaseIdentation}
        focusOnMount={props.focusOnMount ?? false}
        showHiddenControl={props.showHiddenControl}
      />
    </UIGridRow>
  )
})
RowOrFolderWrapper.displayName = 'RowOrFolderWrapper'
