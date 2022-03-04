import React from 'react'
import { ElementPath } from '../../core/shared/project-file-types'
import { DistanceGuideline } from './controls/distance-guideline2'
import { AbsoluteResizeControl } from './controls/select-mode/absolute-resize-control'
import { FlexResizeControl } from './controls/select-mode/flex-resize-control'
import { MultiSelectOutlineControl } from './controls/select-mode/simple-outline-control'

interface ControlsContainerProps {
  localSelectedElements: ElementPath[]
}

export const ControlsContainer = React.memo<ControlsContainerProps>((props) => {
  return (
    <div
      style={{
        position: 'absolute',
        transform: `translate(var(--utopia-canvas-offset-x), var(--utopia-canvas-offset-y))`,
      }}
    >
      <MultiSelectOutlineControl localSelectedElements={props.localSelectedElements} />
      <DistanceGuideline localSelectedElements={props.localSelectedElements} />
      {Array.from(Array(10).keys()).map((v, i) => {
        return <AbsoluteResizeControl key={i} localSelectedElements={props.localSelectedElements} />
      })}
      <FlexResizeControl localSelectedElements={props.localSelectedElements} />
    </div>
  )
})
