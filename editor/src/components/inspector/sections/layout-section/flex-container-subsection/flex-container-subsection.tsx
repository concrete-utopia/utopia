import React from 'react'
import { useInspectorLayoutInfo } from '../../../common/property-path-hooks'
import { FlexWrapControl } from './flex-container-controls'

export const FlexContainerControls = React.memo<{ seeMoreVisible: boolean }>((props) => {
  const flexWrap = useInspectorLayoutInfo('flexWrap')

  return (
    <FlexWrapControl
      value={flexWrap.value}
      onSubmitValue={flexWrap.onSubmitValue}
      onUnset={flexWrap.onUnsetValues}
      controlStatus={flexWrap.controlStatus}
      controlStyles={flexWrap.controlStyles}
    />
  )
})
