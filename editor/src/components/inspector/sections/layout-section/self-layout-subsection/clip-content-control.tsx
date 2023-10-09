import React from 'react'
import { useInspectorStyleInfo, useIsSubSectionVisible } from '../../../common/property-path-hooks'
import { BooleanControl } from '../../../controls/boolean-control'

export const ClipContentControl = React.memo(() => {
  const isVisible = useIsSubSectionVisible('overflow')

  const { value, controlStatus, controlStyles, onSubmitValue, onUnsetValues } =
    useInspectorStyleInfo(
      'overflow',
      (parsed) => !parsed.overflow,
      (clipContent: boolean) => ({
        overflow: !clipContent,
      }),
    )

  if (!isVisible) {
    return null
  }

  return (
    <>
      <BooleanControl
        key='clip-content-control'
        id='clip-content-control'
        testId='clip-content-control'
        value={value}
        controlStatus={controlStatus}
        controlStyles={controlStyles}
        onSubmitValue={onSubmitValue}
      />
      Clip content
    </>
  )
})
