import * as React from 'react'
import { InspectorSubsectionHeader } from '../../../../../uuiui'
import { usePropControlledState, betterReactMemo } from '../../../../../uuiui-deps'
import { CSSPosition } from '../../../common/css-utils'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { FlexElementSubsection } from '../flex-element-subsection/flex-element-subsection'
import { GiganticSizePinsSubsection } from './gigantic-size-pins-subsection'
import { LayoutTypePicker } from './self-layout-controls'

export type SelfLayoutTab = 'absolute' | 'flex' | 'flow' | 'sticky'

function useActiveLayoutTab(position: CSSPosition | null, isChildOfFlexComponent: boolean) {
  let value: SelfLayoutTab
  if (position === 'absolute' || position === 'sticky') {
    value = position
  } else {
    value = isChildOfFlexComponent ? 'flex' : 'flow'
  }
  return usePropControlledState(value)
}

interface SelfLayoutSubsectionProps {
  position: CSSPosition | null
  isChildOfFlexComponent: boolean
  parentFlexDirection: string | null
  aspectRatioLocked: boolean
  toggleAspectRatioLock: () => void
}

export const SelfLayoutSubsection = betterReactMemo(
  'SelfLayoutSubsection',
  (props: SelfLayoutSubsectionProps) => {
    const [activeTab, setActiveTab] = useActiveLayoutTab(
      props.position,
      props.isChildOfFlexComponent,
    )

    return (
      <>
        <InspectorSubsectionHeader>Position and Dimensions</InspectorSubsectionHeader>
        <UIGridRow padded variant='<-------------1fr------------->'>
          <LayoutTypePicker value={activeTab} setActiveTab={setActiveTab} />
        </UIGridRow>
        <GiganticSizePinsSubsection
          layoutType={activeTab}
          parentFlexDirection={props.parentFlexDirection}
          aspectRatioLocked={props.aspectRatioLocked}
          toggleAspectRatioLock={props.toggleAspectRatioLock}
        />
        {activeTab === 'flex' ? <FlexElementSubsection /> : null}
      </>
    )
  },
)
