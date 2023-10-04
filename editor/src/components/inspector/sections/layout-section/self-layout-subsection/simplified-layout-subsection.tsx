import React from 'react'
import { FlexColumn, FlexRow, InspectorSubsectionHeader, NumberInput } from '../../../../../uuiui'
import { EditorContractDropdown } from '../../../editor-contract-section'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { FixedHugDropdown } from '../../../fill-hug-fixed-control'
import type { MultiselectAtProps } from '../../../common/property-path-hooks'
import {
  stylePropPathMappingFn,
  useGetMultiselectedProps,
  useInspectorLayoutInfo,
  useInspectorStyleInfo,
  useIsSubSectionVisible,
} from '../../../common/property-path-hooks'
import { BooleanControl } from '../../../controls/boolean-control'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import type { StyleLayoutProp } from '../../../../../core/layout/layout-helpers-new'
import {
  layoutPinnedPropIsEdgeProp,
  type LayoutPinnedProp,
  LayoutPinnedProps,
} from '../../../../../core/layout/layout-helpers-new'
import {
  isUnknownInputValue,
  type CSSNumber,
  type UnknownOrEmptyInput,
  isEmptyInputValue,
} from '../../../common/css-utils'
import { unsetPropertyMenuItem } from '../../../common/context-menu-items'
import { executeFirstApplicableStrategy } from '../../../inspector-strategies/inspector-strategy'
import { useDispatch } from '../../../../editor/store/dispatch-context'
import { useRefEditorState } from '../../../../editor/store/store-hook'
import { metadataSelector, selectedViewsSelector } from '../../../../inspector/inpector-selectors'
import {
  setPropFixedEdgeStrategies,
  setPropFixedSizeStrategies,
} from '../../../../inspector/inspector-strategies/inspector-strategies'
import type { Axis } from '../../../../inspector/inspector-common'
import { assertNever } from '../../../../../core/shared/utils'

function getLayoutPinnedPropAxis(property: LayoutPinnedProp): Axis {
  switch (property) {
    case 'top':
    case 'bottom':
    case 'height':
      return 'vertical'
    case 'left':
    case 'right':
    case 'width':
      return 'horizontal'
    default:
      assertNever(property)
  }
}

function getLayoutPinnedPropAlternativeProps(
  property: LayoutPinnedProp,
): [LayoutPinnedProp, LayoutPinnedProp] {
  switch (property) {
    case 'top':
      return ['height', 'bottom']
    case 'bottom':
      return ['top', 'height']
    case 'height':
      return ['top', 'bottom']
    case 'left':
      return ['width', 'right']
    case 'right':
      return ['left', 'width']
    case 'width':
      return ['left', 'right']
    default:
      assertNever(property)
  }
}

export interface LayoutPinPropertyControlProps {
  label: string
  property: LayoutPinnedProp
}

export const LayoutPinPropertyControl = React.memo((props: LayoutPinPropertyControlProps) => {
  const pointInfo = useInspectorLayoutInfo(props.property)

  const dispatch = useDispatch()
  const metadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

  const axis = getLayoutPinnedPropAxis(props.property)

  const onSubmitValue = React.useCallback(
    (value: UnknownOrEmptyInput<CSSNumber>) => {
      if (isUnknownInputValue(value)) {
        // Ignore right now.
      } else if (isEmptyInputValue(value)) {
        // Should unset the value?
        pointInfo.onUnsetValues()
      } else {
        if (layoutPinnedPropIsEdgeProp(props.property)) {
          executeFirstApplicableStrategy(
            dispatch,
            metadataRef.current,
            selectedViewsRef.current,
            elementPathTreeRef.current,
            allElementPropsRef.current,
            setPropFixedEdgeStrategies('always', props.property, value),
          )
        } else {
          executeFirstApplicableStrategy(
            dispatch,
            metadataRef.current,
            selectedViewsRef.current,
            elementPathTreeRef.current,
            allElementPropsRef.current,
            setPropFixedSizeStrategies('always', axis, value),
          )
        }
      }
    },
    [
      allElementPropsRef,
      axis,
      dispatch,
      elementPathTreeRef,
      metadataRef,
      pointInfo,
      props.property,
      selectedViewsRef,
    ],
  )

  return (
    <InspectorContextMenuWrapper
      id={`position-${props.property}-context-menu`}
      items={[unsetPropertyMenuItem(props.property, pointInfo.onUnsetValues)]}
      data={{}}
    >
      <NumberInput
        data-controlstatus={pointInfo.controlStatus}
        value={pointInfo.value}
        id={`pin-${props.property}-number-input`}
        testId={`pin-${props.property}-number-input`}
        labelInner={props.label}
        onSubmitValue={onSubmitValue}
        onTransientSubmitValue={onSubmitValue}
        controlStatus={pointInfo.controlStatus}
        numberType={'LengthPercent'}
        defaultUnitToHide={'px'}
      />
    </InspectorContextMenuWrapper>
  )
})

export const SimplifiedLayoutSubsection = React.memo(() => {
  return (
    <FlexColumn style={{ paddingBottom: 8 }}>
      <InspectorSubsectionHeader>
        <FlexRow
          style={{
            flexGrow: 1,
            gap: 8,
            height: 42,
          }}
        >
          <EditorContractDropdown />
        </FlexRow>
      </InspectorSubsectionHeader>
      <FlexColumn style={{ gap: 8, paddingLeft: 8, paddingRight: 8 }}>
        <UIGridRow
          padded={false}
          variant='<--1fr--><--1fr-->'
          style={{ minHeight: undefined, gap: 4 }}
        >
          <LayoutPinPropertyControl property='left' label='L' />
          <LayoutPinPropertyControl property='top' label='T' />
        </UIGridRow>
        <UIGridRow
          padded={false}
          variant='<--1fr--><--1fr-->'
          style={{ minHeight: undefined, gap: 4 }}
        >
          <LayoutPinPropertyControl property='width' label='W' />
          <LayoutPinPropertyControl property='height' label='H' />
        </UIGridRow>
        <UIGridRow
          padded={false}
          variant='<--1fr--><--1fr-->'
          style={{ minHeight: undefined, gap: 4 }}
        >
          <FixedHugDropdown dimension='width' />
          <FixedHugDropdown dimension='height' />
        </UIGridRow>
        <FlexRow style={{ minHeight: undefined, gap: 4 }}>
          <ClipContentControl />
        </FlexRow>
      </FlexColumn>
    </FlexColumn>
  )
})
SimplifiedLayoutSubsection.displayName = 'SimplifiedLayoutSubsection'

const ClipContentControl = React.memo(() => {
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
