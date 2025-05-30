import React from 'react'
import { FlexWrap } from 'utopia-api/core'
import type { ControlStatus } from '../../../common/control-status'
import type { ControlStyles } from '../../../common/control-styles'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import type { OptionChainOption } from '../../../controls/option-chain-control'
import { useInspectorLayoutInfo } from '../../../common/property-path-hooks'
import type { SelectOption } from '../../../controls/select-control'
import type { OptionsType } from 'react-select'
import { unsetPropertyMenuItem } from '../../../common/context-menu-items'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import {
  PopupList,
  useWrappedEmptyOrUnknownOnSubmitValue,
  NumberInput,
  Icons,
} from '../../../../../uuiui'
import type { FlexDirection } from '../../../common/css-utils'
import { when } from '../../../../../utils/react-conditionals'
import { Substores, useEditorState } from '../../../../editor/store/store-hook'
import { flexDirectionSelector } from '../../../inpector-selectors'
import type { CanvasControlWithProps } from '../../../../../components/inspector/common/inspector-atoms'
import type { SubduedFlexGapControlProps } from '../../../../../components/canvas/controls/select-mode/subdued-flex-gap-controls'
import { SubduedFlexGapControl } from '../../../../../components/canvas/controls/select-mode/subdued-flex-gap-controls'
import {
  useSetFocusedControlsHandlers,
  useSetHoveredControlsHandlers,
} from '../../../../../components/canvas/controls/select-mode/select-mode-hooks'

type uglyLabel =
  | 'left'
  | 'center'
  | 'right'
  | 'bottom'
  | 'top'
  | 'spaceBetween'
  | 'spaceAround'
  | 'spaceEvenly'
  | 'horizontal'
  | 'vertical'
type prettyLabel =
  | 'Left'
  | 'Center'
  | 'Right'
  | 'Bottom'
  | 'Top'
  | 'Space Between'
  | 'Space Around'
  | 'Space Evenly'
  | 'Horizontal'
  | 'Vertical'

export const PrettyLabel: { [K in uglyLabel]: prettyLabel } = {
  left: 'Left',
  center: 'Center',
  right: 'Right',
  bottom: 'Bottom',
  top: 'Top',
  spaceBetween: 'Space Between',
  spaceAround: 'Space Around',
  spaceEvenly: 'Space Evenly',
  horizontal: 'Horizontal',
  vertical: 'Vertical',
}

export interface FlexFieldControlProps<T> {
  value: T
  controlStatus: ControlStatus
  controlStyles: ControlStyles
  onSubmitValue: (newValue: T) => void
  onUnset: () => void
}

interface FlexWrapControlProps extends FlexFieldControlProps<FlexWrap> {}

const FlexWrapOptions: OptionsType<SelectOption> = [
  {
    value: FlexWrap.NoWrap,
    label: 'No Wrap',
  },
  {
    value: FlexWrap.Wrap,
    label: 'Wrap',
  },
  {
    value: FlexWrap.WrapReverse,
    label: 'Wrap Reverse',
  },
]

export const FlexWrapControl = React.memo((props: FlexWrapControlProps) => {
  const { onSubmitValue: onSubmit } = props
  const onSubmitValue = React.useCallback(
    (newValue: SelectOption) => {
      onSubmit(newValue.value)
    },
    [onSubmit],
  )
  const flexDirection = useEditorState(
    Substores.metadata,
    flexDirectionSelector,
    'FlexWrapControl flexDirection',
  )

  return (
    <InspectorContextMenuWrapper
      id={`flexWrap-context-menu`}
      items={[unsetPropertyMenuItem('Flex Wrap', props.onUnset)]}
      data={{}}
      style={{
        display: 'flex',
        overflow: 'hidden',
        width: undefined,
      }}
    >
      <UIGridRow padded={false} variant='<-auto-><----------1fr--------->'>
        {when(flexDirection.startsWith('row'), <Icons.WrapRow />)}
        {when(flexDirection.startsWith('column'), <Icons.WrapColumn />)}
        <PopupList
          value={FlexWrapOptions.find((option) => option.value === props.value)}
          options={FlexWrapOptions}
          onSubmitValue={onSubmitValue}
          controlStyles={props.controlStyles}
          style={{ background: 'transparent' }}
        />
      </UIGridRow>
    </InspectorContextMenuWrapper>
  )
})

const flexGapControlsForHoverAndFocused: {
  hovered: Array<CanvasControlWithProps<SubduedFlexGapControlProps>>
  focused: Array<CanvasControlWithProps<SubduedFlexGapControlProps>>
} = {
  hovered: [
    {
      control: SubduedFlexGapControl,
      props: {
        hoveredOrFocused: 'hovered',
      },
      key: `subdued-flex-gap-control-hovered`,
    },
  ],
  focused: [
    {
      control: SubduedFlexGapControl,
      props: {
        hoveredOrFocused: 'focused',
      },
      key: `subdued-flex-gap-control-focused`,
    },
  ],
}

export const FlexGapControl = React.memo(() => {
  const { value, onSubmitValue, onUnsetValues, onTransientSubmitValue, controlStatus } =
    useInspectorLayoutInfo('gap')
  const menuItems = [unsetPropertyMenuItem('Flex Gap', onUnsetValues)]

  const wrappedOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(onSubmitValue, onUnsetValues)
  const wrappedOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    onTransientSubmitValue,
    onUnsetValues,
  )

  const flexDirection = useEditorState(
    Substores.metadata,
    flexDirectionSelector,
    'FlexGapControl flexDirection',
  )

  const { onMouseEnter, onMouseLeave } = useSetHoveredControlsHandlers<SubduedFlexGapControlProps>()
  const onMouseEnterWithFlexGapControls = React.useCallback(
    () => onMouseEnter(flexGapControlsForHoverAndFocused.hovered),
    [onMouseEnter],
  )

  const { onFocus, onBlur } = useSetFocusedControlsHandlers<SubduedFlexGapControlProps>()
  const onFocusWithFlexGapControls = React.useCallback(
    () => onFocus(flexGapControlsForHoverAndFocused.focused),
    [onFocus],
  )

  const inputProps = React.useMemo(
    () => ({
      onFocus: onFocusWithFlexGapControls,
      onBlur: onBlur,
    }),
    [onFocusWithFlexGapControls, onBlur],
  )

  return (
    <InspectorContextMenuWrapper id={`gap-context-menu`} items={menuItems} data={{}}>
      <UIGridRow
        padded={false}
        variant='<-------------1fr------------->'
        onMouseEnter={onMouseEnterWithFlexGapControls}
        onMouseLeave={onMouseLeave}
      >
        <NumberInput
          id='flex.container.gap'
          testId='flex.container.gap'
          key='flex.container.gap'
          value={value}
          minimum={0}
          clampOnSubmitValue={true}
          onSubmitValue={wrappedOnSubmitValue}
          onTransientSubmitValue={wrappedOnTransientSubmitValue}
          onForcedSubmitValue={wrappedOnSubmitValue}
          controlStatus={controlStatus}
          numberType='LengthPercent'
          defaultUnitToHide={'px'}
          innerLabel={
            flexDirection.startsWith('row') ? (
              <Icons.GapHorizontal color='on-highlight-secondary' />
            ) : (
              <Icons.GapVertical color='on-highlight-secondary' />
            )
          }
          inputProps={inputProps}
        />
      </UIGridRow>
    </InspectorContextMenuWrapper>
  )
})

export const alignItemsOptions = (
  alignDirection: string,
  alignItemsFlexStart: uglyLabel,
  alignItemsFlexEnd: uglyLabel,
) =>
  [
    {
      value: 'flex-start',
      tooltip: PrettyLabel[alignItemsFlexStart],
      icon: {
        category: `layout/flex`,
        type: `alignItems-${alignDirection}-${alignItemsFlexStart}`,
        color: 'secondary',
        width: 16,
        height: 16,
      },
    },
    {
      value: 'center',
      tooltip: 'Center',
      icon: {
        category: `layout/flex`,
        type: `alignItems-${alignDirection}-center`,
        color: 'secondary',
        width: 16,
        height: 16,
      },
    },
    {
      value: 'flex-end',
      tooltip: PrettyLabel[alignItemsFlexEnd],
      icon: {
        category: `layout/flex`,
        type: `alignItems-${alignDirection}-${alignItemsFlexEnd}`,
        color: 'secondary',
        width: 16,
        height: 16,
      },
    },
    {
      value: 'stretch',
      tooltip: 'Stretch',
      icon: {
        category: `layout/flex`,
        type: `alignItems-${alignDirection}-stretch`,
        color: 'secondary',
        width: 16,
        height: 16,
      },
    },
  ] as Array<OptionChainOption<string | number>>

export const flexDirectionOptions = (flexWrap: FlexWrap) => {
  const flexDirectionWrap = flexWrap === 'wrap' || flexWrap === 'wrap-reverse' ? 'wrap' : 'nowrap'
  const flexWrapReverse = flexWrap === 'wrap-reverse' ? 'reverse' : 'regular'

  return [
    {
      value: 'row',
      tooltip: 'Row',
      icon: {
        category: 'layout/flex',
        type: `flexDirection-row-${flexWrapReverse}-${flexDirectionWrap}`,
        color: 'main',
        width: 16,
        height: 16,
      },
    },
    {
      value: 'column',
      tooltip: 'Column',
      icon: {
        category: 'layout/flex',
        type: `flexDirection-column-${flexWrapReverse}-${flexDirectionWrap}`,
        color: 'main',
        width: 16,
        height: 16,
      },
    },
  ] as Array<OptionChainOption<string | number>>
}

export function getDirectionAwareLabels(
  flexWrap: FlexWrap,
  flexDirection: FlexDirection,
): { [K in string]: uglyLabel } {
  type NonConditionals = {
    justifyFlexStart: uglyLabel
    justifyFlexEnd: uglyLabel
    alignDirection: uglyLabel
  }

  switch (flexDirection) {
    case 'row-reverse': {
      const nonConditionals: NonConditionals = {
        justifyFlexStart: 'right',
        justifyFlexEnd: 'left',
        alignDirection: 'horizontal',
      }
      if (flexWrap === 'wrap-reverse') {
        return {
          ...nonConditionals,
          alignContentFlexStart: 'bottom',
          alignContentFlexEnd: 'top',
          alignItemsFlexStart: 'bottom',
          alignItemsFlexEnd: 'top',
        }
      } else {
        return {
          ...nonConditionals,
          alignContentFlexStart: 'top',
          alignContentFlexEnd: 'bottom',
          alignItemsFlexStart: 'top',
          alignItemsFlexEnd: 'bottom',
        }
      }
    }
    case 'column': {
      const nonConditionals: NonConditionals = {
        justifyFlexStart: 'top',
        justifyFlexEnd: 'bottom',
        alignDirection: 'horizontal',
      }
      if (flexWrap === 'wrap-reverse') {
        return {
          ...nonConditionals,
          alignContentFlexStart: 'right',
          alignContentFlexEnd: 'left',
          alignItemsFlexStart: 'right',
          alignItemsFlexEnd: 'left',
        }
      } else {
        return {
          ...nonConditionals,
          alignContentFlexStart: 'left',
          alignContentFlexEnd: 'right',
          alignItemsFlexStart: 'left',
          alignItemsFlexEnd: 'right',
        }
      }
    }
    case 'column-reverse': {
      const nonConditionals: NonConditionals = {
        justifyFlexStart: 'bottom',
        justifyFlexEnd: 'top',
        alignDirection: 'horizontal',
      }
      if (flexWrap === 'wrap-reverse') {
        return {
          ...nonConditionals,
          alignContentFlexStart: 'right',
          alignContentFlexEnd: 'left',
          alignItemsFlexStart: 'right',
          alignItemsFlexEnd: 'left',
        }
      } else {
        return {
          ...nonConditionals,
          alignContentFlexStart: 'left',
          alignContentFlexEnd: 'right',
          alignItemsFlexStart: 'left',
          alignItemsFlexEnd: 'right',
        }
      }
    }
    case 'row':
    default: {
      const nonConditionals: NonConditionals = {
        justifyFlexStart: 'left',
        justifyFlexEnd: 'right',
        alignDirection: 'vertical',
      }
      if (flexWrap === 'wrap-reverse') {
        return {
          ...nonConditionals,
          alignContentFlexStart: 'bottom',
          alignContentFlexEnd: 'top',
          alignItemsFlexStart: 'bottom',
          alignItemsFlexEnd: 'top',
        }
      } else {
        return {
          ...nonConditionals,
          alignContentFlexStart: 'top',
          alignContentFlexEnd: 'bottom',
          alignItemsFlexStart: 'top',
          alignItemsFlexEnd: 'bottom',
        }
      }
    }
  }
}
