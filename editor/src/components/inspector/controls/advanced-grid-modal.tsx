import React from 'react'
import { InspectorModal } from '../widgets/inspector-modal'
import { FlexColumn, H1, SquareButton, UtopiaStyles } from '../../../uuiui'
import { UIGridRow } from '../widgets/ui-grid-row'
import type { OptionChainOption } from './option-chain-control'
import { OptionChainControl } from './option-chain-control'
import { PrettyLabel } from '../sections/layout-section/flex-container-subsection/flex-container-controls'
import { useInspectorLayoutInfo } from '../common/property-path-hooks'
import {
  RadixSelect,
  regularRadixSelectOption,
  separatorRadixSelectOption,
} from '../../../uuiui/radix-components'
import { optionalMap } from '../../../core/shared/optional-utils'
import { FlexJustifyContent } from 'utopia-api/core'

export interface AdvancedGridModalProps {
  id: string
  testId: string
  key: string
  popupOpen?: boolean
  openPopup?: (id: string) => void
  closePopup?: () => void
  style?: React.CSSProperties
  pickerOffset?: {
    x: number
    y: number
  }
}

export const AdvancedGridModal = React.memo((props: AdvancedGridModalProps) => {
  const pickerOffset = props.pickerOffset != null ? props.pickerOffset : { x: -280, y: -20 }
  const closePopup = props.closePopup ?? (() => {})

  const alignItemsControl = useInspectorLayoutInfo('alignItems')
  const justifyItemsControl = useInspectorLayoutInfo('justifyItems')
  const alignContentControl = useInspectorLayoutInfo('alignContent')
  const justifyContentControl = useInspectorLayoutInfo('justifyContent')

  const currentJustifyContentValue = React.useMemo(
    () =>
      justifyContentControl.controlStatus === 'detected'
        ? unsetSelectOption
        : optionalMap(selectOption, justifyContentControl.value) ?? undefined,
    [justifyContentControl.controlStatus, justifyContentControl.value],
  )

  const contentOptions = [
    unsetSelectOption,
    separatorRadixSelectOption(),
    ...Object.values(FlexJustifyContent).map(selectOption),
  ]

  const onSubmitJustifyContent = React.useCallback(
    (value: string) => {
      justifyContentControl.onSubmitValue(value as FlexJustifyContent)
    },
    [justifyContentControl],
  )

  const currentAlignContentValue = React.useMemo(
    () =>
      alignContentControl.controlStatus === 'detected'
        ? unsetSelectOption
        : optionalMap(selectOption, alignContentControl.value) ?? undefined,
    [alignContentControl.controlStatus, alignContentControl.value],
  )

  const onSubmitAlignContent = React.useCallback(
    (value: string) => {
      alignContentControl.onSubmitValue(value as FlexJustifyContent)
    },
    [alignContentControl],
  )

  const justifyItemsOptions = React.useMemo(() => itemsOptions('justify'), [])
  const alignItemsOptions = React.useMemo(() => itemsOptions('align'), [])

  const picker = (
    <InspectorModal
      offsetX={pickerOffset.x}
      offsetY={pickerOffset.y}
      closePopup={closePopup}
      closePopupOnUnmount={true}
      outsideClickIgnoreClass={`ignore-react-onclickoutside-${props.id}`}
      style={{
        ...UtopiaStyles.popup,
        zIndex: 3,
        minWidth: 230,
      }}
    >
      <FlexColumn>
        <UIGridRow padded variant='<--------auto-------->||22px|'>
          <H1>
            <span style={{ textTransform: 'capitalize', fontSize: '11px' }}>Grid Settings</span>
          </H1>

          <SquareButton highlight onMouseDown={props.closePopup}>
            ×
          </SquareButton>
        </UIGridRow>
        <UIGridRow padded variant='<-------------1fr------------->'>
          <span style={{ fontWeight: 600 }}>Items</span>
        </UIGridRow>

        <UIGridRow padded variant='|--67px--|<--------1fr-------->'>
          <span>Justify</span>
          <OptionChainControl
            id='grid.items.justifyItems'
            key='grid.items.justifyItems'
            testId='grid.items.justifyItems'
            value={justifyItemsControl.value}
            options={justifyItemsOptions}
            onSubmitValue={justifyItemsControl.onSubmitValue}
            controlStatus={justifyItemsControl.controlStatus}
            controlStyles={justifyItemsControl.controlStyles}
          />
        </UIGridRow>
        <UIGridRow padded variant='|--67px--|<--------1fr-------->'>
          <span>Align</span>
          <OptionChainControl
            id='grid.items.alignItems'
            key='grid.items.alignItems'
            testId='grid.items.alignItems'
            value={alignItemsControl.value}
            options={alignItemsOptions}
            onSubmitValue={alignItemsControl.onSubmitValue}
            controlStatus={alignItemsControl.controlStatus}
            controlStyles={alignItemsControl.controlStyles}
          />
        </UIGridRow>
        <UIGridRow padded variant='<-------------1fr------------->'>
          <span style={{ fontWeight: 600 }}>Entire Grid</span>
        </UIGridRow>
        <UIGridRow padded variant='|--67px--|<--------1fr-------->'>
          <span>Justify</span>
          <RadixSelect
            id='grid.justifyContent'
            value={currentJustifyContentValue ?? unsetSelectOption}
            options={contentOptions}
            onValueChange={onSubmitJustifyContent}
          />
        </UIGridRow>
        <UIGridRow padded variant='|--67px--|<--------1fr-------->'>
          <span>Align</span>
          <RadixSelect
            id='grid.alignContent'
            value={currentAlignContentValue ?? unsetSelectOption}
            options={contentOptions}
            onValueChange={onSubmitAlignContent}
          />
        </UIGridRow>
      </FlexColumn>
    </InspectorModal>
  )

  return (
    <div
      key={props.id}
      id={`trigger-${props.id}`}
      className={`ignore-react-onclickoutside-${props.id}`}
      style={props.style}
    >
      {props.popupOpen ? picker : null}
    </div>
  )
})

export const itemsOptions = (alignOrJustify: 'align' | 'justify') =>
  [
    {
      value: 'flex-start',
      tooltip: PrettyLabel[alignOrJustify === 'justify' ? 'left' : 'top'],
      icon: {
        category: `inspector-element`,
        type: `${alignOrJustify}Items-start`,
        color: 'secondary',
        width: 16,
        height: 16,
      },
    },
    {
      value: 'center',
      tooltip: 'Center',
      icon: {
        category: `inspector-element`,
        type: `${alignOrJustify}Items-center`,
        color: 'secondary',
        width: 16,
        height: 16,
      },
    },
    {
      value: 'flex-end',
      tooltip: PrettyLabel[alignOrJustify === 'justify' ? 'right' : 'bottom'],
      icon: {
        category: `inspector-element`,
        type: `${alignOrJustify}Items-end`,
        color: 'secondary',
        width: 16,
        height: 16,
      },
    },
    {
      value: 'stretch',
      tooltip: 'Stretch',
      icon: {
        category: `inspector-element`,
        type: `${alignOrJustify}Items-stretch`,
        color: 'secondary',
        width: 16,
        height: 16,
      },
    },
  ] as Array<OptionChainOption<string | number>>

function selectOption(value: FlexJustifyContent) {
  return regularRadixSelectOption({
    label: value.replace('-', ' '),
    value: value,
    placeholder: true,
  })
}

const unsetSelectOption = regularRadixSelectOption({
  label: 'unset',
  value: 'unset',
  placeholder: true,
})
