import React from 'react'
import { InspectorModal } from '../widgets/inspector-modal'
import { colorTheme, FlexColumn, H1, SquareButton, UtopiaStyles } from '../../../uuiui'
import type { GridRowVariant } from '../widgets/ui-grid-row'
import { UIGridRow } from '../widgets/ui-grid-row'
import type { OptionChainOption } from './option-chain-control'
import { OptionChainControl } from './option-chain-control'
import { PrettyLabel } from '../sections/layout-section/flex-container-subsection/flex-container-controls'
import type { InspectorInfo } from '../common/property-path-hooks'
import { useInspectorLayoutInfo } from '../common/property-path-hooks'
import {
  RadixSelect,
  regularRadixSelectOption,
  separatorRadixSelectOption,
} from '../../../uuiui/radix-components'
import { optionalMap } from '../../../core/shared/optional-utils'
import { FlexAlignment } from 'utopia-api/core'
import { FlexJustifyContent } from 'utopia-api/core'
import { GridAutoColsOrRowsControlInner } from '../grid-auto-cols-or-rows-control'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { selectedViewsSelector } from '../inpector-selectors'

export interface AdvancedGridModalProps {
  id: string
  testId: string
  popupOpen: boolean
  closePopup: () => void
  modalOffset: {
    x: number
    y: number
  }
}

export const AdvancedGridModal = React.memo((props: AdvancedGridModalProps) => {
  const modalOffset = props.modalOffset ?? { x: 0, y: 0 }
  const [dropdownOpen, setDropdownOpen] = React.useState({
    justifyContent: false,
    alignContent: false,
  })
  const toggleJustifyContentDropdown = React.useCallback((state: boolean) => {
    setDropdownOpen((prev) => ({ ...prev, justifyContent: state }))
  }, [])
  const toggleAlignContentDropdown = React.useCallback((state: boolean) => {
    setDropdownOpen((prev) => ({ ...prev, alignContent: state }))
  }, [])
  const closePopup = React.useCallback(() => {
    if (!dropdownOpen.justifyContent && !dropdownOpen.alignContent) {
      props.closePopup()
    }
  }, [dropdownOpen, props])

  /**
   * If you edit/add/remove props here, remember to update the same values in useGridAdvancedPropertiesCount
   * to keep the counter badge up to date displayed in the advanced menu up to date.
   */
  const alignItemsLayoutInfo = useInspectorLayoutInfo('alignItems')
  const justifyItemsLayoutInfo = useInspectorLayoutInfo('justifyItems')
  const alignContentLayoutInfo = useInspectorLayoutInfo('alignContent')
  const justifyContentLayoutInfo = useInspectorLayoutInfo('justifyContent')

  const currentJustifyContentValue = React.useMemo(
    () => getLayoutInfoValue(justifyContentLayoutInfo),
    [justifyContentLayoutInfo],
  )

  const currentAlignContentValue = React.useMemo(
    () => getLayoutInfoValue(alignContentLayoutInfo),
    [alignContentLayoutInfo],
  )

  const currentJustifyItemsValue = React.useMemo(
    () => getValueOrUnset(justifyItemsLayoutInfo),
    [justifyItemsLayoutInfo],
  )

  const currentAlignItemsValue = React.useMemo(
    () => getValueOrUnset(alignItemsLayoutInfo),
    [alignItemsLayoutInfo],
  )

  const justifyOptions = [
    unsetSelectOption,
    separatorRadixSelectOption(),
    ...Object.values(FlexJustifyContent).map(selectOption),
  ]

  const alignOptions = [
    unsetSelectOption,
    separatorRadixSelectOption(),
    ...Object.values(FlexAlignment).map(selectOption),
  ]

  const onSubmitJustifyContent = React.useCallback(
    (value: string) => {
      if (value === 'unset') {
        justifyContentLayoutInfo.onUnsetValues()
      } else {
        justifyContentLayoutInfo.onSubmitValue(value as FlexJustifyContent)
      }
    },
    [justifyContentLayoutInfo],
  )

  const onSubmitAlignContent = React.useCallback(
    (value: string) => {
      if (value === 'unset') {
        alignContentLayoutInfo.onUnsetValues()
      } else {
        alignContentLayoutInfo.onSubmitValue(value as FlexJustifyContent)
      }
    },
    [alignContentLayoutInfo],
  )

  const selectedViewsRef = useRefEditorState(selectedViewsSelector)
  const grid = useEditorState(
    Substores.metadata,
    (store) => {
      if (selectedViewsRef.current.length !== 1) {
        return null
      }
      return MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        selectedViewsRef.current[0],
      )
    },
    'AdvancedGridModal grid',
  )

  if (grid == null) {
    return null
  }

  const advancedGridModal = (
    <InspectorModal
      offsetX={modalOffset.x}
      offsetY={modalOffset.y}
      closePopup={closePopup}
      closePopupOnUnmount={false}
      outsideClickIgnoreClass={`ignore-react-onclickoutside-${props.id}`}
      style={{
        ...UtopiaStyles.popup,
        minWidth: 230,
        color: colorTheme.fg1.value,
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

        <UIGridRow padded variant={rowVariant}>
          <span>Justify</span>
          <OptionChainControl
            id='grid.items.justifyItems'
            key='grid.items.justifyItems'
            testId='grid.items.justifyItems'
            {...justifyItemsLayoutInfo}
            value={currentJustifyItemsValue}
            options={justifyItemsOptions}
          />
        </UIGridRow>
        <UIGridRow padded variant={rowVariant}>
          <span>Align</span>
          <OptionChainControl
            id='grid.items.alignItems'
            key='grid.items.alignItems'
            testId='grid.items.alignItems'
            {...alignItemsLayoutInfo}
            value={currentAlignItemsValue}
            options={alignItemsOptions}
          />
        </UIGridRow>
        <UIGridRow padded variant='<-------------1fr------------->'>
          <span style={{ fontWeight: 600 }}>Entire Grid</span>
        </UIGridRow>
        <UIGridRow
          padded
          variant={rowVariant}
          className={`ignore-react-onclickoutside-${props.id}`}
        >
          <span>Justify</span>
          <RadixSelect
            id='grid.justifyContent'
            value={currentJustifyContentValue ?? unsetSelectOption}
            options={justifyOptions}
            onValueChange={onSubmitJustifyContent}
            contentClassName={`ignore-react-onclickoutside-${props.id}`}
            contentStyle={{
              zIndex: 1,
            }}
            onOpenChange={toggleJustifyContentDropdown}
          />
        </UIGridRow>
        <UIGridRow
          padded
          variant={rowVariant}
          className={`ignore-react-onclickoutside-${props.id}`}
        >
          <span>Align</span>
          <RadixSelect
            id='grid.alignContent'
            value={currentAlignContentValue ?? unsetSelectOption}
            options={alignOptions}
            onValueChange={onSubmitAlignContent}
            contentClassName={`ignore-react-onclickoutside-${props.id}`}
            contentStyle={{
              zIndex: 1,
            }}
            onOpenChange={toggleAlignContentDropdown}
          />
        </UIGridRow>
        <UIGridRow padded variant='<-------------1fr------------->'>
          <span style={{ fontWeight: 600 }}>Template</span>
        </UIGridRow>
        <UIGridRow
          padded
          variant={rowVariant}
          className={`ignore-react-onclickoutside-${props.id}`}
        >
          <GridAutoColsOrRowsControlInner grid={grid} axis='column' label='Auto Cols' />
        </UIGridRow>
        <UIGridRow
          padded
          variant={rowVariant}
          className={`ignore-react-onclickoutside-${props.id}`}
        >
          <GridAutoColsOrRowsControlInner grid={grid} axis='row' label='Auto Rows' />
        </UIGridRow>
      </FlexColumn>
    </InspectorModal>
  )

  return (
    <div
      key={props.id}
      id={`trigger-${props.id}`}
      className={`ignore-react-onclickoutside-${props.id}`}
      data-testid={props.testId}
    >
      {props.popupOpen ? advancedGridModal : null}
    </div>
  )
})

AdvancedGridModal.displayName = 'AdvancedGridModal'

const itemsOptions = (alignOrJustify: 'align' | 'justify') =>
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

const justifyItemsOptions = itemsOptions('justify')
const alignItemsOptions = itemsOptions('align')
const rowVariant: GridRowVariant = '|--67px--|<--------1fr-------->'

function selectOption(value: FlexJustifyContent | FlexAlignment) {
  return regularRadixSelectOption({
    label: value.replace('-', ' '),
    value: value,
    placeholder: false,
  })
}

const unsetSelectOption = regularRadixSelectOption({
  label: 'unset',
  value: 'unset',
  placeholder: true,
})

const isUnset = (control: InspectorInfo<any>): boolean =>
  control.controlStatus === 'detected' || control.controlStatus === 'unset'

const getLayoutInfoValue = (control: InspectorInfo<FlexJustifyContent>) =>
  isUnset(control) ? unsetSelectOption : optionalMap(selectOption, control.value) ?? undefined

const getValueOrUnset = (control: InspectorInfo<FlexAlignment>): string =>
  isUnset(control) ? 'unset' : control.value
