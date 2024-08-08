import React from 'react'
import { InspectorModal } from '../widgets/inspector-modal'
import { FlexColumn, Icons, SquareButton, UtopiaStyles } from '../../../uuiui'
import { UIGridRow } from '../widgets/ui-grid-row'
import { OptionChainControl } from './option-chain-control'

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
          <span>Grid Settings</span>
          <SquareButton highlight onMouseDown={props.closePopup}>
            ×
          </SquareButton>
        </UIGridRow>
        <UIGridRow padded variant='<-------------1fr------------->'>
          <span style={{ fontWeight: 600 }}>Items</span>
        </UIGridRow>
        <UIGridRow padded variant='|--67px--|<--------1fr-------->'>
          <span>Align</span>
        </UIGridRow>
        <UIGridRow padded variant='|--67px--|<--------1fr-------->'>
          <span>Justify</span>
        </UIGridRow>
        <UIGridRow padded variant='<-------------1fr------------->'>
          <span style={{ fontWeight: 600 }}>Entire Grid</span>
        </UIGridRow>
        <UIGridRow padded variant='|--67px--|<--------1fr-------->'>
          <span>Align</span>
        </UIGridRow>
        <UIGridRow padded variant='|--67px--|<--------1fr-------->'>
          <span>Justify</span>
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
