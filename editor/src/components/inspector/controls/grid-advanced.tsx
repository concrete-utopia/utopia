import React from 'react'
import { InspectorModal } from '../widgets/inspector-modal'
import { FlexColumn, Icons, SquareButton, UtopiaStyles } from '../../../uuiui'
import { UIGridRow } from '../widgets/ui-grid-row'

export interface GridAdvancedButtonAndModalProps {
  id: string
  testId: string
  key: string
  openPopup?: (id: string) => void
  closePopup?: () => void
  style?: React.CSSProperties
  pickerOffset?: {
    x: number
    y: number
  }
}

export const GridAdvancedButtonAndModal = React.memo((props: GridAdvancedButtonAndModalProps) => {
  const [popupOpen, setPopupOpen] = React.useState(false)
  const pickerOffset = props.pickerOffset != null ? props.pickerOffset : { x: -250, y: 0 }
  const closePopup = React.useCallback(() => setPopupOpen(false), [setPopupOpen])
  const togglePopup = React.useCallback(() => setPopupOpen((value) => !value), [setPopupOpen])
  const picker = !popupOpen ? null : (
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
        minHeight: 200,
        overflowY: 'scroll',
      }}
    >
      <FlexColumn>
        <UIGridRow padded variant='<--------auto-------->||22px|'>
          <b>Title</b>
          <SquareButton highlight onMouseDown={closePopup}>
            ×
          </SquareButton>
        </UIGridRow>
        <UIGridRow padded variant='<-auto-><----------1fr--------->'>
          <span>Label</span>
          <input value='value' />
        </UIGridRow>
        <UIGridRow padded variant='<-auto-><----------1fr--------->'>
          <span>Label</span>
          <input value='value' />
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
      {picker}
      <SquareButton
        className={`widget-grid-control`}
        key={`${props.id}-surround`}
        spotlight={popupOpen}
        highlight
        // @eslint-ignore
        onMouseDown={togglePopup}
      >
        <Icons.Gear />
      </SquareButton>
    </div>
  )
})
