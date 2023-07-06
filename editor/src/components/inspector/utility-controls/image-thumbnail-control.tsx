import React from 'react'
import type { CSSURLFunctionBackgroundLayer } from '../common/css-utils'
import { BackgroundPicker } from '../sections/style-section/background-subsection/background-picker'
import type { BackgroundThumbnailControlProps } from '../controls/background-solid-or-gradient-thumbnail-control'
import { backgroundControlContainerStyle } from '../controls/background-solid-or-gradient-thumbnail-control'
import { clampString } from '../common/inspector-utils'
import { Tooltip, FlexRow, UtopiaTheme, useColorTheme, Icn } from '../../../uuiui'

interface ImageThumbnailControlProps extends BackgroundThumbnailControlProps {
  value: CSSURLFunctionBackgroundLayer
}

export const ImageThumbnailControl = React.memo<ImageThumbnailControlProps>((props) => {
  const colorTheme = useColorTheme()
  const [imageNotFound, setImageNotFound] = React.useState(false)
  const onError = React.useCallback(() => {
    setImageNotFound(true)
  }, [])
  const setOpenPopup = props.setOpenPopup
  const closePopup = React.useCallback(() => setOpenPopup(undefined), [setOpenPopup])

  const backgroundIndex = props.backgroundIndex
  const onMouseDown: React.MouseEventHandler<HTMLDivElement> = React.useCallback(
    (e) => {
      e.stopPropagation()
      setOpenPopup((openPopup) => {
        if (openPopup != null) {
          return undefined
        } else {
          return backgroundIndex
        }
      })
    },
    [setOpenPopup, backgroundIndex],
  )

  const modalOffset = props.modalOffset != null ? props.modalOffset : { x: 0, y: 0 }

  return (
    <div>
      {props.popupOpen ? (
        <BackgroundPicker
          id={props.id}
          testId={props.testId}
          offsetX={modalOffset.x}
          offsetY={modalOffset.y}
          closePopup={closePopup}
          value={props.value}
          useSubmitValueFactory={props.useSubmitValueFactory}
          backgroundLayerIndex={props.backgroundIndex}
          controlStatus={props.controlStatus}
        />
      ) : null}
      {imageNotFound ? (
        <Tooltip title={`Image URL "${clampString(props.value.url, 20)}" not found`}>
          <FlexRow
            onMouseDown={onMouseDown}
            style={{
              width: 28,
              height: UtopiaTheme.layout.inputHeight.default,
              borderRadius: UtopiaTheme.inputBorderRadius,
              backgroundColor: colorTheme.warningBgTranslucent.value,
              boxShadow: `0 0 0 1px ${colorTheme.warningForeground.value} inset`,
              justifyContent: 'center',
            }}
          >
            <Icn type='warningtriangle' color='warning' width={16} height={16} />
          </FlexRow>
        </Tooltip>
      ) : (
        <img
          onMouseDown={onMouseDown}
          id={`background-layer-gradient-${props.backgroundIndex}`}
          alt={`background-layer-gradient-${props.backgroundIndex}`}
          src={props.value.url}
          style={{
            ...backgroundControlContainerStyle,
            display: 'block',
            width: 28,
            height: UtopiaTheme.layout.inputHeight.default,
            objectFit: 'cover',
            boxShadow: `0 0 0 1px ${props.controlStyles.borderColor} inset`,
          }}
          onError={onError}
        />
      )}
    </div>
  )
})
