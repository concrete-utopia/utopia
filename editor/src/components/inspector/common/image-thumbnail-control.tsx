import * as React from 'react'
import { colorTheme, FlexRow, Icn, Tooltip, UtopiaTheme } from 'uuiui'
import { betterReactMemo } from 'uuiui-deps'
import { CSSURLFunctionBackgroundLayer } from '../new-inspector/css-utils'
import { BackgroundPicker } from '../sections/style-section/background-subsection/background-picker'
import {
  backgroundControlContainerStyle,
  BackgroundThumbnailControlProps,
} from '../sections/style-section/background-subsection/background-solid-or-gradient-thumbnail-control'
import { clampString } from '../utils'

interface ImageThumbnailControlProps extends BackgroundThumbnailControlProps {
  value: CSSURLFunctionBackgroundLayer
}

export const ImageThumbnailControl = betterReactMemo<ImageThumbnailControlProps>(
  'ImageThumbnailControl',
  (props) => {
    const [imageNotFound, setImageNotFound] = React.useState(false)
    const onError = React.useCallback(() => {
      setImageNotFound(true)
    }, [])
    const setOpenPopup = props.setOpenPopup
    const closePopup = React.useCallback(() => setOpenPopup(undefined), [setOpenPopup])

    const backgroundIndex = props.backgroundIndex
    const onMouseDown = React.useCallback(
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
              <Icn type='warningtriangle' color='orange' width={16} height={16} />
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
  },
)
