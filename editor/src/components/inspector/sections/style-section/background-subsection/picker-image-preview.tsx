import React from 'react'
import {
  useColorTheme,
  FlexRow,
  Icn,
  UtopiaTheme,
  SimpleFlexRow,
  UtopiaStyles,
} from '../../../../../uuiui'
import { MetadataEditorModalPreviewHeight } from '../../../controls/color-picker'
import type { CSSURLFunctionBackgroundLayer } from '../../../common/css-utils'

const checkerboardBackground = UtopiaStyles.backgrounds.checkerboardBackground

interface PickerImagePreviewProps {
  value: CSSURLFunctionBackgroundLayer
}

export const PickerImagePreview = React.memo((props: PickerImagePreviewProps) => {
  const colorTheme = useColorTheme()
  const [imageNotFound, setImageNotFound] = React.useState(false)
  const onError = React.useCallback(() => {
    setImageNotFound(true)
  }, [])

  const imageURLempty = props.value.url === ''

  return (
    <div
      style={{
        width: '100%',
        height: MetadataEditorModalPreviewHeight,
        ...checkerboardBackground,
      }}
    >
      {imageNotFound ? (
        <FlexRow
          style={{
            width: '100%',
            height: '100%',
            borderRadius: UtopiaTheme.inputBorderRadius,
            backgroundColor: '#FFFFFF88',
            boxShadow: imageURLempty
              ? undefined
              : `0 0 0 1px ${colorTheme.warningForeground.value} inset`,
            justifyContent: 'center',
            color: imageURLempty ? undefined : colorTheme.warningForeground.value,
          }}
        >
          <div>
            {imageURLempty ? (
              <div style={{ paddingLeft: 8, whiteSpace: 'normal' }}>
                <p>
                  Add an image URL, e.g. <span style={{ fontWeight: 600 }}>assets/image.png</span>{' '}
                  or <span style={{ fontWeight: 600 }}>https://utopia.fm/smiangle.png</span>
                </p>

                <p>
                  NB You can add images by dropping them onto the canvas or into the file system, or
                  with copy &amp; paste
                </p>
              </div>
            ) : (
              <SimpleFlexRow>
                <Icn type='warningtriangle' color='warning' width={16} height={16} />
                <span style={{ paddingLeft: 8 }}>Unable to load {props.value.url}</span>
              </SimpleFlexRow>
            )}
          </div>
        </FlexRow>
      ) : (
        <img
          src={props.value.url}
          style={{
            width: '100%',
            height: '100%',
            objectFit: 'contain',
          }}
          onError={onError}
        />
      )}
    </div>
  )
})
