import * as React from 'react'
import { colorTheme, FlexRow, Icn, UtopiaTheme } from '../../../../../uuiui'
import { betterReactMemo } from '../../../../../uuiui-deps'
import { MetadataEditorModalPreviewHeight } from '../../../controls/color-picker'
import { CSSURLFunctionBackgroundLayer } from '../../../common/css-utils'
import { checkerboardBackground } from '../../../common/inspector-utils'

interface PickerImagePreviewProps {
  value: CSSURLFunctionBackgroundLayer
}

export const PickerImagePreview = betterReactMemo(
  'BackgroundLayerMetadataModalURLImagePreview',
  (props: PickerImagePreviewProps) => {
    const [imageNotFound, setImageNotFound] = React.useState(false)
    const onError = React.useCallback(() => {
      setImageNotFound(true)
    }, [])

    const noImageProvided = props.value.url === ''

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
              backgroundColor: '#FFFFFF33',
              boxShadow: noImageProvided
                ? undefined
                : `0 0 0 1px ${colorTheme.warningForeground.value} inset`,
              justifyContent: 'center',
              color: noImageProvided ? undefined : colorTheme.warningForeground.value,
            }}
          >
            {noImageProvided ? undefined : (
              <Icn type='warningtriangle' color='orange' width={16} height={16} />
            )}
            <span style={{ paddingLeft: 4 }}>
              {props.value.url === '' ? (
                <div>
                  Add an image URL <br />
                  Local assets:<code>assets/image.png</code>
                  <br />
                  From the internet: <code>https://utopia.fm/smiangle.png</code>
                </div>
              ) : (
                'The image did not load load.'
              )}
            </span>
            <span>{props.value.url}</span>
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
  },
)
