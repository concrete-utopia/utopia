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
              boxShadow: `0 0 0 1px ${colorTheme.warningForeground.value} inset`,
              justifyContent: 'center',
              color: colorTheme.warningForeground.value,
            }}
          >
            <Icn type='warningtriangle' color='orange' width={16} height={16} />
            <span style={{ paddingLeft: 4 }}>Image failed to load.</span>
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
