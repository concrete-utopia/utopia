import * as React from 'react'
import { betterReactMemo } from 'uuiui-deps'
import { InstancePath } from '../../../../core/shared/project-file-types'
import { EditorAction, EditorDispatch } from '../../../editor/action-types'
import { showToast, updateFrameDimensions } from '../../../editor/actions/actions'
import { OptionChainControl } from '../../controls/option-chain-control'
import { getControlStyles } from '../../common/control-status'

interface ImageDensityControl {
  dispatch: EditorDispatch
  selectedViews: Array<InstancePath>
  naturalWidth: number | null
  naturalHeight: number | null
  clientWidth: number
  clientHeight: number
}

export const ImageDensityControl = betterReactMemo(
  'ImageDensityControl',
  ({
    naturalWidth,
    clientWidth,
    naturalHeight,
    clientHeight,
    dispatch,
    selectedViews,
  }: ImageDensityControl) => {
    const dimensionMultiplier: number | null = React.useMemo(() => {
      if (naturalWidth != null && naturalHeight != null) {
        const widthMultiplier = naturalWidth / clientWidth
        const heightMultiplier = naturalHeight / clientHeight
        if (widthMultiplier === heightMultiplier) {
          return widthMultiplier
        }
      }
      return null
    }, [clientWidth, naturalWidth, clientHeight, naturalHeight])

    const onSubmitValue = React.useCallback(
      (value: number) => {
        if (naturalWidth != null && naturalHeight != null) {
          const newWidth = naturalWidth / value
          const newHeight = naturalHeight / value
          let actions: Array<EditorAction> = selectedViews.map((view) =>
            updateFrameDimensions(view, newWidth, newHeight),
          )
          if (!Number.isInteger(newWidth) || !Number.isInteger(newHeight)) {
            actions.push(
              showToast({
                message: `Image does not have native @${value}x dimensions: decimals used, and browser will apply rounding.`,
                level: 'WARNING',
              }),
            )
          }
          dispatch(actions)
        }
      },
      [naturalWidth, naturalHeight, selectedViews, dispatch],
    )

    return (
      <OptionChainControl
        id='image-density-control'
        key='image-density-control'
        value={dimensionMultiplier}
        options={[
          {
            value: 1,
            label: '@1x',
          },
          {
            value: 2,
            label: '@2x',
          },
          {
            value: 3,
            label: '@3x',
          },
        ]}
        onSubmitValue={onSubmitValue}
        // TODO controlstatus: this control style needs to dynamically take into account the various properties that this could steamroll through.
        controlStatus={'simple'}
        controlStyles={getControlStyles('simple')}
      />
    )
  },
)
