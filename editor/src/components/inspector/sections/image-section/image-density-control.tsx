import React from 'react'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { EditorAction, EditorDispatch } from '../../../editor/action-types'
import { showToast, updateFrameDimensions } from '../../../editor/actions/action-creators'
import { OptionChainControl } from '../../controls/option-chain-control'
import { getControlStyles } from '../../common/control-styles'
import { notice } from '../../../common/notice'
import type { Size } from '../../../../core/shared/math-utils'
import { size } from '../../../../core/shared/math-utils'
import { getFilenameParts } from '../../../images'

interface ImageDensityControl {
  dispatch: EditorDispatch
  selectedViews: Array<ElementPath>
  naturalWidth: number | null
  naturalHeight: number | null
  clientWidth: number
  clientHeight: number
  src: string
}

export const ImageDensityControl = React.memo(
  ({
    naturalWidth,
    clientWidth,
    naturalHeight,
    clientHeight,
    dispatch,
    selectedViews,
    src,
  }: ImageDensityControl) => {
    const dimensionMultiplier: number = React.useMemo(
      () =>
        imageMultiplierFromSizeMeasurements({
          clientHeight,
          clientWidth,
          naturalHeight,
          naturalWidth,
        }) ?? imageMultiplierFromSrc(src),
      [clientHeight, clientWidth, naturalHeight, naturalWidth, src],
    )

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
              showToast(
                notice(
                  `Image does not have native @${value}x dimensions: decimals used, and browser will apply rounding.`,
                  'WARNING',
                ),
              ),
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
        testId='image-density-control'
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

interface SizeMeasurements {
  naturalWidth: number | null
  naturalHeight: number | null
  clientWidth: number
  clientHeight: number
}

function imageMultiplierFromSizeMeasurements(measurements: SizeMeasurements): number | null {
  const naturalSize = size(measurements.naturalWidth ?? 0, measurements.naturalHeight ?? 0)
  const clientSize = size(measurements.clientWidth, measurements.clientHeight)

  if (isZeroSize(naturalSize) || isZeroSize(clientSize)) {
    return null
  }

  const widthMultiplier = naturalSize.width / clientSize.width
  const heightMultiplier = naturalSize.height / clientSize.height

  if (widthMultiplier === heightMultiplier) {
    return widthMultiplier
  }

  return null
}

function isZeroSize(s: Size): boolean {
  return s.width === 0 && s.height === 0
}

function imageMultiplierFromSrc(src: string): number {
  return getFilenameParts(src)?.multiplier ?? 1
}
