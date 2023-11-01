import type { Frame, FramePoint } from 'utopia-api/core'
import { isHorizontalPoint } from 'utopia-api/core'
import type { LayoutPinnedProp } from '../../../core/layout/layout-helpers-new'
import {
  HorizontalLayoutPinnedProps,
  LayoutPinnedProps,
  VerticalLayoutPinnedProps,
  framePointForPinnedProp,
} from '../../../core/layout/layout-helpers-new'
import type { LocalRectangle } from '../../../core/shared/math-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import Utils from '../../../utils/utils'
import type { InspectorInfo } from './property-path-hooks'

import { MaxContent } from '../inspector-common'
import type { CSSNumber } from './css-utils'

const HorizontalPropPreference: Array<LayoutPinnedProp> = ['left', 'width', 'right']
const HorizontalPropPreferenceHug: Array<LayoutPinnedProp> = ['width', 'left', 'right']
const VerticalPropPreference: Array<LayoutPinnedProp> = ['top', 'height', 'bottom']
const VerticalPropPreferenceHug: Array<LayoutPinnedProp> = ['height', 'top', 'bottom']

export interface ElementFrameInfo {
  path: ElementPath
  frame: Frame
  localFrame: LocalRectangle | null
  parentFrame: LocalRectangle | null
}

type PinInspectorInfo = InspectorInfo<CSSNumber | undefined>

export type PinsInfo = { [key in LayoutPinnedProp]: PinInspectorInfo }

export function getPinsToDelete(
  newFrameProp: LayoutPinnedProp,
  frame: Frame,
  lastSetHorizontalProp: LayoutPinnedProp | null,
  lastSetVerticalProp: LayoutPinnedProp | null,
): Array<LayoutPinnedProp> {
  const newFramePoint = framePointForPinnedProp(newFrameProp)
  const pinExists = frame[newFramePoint] != null
  const isHorizontalPin = isHorizontalPoint(newFramePoint)

  let pointsToDelete: Array<LayoutPinnedProp> = []
  if (!pinExists) {
    let pointsToKeep: Array<LayoutPinnedProp>
    if (isHorizontalPin) {
      if (lastSetHorizontalProp != null) {
        pointsToKeep = [newFrameProp, lastSetHorizontalProp, ...VerticalLayoutPinnedProps]
      } else {
        const HorizontalPreference: Array<LayoutPinnedProp> =
          frame.width === MaxContent ? HorizontalPropPreferenceHug : HorizontalPropPreference
        const pinToKeep = HorizontalPreference.find((p) => frame[p] != null)
        pointsToKeep = Utils.maybeToArray(pinToKeep).concat([
          newFrameProp,
          ...VerticalLayoutPinnedProps,
        ])
      }
    } else {
      if (lastSetVerticalProp != null) {
        pointsToKeep = [newFrameProp, lastSetVerticalProp, ...HorizontalLayoutPinnedProps]
      } else {
        const VerticalPreference: Array<LayoutPinnedProp> =
          frame.height === MaxContent ? VerticalPropPreferenceHug : VerticalPropPreference
        const pinToKeep = VerticalPreference.find((p) => frame[p] != null)
        pointsToKeep = Utils.maybeToArray(pinToKeep).concat([
          newFrameProp,
          ...HorizontalLayoutPinnedProps,
        ])
      }
    }

    Utils.fastForEach(LayoutPinnedProps, (framePoint) => {
      if (!pointsToKeep.includes(framePoint) && frame[framePoint] !== undefined) {
        pointsToDelete.push(framePoint)
      }
    })
  }
  return pointsToDelete
}

export interface FramePinInfo {
  isPrimaryPosition: boolean
  isRelativePosition: boolean
}

export type FramePinsInfo = { [key in FramePoint]: FramePinInfo }
