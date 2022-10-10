import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import { MetadataUtils, PropsOrJSXAttributes } from '../../../../core/model/element-metadata-utils'
import { isRight } from '../../../../core/shared/either'
import { EdgePosition } from '../../canvas-types'
import { honoursPropsPosition, honoursPropsSize } from './absolute-utils'
import { InteractionCanvasState } from '../canvas-strategy-types'

export type AbsolutePin = 'left' | 'top' | 'right' | 'bottom' | 'width' | 'height'

export const allPins: Array<AbsolutePin> = ['top', 'left', 'width', 'height', 'bottom', 'right']
export const horizontalPins: Array<AbsolutePin> = ['left', 'width', 'right']
export const verticalPins: Array<AbsolutePin> = ['top', 'height', 'bottom']

export function hasAtLeastTwoPinsPerSide(props: { [key: string]: any }): boolean {
  return (
    horizontalPins.filter((pin) => props.style?.[pin] != null).length >= 2 &&
    verticalPins.filter((pin) => props.style?.[pin] != null).length >= 2
  )
}

export function ensureAtLeastTwoPinsForEdgePosition(
  props: PropsOrJSXAttributes,
  edgePosition: EdgePosition,
): Array<AbsolutePin> {
  const existingHorizontalPins = horizontalPins.filter((p) => {
    const prop = getLayoutProperty(p, props, ['style'])
    return isRight(prop) && prop.value != null
  })
  const existingVerticalPins = verticalPins.filter((p) => {
    const prop = getLayoutProperty(p, props, ['style'])
    return isRight(prop) && prop.value != null
  })

  const horizontalPinsToAdd: Array<AbsolutePin> = [...existingHorizontalPins]
  if (edgePosition.x !== 0.5) {
    if (existingHorizontalPins.length === 0) {
      horizontalPinsToAdd.push('left')
      horizontalPinsToAdd.push('width')
    } else if (existingHorizontalPins.length === 1) {
      if (existingHorizontalPins.includes('width')) {
        horizontalPinsToAdd.push('left')
      } else {
        horizontalPinsToAdd.push('width')
      }
    }
  }
  const verticalPinsToAdd: Array<AbsolutePin> = [...existingVerticalPins]
  if (edgePosition.y !== 0.5) {
    if (existingVerticalPins.length === 0) {
      verticalPinsToAdd.push('top')
      verticalPinsToAdd.push('height')
    } else if (existingVerticalPins.length === 1) {
      if (existingVerticalPins.includes('height')) {
        verticalPinsToAdd.push('top')
      } else {
        verticalPinsToAdd.push('height')
      }
    }
  }

  return [...horizontalPinsToAdd, ...verticalPinsToAdd]
}

export function supportsAbsoluteResize(
  metadata: ElementInstanceMetadataMap,
  element: ElementPath,
  canvasState: InteractionCanvasState,
) {
  const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)
  return (
    elementMetadata?.specialSizeMeasurements.position === 'absolute' &&
    honoursPropsPosition(canvasState, element) &&
    honoursPropsSize(canvasState, element)
  )
}
