import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import type { PropsOrJSXAttributes } from '../../../../core/model/element-metadata-utils'
import { foldEither, isLeft, right } from '../../../../core/shared/either'
import type { ElementInstanceMetadata } from '../../../../core/shared/element-template'
import { isJSXElement } from '../../../../core/shared/element-template'
import { styleStringInArray } from '../../../../utils/common-constants'

export type ElementDimensions = {
  width: number | null
  height: number | null
  flexBasis: number | null
} | null

export const getElementDimensions = (metadata: ElementInstanceMetadata): ElementDimensions => {
  const getOffsetPropValue = (
    name: 'width' | 'height' | 'flexBasis',
    attrs: PropsOrJSXAttributes,
  ): number | null => {
    return foldEither(
      (_) => null,
      (v) => v?.value ?? null,
      getLayoutProperty(name, attrs, styleStringInArray),
    )
  }

  if (isLeft(metadata.element)) {
    return null
  }
  const { value } = metadata.element
  if (!isJSXElement(value)) {
    return null
  }

  const attrs = right(value.props)

  return {
    width: getOffsetPropValue('width', attrs),
    height: getOffsetPropValue('height', attrs),
    flexBasis: getOffsetPropValue('flexBasis', attrs),
  }
}
