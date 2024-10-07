import type { ElementPath } from 'utopia-shared/src/types'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { defaultEither, isLeft, right } from '../../../core/shared/either'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { isJSXElement } from '../../../core/shared/element-template'
import { styleStringInArray } from '../../../utils/common-constants'
import type { CSSNumber } from '../../inspector/common/css-utils'
import type { FlexGapData } from '../gap-utils'
import type { StylePlugin } from './style-plugins'
import { stripNulls } from '../../../core/shared/array-utils'
import { optionalMap } from '../../../core/shared/optional-utils'
import { flexDirectionInfo, flexGapInfo, styleProperty } from '../canvas-types'

function maybeFlexGapData(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): FlexGapData | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (
    element == null ||
    element.specialSizeMeasurements.display !== 'flex' ||
    isLeft(element.element) ||
    !isJSXElement(element.element.value)
  ) {
    return null
  }

  if (element.specialSizeMeasurements.justifyContent?.startsWith('space')) {
    return null
  }

  const gap = element.specialSizeMeasurements.gap ?? 0

  const gapFromProps: CSSNumber | undefined = defaultEither(
    undefined,
    getLayoutProperty('gap', right(element.element.value.props), styleStringInArray),
  )

  const flexDirection = element.specialSizeMeasurements.flexDirection ?? 'row'

  return {
    value: {
      renderedValuePx: gap,
      value: gapFromProps ?? null,
    },
    direction: flexDirection,
  }
}

export const InlineStylePlugin: StylePlugin = {
  name: 'Inline Style',
  styleInfoFactory:
    ({ metadata }) =>
    (elementPath) => {
      const flexGapData = maybeFlexGapData(metadata, elementPath)
      return stripNulls([
        optionalMap((gap) => styleProperty(flexGapInfo(gap)), flexGapData?.value),
        optionalMap(
          (direction) => styleProperty(flexDirectionInfo(direction)),
          flexGapData?.direction,
        ),
      ])
    },
  normalizeFromInlineStyle: (editor) => editor,
}
