import type { Feature, FeatureRange, MediaQuery, ScreenSize } from './responsive-types'
import * as csstree from 'css-tree'
import type { StyleMediaSizeModifier, StyleInfo, StyleModifier } from './canvas-types'
import {
  type CSSNumber,
  type CSSNumberUnit,
  compValueAsPx,
  cssNumber,
} from '../inspector/common/css-utils'
import type { ElementPath } from 'utopia-shared/src/types'
import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { MetadataUtils } from '../../core/model/element-metadata-utils'

function extractFromFeatureRange(featureRange: FeatureRange): {
  leftValue: CSSNumber | null
  rightValue: CSSNumber | null
  leftComparison: '<' | '>' | null
  rightComparison: '<' | '>' | null
} | null {
  // 100px < width < 500px or 500px > width > 100px or 100px > width or 500px < width
  if (featureRange?.middle?.type === 'Identifier' && featureRange.middle.name === 'width') {
    const leftValue =
      featureRange.left?.type === 'Dimension'
        ? cssNumber(Number(featureRange.left.value), featureRange.left.unit as CSSNumberUnit)
        : null

    const rightValue =
      featureRange.right?.type === 'Dimension'
        ? cssNumber(Number(featureRange.right.value), featureRange.right.unit as CSSNumberUnit)
        : null

    return {
      leftValue: leftValue,
      rightValue: rightValue,
      leftComparison: featureRange.leftComparison,
      rightComparison: featureRange.rightComparison,
    }
  }
  // width > 100px or width < 500px
  if (featureRange?.left?.type === 'Identifier' && featureRange.left.name === 'width') {
    const rightValue =
      featureRange.middle?.type === 'Dimension'
        ? cssNumber(Number(featureRange.middle.value), featureRange.middle.unit as CSSNumberUnit)
        : null
    // this is not a mistake, since we normalize the "width" to be in the middle
    const rightComparison = featureRange.leftComparison

    return {
      leftValue: null,
      leftComparison: null,
      rightValue: rightValue,
      rightComparison: rightComparison,
    }
  }
  return null
}
export function mediaQueryToScreenSize(mediaQuery: MediaQuery): ScreenSize {
  const result: ScreenSize = {}

  if (mediaQuery.condition?.type === 'Condition') {
    // Handle FeatureRange case
    const featureRanges = mediaQuery.condition.children.filter(
      (child): child is FeatureRange => child.type === 'FeatureRange',
    ) as Array<FeatureRange>

    featureRanges.forEach((featureRange) => {
      const rangeData = extractFromFeatureRange(featureRange)
      if (rangeData == null) {
        return
      }
      const { leftValue, rightValue, leftComparison, rightComparison } = rangeData
      if (leftValue != null) {
        if (leftComparison === '<') {
          result.min = leftValue
        } else {
          result.max = leftValue
        }
      }
      if (rightValue != null) {
        if (rightComparison === '<') {
          result.max = rightValue
        } else {
          result.min = rightValue
        }
      }
    })

    // Handle Feature case (min-width/max-width)
    const features = mediaQuery.condition.children.filter(
      (child): child is Feature => child.type === 'Feature',
    )
    features.forEach((feature) => {
      if (feature.value?.type === 'Dimension') {
        if (feature.name === 'min-width') {
          result.min = cssNumber(Number(feature.value.value), feature.value.unit as CSSNumberUnit)
        } else if (feature.name === 'max-width') {
          result.max = cssNumber(Number(feature.value.value), feature.value.unit as CSSNumberUnit)
        }
      }
    })
  }

  return result
}

export function extractMediaQueryFromCss(css: string): MediaQuery | null {
  let result: MediaQuery | null = null
  csstree.walk(csstree.parse(css), (node) => {
    if (node.type === 'MediaQuery') {
      result = node as unknown as MediaQuery
    }
  })
  return result
}

// Cache for storing previously parsed screen sizes
const screenSizeCache: Map<string, ScreenSize | null> = new Map()
export function extractScreenSizeFromCss(css: string): ScreenSize | null {
  // Check cache first
  const cached = screenSizeCache.get(css)
  if (cached !== undefined) {
    return cached
  }

  // If not in cache, compute and store result
  const mediaQuery = extractMediaQueryFromCss(css)
  const result = mediaQuery == null ? null : mediaQueryToScreenSize(mediaQuery)

  screenSizeCache.set(css, result)
  return result
}

function getMediaModifier(
  modifiers: StyleModifier[] | undefined | null,
): StyleMediaSizeModifier | null {
  return (modifiers ?? []).filter(
    (modifier): modifier is StyleMediaSizeModifier => modifier.type === 'media-size',
  )[0]
}

export function selectValueByBreakpoint<T extends { modifiers?: StyleModifier[] }>(
  parsedVariants: T[],
  sceneWidthInPx?: number,
): T | null {
  const relevantModifiers = parsedVariants.filter((variant) => {
    // 1. filter out variants that don't have media modifiers, but keep variants with no modifiers at all
    if (variant.modifiers == null || variant.modifiers.length === 0) {
      return true
    }
    // FIXME - we take only the first media modifier for now
    const mediaModifier = getMediaModifier(variant.modifiers)
    if (mediaModifier == null) {
      // this means it only has other modifiers
      return false
    }

    if (sceneWidthInPx == null) {
      // filter out variants that require a scene width
      return false
    }

    // 2. check that it has at least one media modifier that satisfies the current scene width
    const maxSizeInPx = compValueAsPx(mediaModifier.size.max)
    const minSizeInPx = compValueAsPx(mediaModifier.size.min)

    // if it has only max
    if (maxSizeInPx != null && minSizeInPx == null && sceneWidthInPx <= maxSizeInPx) {
      return true
    }

    // if it has only min
    if (maxSizeInPx == null && minSizeInPx != null && sceneWidthInPx >= minSizeInPx) {
      return true
    }

    // if it has both max and min
    if (
      maxSizeInPx != null &&
      minSizeInPx != null &&
      sceneWidthInPx >= minSizeInPx &&
      sceneWidthInPx <= maxSizeInPx
    ) {
      return true
    }
    return false
  })
  let chosen: T | null = null
  for (const variant of relevantModifiers) {
    const chosenMediaModifier = getMediaModifier(chosen?.modifiers)
    const variantMediaModifier = getMediaModifier(variant.modifiers)
    if (variantMediaModifier == null) {
      if (chosenMediaModifier == null) {
        // if we have nothing chosen then we'll take the base value
        chosen = variant
      }
      continue
    }
    if (chosenMediaModifier == null) {
      chosen = variant
      continue
    }
    // fixme - select by actual media query precedence
    const minSizeInPx = compValueAsPx(variantMediaModifier.size.min)
    const chosenMinSizeInPx = compValueAsPx(chosenMediaModifier.size.min)
    if (minSizeInPx != null && (chosenMinSizeInPx == null || minSizeInPx > chosenMinSizeInPx)) {
      chosen = variant
    }
    const maxSizeInPx = compValueAsPx(variantMediaModifier.size.max)
    const chosenMaxSizeInPx = compValueAsPx(chosenMediaModifier.size.max)
    if (maxSizeInPx != null && (chosenMaxSizeInPx == null || maxSizeInPx < chosenMaxSizeInPx)) {
      chosen = variant
    }
  }
  if (chosen == null) {
    return null
  }
  return chosen
}

export function getContainingSceneWidth(
  elementPath: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
): number | undefined {
  const containingScene = MetadataUtils.getParentSceneMetadata(jsxMetadata, elementPath)
  return containingScene?.specialSizeMeasurements?.clientWidth
}

export function getAppliedMediaSizeModifierFromBreakpoint(
  styleInfo: StyleInfo,
  prop: keyof StyleInfo,
): StyleMediaSizeModifier | null {
  if (styleInfo == null) {
    return null
  }
  return styleInfo[prop]?.type === 'property'
    ? (styleInfo[prop].currentVariant.modifiers ?? []).find((m) => m.type === 'media-size') ?? null
    : null
}
