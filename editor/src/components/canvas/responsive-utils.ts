import type { Feature, FeatureRange, MediaQuery, ScreenSize } from './responsive-types'
import * as csstree from 'css-tree'
import type { StyleMediaSizeModifier, StyleModifier } from './canvas-types'
import { type CSSNumber, type CSSNumberUnit, cssNumber } from '../inspector/common/css-utils'
import { memoize } from '../../core/shared/memoize'

/**
 * Extracts the screen size from a CSS string, for example:
 * `@media (min-width: 100px)` -> { min: {value: 100, unit: 'px'} }
 * `@media (20px < width < 50em)` -> { min: {value: 20, unit: 'px'}, max: {value: 50, unit: 'em'} }
 */
export const extractScreenSizeFromCss = memoize((css: string): ScreenSize | null => {
  const mediaQuery = parseMediaQueryFromCss(css)
  return mediaQuery == null ? null : mediaQueryToScreenSize(mediaQuery)
})

function extractFromFeatureRange(featureRange: FeatureRange): {
  leftValue: CSSNumber | null
  rightValue: CSSNumber | null
  leftComparison: '<' | '>' | null
  rightComparison: '<' | '>' | null
} | null {
  // (100px < width < 500px) OR (500px > width > 100px) OR (100px > width) OR (500px < width)
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
  // (width > 100px) OR (width < 500px)
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
    // 1. Handle FeatureRange case
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

    // 2. Handle Feature case (min-width/max-width)
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

function parseMediaQueryFromCss(css: string): MediaQuery | null {
  let result: MediaQuery | null = null
  csstree.walk(csstree.parse(css), (node) => {
    if (node.type === 'MediaQuery') {
      result = node as unknown as MediaQuery
    }
  })
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
    const maxSizeInPx = cssNumberAsPx(mediaModifier.size.max)
    const minSizeInPx = cssNumberAsPx(mediaModifier.size.min)

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
    // find the closest media modifier
    const minSizeInPx = cssNumberAsPx(variantMediaModifier.size.min)
    const chosenMinSizeInPx = cssNumberAsPx(chosenMediaModifier.size.min)
    if (minSizeInPx != null && (chosenMinSizeInPx == null || minSizeInPx > chosenMinSizeInPx)) {
      chosen = variant
    }
    const maxSizeInPx = cssNumberAsPx(variantMediaModifier.size.max)
    const chosenMaxSizeInPx = cssNumberAsPx(chosenMediaModifier.size.max)
    if (maxSizeInPx != null && (chosenMaxSizeInPx == null || maxSizeInPx < chosenMaxSizeInPx)) {
      chosen = variant
    }
  }
  if (chosen == null) {
    return null
  }
  return chosen
}

// TODO: get this value from the Scene
const EM_TO_PX_RATIO = 16
export function cssNumberAsPx(value: CSSNumber | null | undefined): number | null {
  return value == null ? null : value.unit === 'em' ? value.value * EM_TO_PX_RATIO : value.value
}
