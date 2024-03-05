import {
  MetadataUtils,
  getSimpleAttributeAtPath,
} from '../../../../core/model/element-metadata-utils'
import {
  isJSXElement,
  type ElementInstanceMetadataMap,
} from '../../../../core/shared/element-template'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { KeyCharacter } from '../../../../utils/keyboard'
import type { Modifiers } from '../../../../utils/modifiers'
import {
  cssNumber,
  parseCSSLengthPercent,
  type CSSNumber,
} from '../../../inspector/common/css-utils'
import Keyboard from '../../../../utils/keyboard'
import { defaultEither, isLeft, right } from '../../../../core/shared/either'
import * as PP from '../../../../core/shared/property-path'

export function isAdjustFontSizeShortcut(modifiers: Modifiers, key: KeyCharacter): boolean {
  return modifiers.cmd && modifiers.shift && Keyboard.keyTriggersFontSizeStrategy(key)
}

const FontSizeProp = 'fontSize'

export function parseMaybeFontSize(maybeFontSize: unknown): CSSNumber | null {
  return defaultEither(null, parseCSSLengthPercent(maybeFontSize))
}

export function getFontSizeFromProp(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): CSSNumber | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null || isLeft(element.element) || !isJSXElement(element.element.value)) {
    return null
  }

  const attribute: string | null = defaultEither(
    null,
    getSimpleAttributeAtPath(right(element.element.value.props), PP.create('style', FontSizeProp)),
  )

  return parseMaybeFontSize(attribute)
}

function getFontSizeFromMetadata(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): CSSNumber | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null) {
    return null
  }

  return parseMaybeFontSize(element.specialSizeMeasurements?.fontSize)
}

export function adjustFontSize(value: CSSNumber, delta: number): CSSNumber {
  const scaleFactor = value.unit === 'em' ? 0.1 : 1
  if (value.unit === 'em' && value.value < 1) {
    return value
  }
  if (value.unit === 'px' && value.value < 5) {
    return value
  }
  return cssNumber(value.value + delta * scaleFactor, value.unit)
}

export function getFontSize(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): [CSSNumber, ElementPath] | null {
  const size =
    getFontSizeFromProp(metadata, elementPath) ?? getFontSizeFromMetadata(metadata, elementPath)
  if (size != null) {
    return [size, elementPath]
  }
  return null
}
