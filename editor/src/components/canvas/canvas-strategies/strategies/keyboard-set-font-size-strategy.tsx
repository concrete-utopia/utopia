import {
  getSimpleAttributeAtPath,
  MetadataUtils,
} from '../../../../core/model/element-metadata-utils'
import { elementOnlyHasTextChildren } from '../../../../core/model/element-template-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { defaultEither, isLeft, right } from '../../../../core/shared/either'
import { ElementInstanceMetadataMap, isJSXElement } from '../../../../core/shared/element-template'
import { ElementPath } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import Keyboard from '../../../../utils/keyboard'
import {
  cssNumber,
  CSSNumber,
  parseCSSLengthPercent,
  printCSSNumber,
} from '../../../inspector/common/css-utils'
import { setProperty } from '../../commands/set-property-command'
import {
  InteractionCanvasState,
  CanvasStrategy,
  getTargetPathsFromInteractionTarget,
  emptyStrategyApplicationResult,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import { accumulatePresses, getLastKeyPressState } from './shared-keyboard-strategy-helpers'

export function keyboardSetFontSizeStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const validTargets = getTargetPathsFromInteractionTarget(canvasState.interactionTarget).filter(
    (path) => isValidTarget(canvasState.startingMetadata, path),
  )

  if (validTargets.length === 0) {
    return null
  }

  return {
    id: 'set-font-size',
    name: 'Set font size',
    controlsToRender: [],
    fitness: fitness(interactionSession),
    apply: () => {
      if (interactionSession == null || interactionSession.interactionData.type !== 'KEYBOARD') {
        return emptyStrategyApplicationResult
      }

      let fontSizeDelta: number = 0

      const accumulatedPresses = accumulatePresses(interactionSession.interactionData.keyStates)
      accumulatedPresses.forEach((accumulatedPress) => {
        accumulatedPress.keysPressed.forEach((key) => {
          if (accumulatedPress.modifiers.cmd && accumulatedPress.modifiers.shift) {
            if (key === 'period') {
              fontSizeDelta += accumulatedPress.count
            }
            if (key === 'comma') {
              fontSizeDelta -= accumulatedPress.count
            }
          }
        })
      })

      const commands = mapDropNulls(
        (path) => getFontSize(canvasState.startingMetadata, path),
        validTargets,
      ).map(([fontSize, path]) =>
        setProperty(
          'always',
          path,
          PP.create(['style', 'fontSize']),
          printCSSNumber(adjust(fontSize, fontSizeDelta), null),
        ),
      )

      return strategyApplicationResult(commands)
    },
  }
}

function isValidTarget(metadata: ElementInstanceMetadataMap, elementPath: ElementPath): boolean {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null || isLeft(element.element) || !isJSXElement(element.element.value)) {
    return false
  }
  return (
    elementOnlyHasTextChildren(element.element.value) ||
    getFontSizeFromProp(metadata, elementPath) != null
  )
}

function fitness(interactionSession: InteractionSession | null): number {
  if (interactionSession == null || interactionSession.interactionData.type !== 'KEYBOARD') {
    return 0
  }
  const lastKeyState = getLastKeyPressState(
    interactionSession.interactionData.keyStates,
    Keyboard.keyTriggersFontSizeStrategy,
  )
  if (lastKeyState == null) {
    return 0
  }
  return 1
}

const FontSizeProp = 'fontSize'

function parseMaybeFontSize(maybeFontSize: unknown): CSSNumber | null {
  return defaultEither(null, parseCSSLengthPercent(maybeFontSize))
}

function getFontSizeFromProp(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): CSSNumber | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null || isLeft(element.element) || !isJSXElement(element.element.value)) {
    return null
  }

  const attribute: string | null = defaultEither(
    null,
    getSimpleAttributeAtPath(
      right(element.element.value.props),
      PP.create(['style', FontSizeProp]),
    ),
  )

  return parseMaybeFontSize(attribute)
}

function getFontSizeFromComputedStyle(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): CSSNumber | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null) {
    return null
  }

  return parseMaybeFontSize(element.computedStyle?.['fontSize'])
}

function adjust(value: CSSNumber, delta: number): CSSNumber {
  const scaleFactor = value.unit === 'em' ? 0.1 : 1
  if (value.unit === 'em' && value.value < 1) {
    return value
  }
  if (value.unit === 'px' && value.value < 5) {
    return value
  }
  return cssNumber(value.value + delta * scaleFactor, value.unit)
}

function getFontSize(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): [CSSNumber, ElementPath] | null {
  const size =
    getFontSizeFromProp(metadata, elementPath) ??
    getFontSizeFromComputedStyle(metadata, elementPath)
  if (size != null) {
    return [size, elementPath]
  }
  return null
}
