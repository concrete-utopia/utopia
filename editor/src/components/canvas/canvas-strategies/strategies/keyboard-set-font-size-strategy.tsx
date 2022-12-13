import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { elementOnlyHasTextChildren } from '../../../../core/model/element-template-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { defaultEither, isLeft } from '../../../../core/shared/either'
import { ElementInstanceMetadataMap, isJSXElement } from '../../../../core/shared/element-template'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { create } from '../../../../core/shared/property-path'
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
import { accumulatePresses } from './shared-keyboard-strategy-helpers'

export function keyboardSetFontSizeStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(
    canvasState.interactionTarget,
  ).filter((path) => {
    const element = MetadataUtils.findElementByElementPath(canvasState.startingMetadata, path)
    if (element == null || isLeft(element.element) || !isJSXElement(element.element.value)) {
      return false
    }
    return elementOnlyHasTextChildren(element.element.value)
  })

  if (selectedElements.length === 0) {
    return null
  }

  return {
    id: 'set-font-size',
    name: 'Set font size',
    controlsToRender: [],
    fitness: 1,
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

      // console.log(fontSizeDelta, accumulatePresses)

      const commands = mapDropNulls(
        (path) => getFontSize(canvasState.startingMetadata, path),
        selectedElements,
      ).map(([fontSize, path]) =>
        setProperty(
          'always',
          path,
          create(['style', 'fontSize']),
          printCSSNumber(cssNumber(fontSize.value + fontSizeDelta, fontSize.unit), null),
        ),
      )

      return strategyApplicationResult(commands)
    },
  }
}

function getFontSize(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): [CSSNumber, ElementPath] | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null || isLeft(element.element) || !isJSXElement(element.element.value)) {
    return null
  }

  const size = defaultEither(null, parseCSSLengthPercent(element.computedStyle?.['fontSize']))
  if (size == null) {
    return null
  }

  return [size, elementPath]
}
