import * as TailwindClassParser from '@xengine/tailwindcss-class-parser'
import type { ElementPath, JSXAttributesEntry } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { isLeft } from '../../../core/shared/either'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'
import { getClassNameAttribute } from '../../../core/tailwind/tailwind-options'
import {
  getElementFromProjectContents,
  getJSXElementFromProjectContents,
} from '../../editor/store/editor-state'
import { cssParsers } from '../../inspector/common/css-utils'
import { foldAndApplyCommandsSimple } from './../commands/commands'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { deleteProperties } from './../commands/delete-properties-command'
import * as PP from '../../../core/shared/property-path'
import { updateClassListCommand } from './../commands/update-class-list-command'
import * as UCL from './../commands/update-class-list-command'
import type { StylePlugin } from './style-plugins'
import type { FlexDirectionInfo, FlexGapInfo, StyleProperty, StylePropInfo } from '../canvas-types'
import { flexDirectionInfo, flexGapInfo, styleProperty } from '../canvas-types'
import { cssNumberWithRenderedValue } from '../controls/select-mode/controls-common'
import type { Config } from 'tailwindcss/types/config'

function parseTailwindGap(
  gapValue: string | null,
  instanceMetadata: ElementInstanceMetadata | null,
): StyleProperty<FlexGapInfo> | null {
  if (
    instanceMetadata == null ||
    instanceMetadata.specialSizeMeasurements.gap == null ||
    gapValue == null
  ) {
    return null
  }

  const parsedGapValue = cssParsers.gap(gapValue, null)
  if (isLeft(parsedGapValue)) {
    return null
  }

  return styleProperty(
    flexGapInfo(
      cssNumberWithRenderedValue(
        parsedGapValue.value,
        instanceMetadata.specialSizeMeasurements.gap,
      ),
    ),
  )
}

function parseTailwindFlexDirection(
  directionValue: string | null,
): StyleProperty<FlexDirectionInfo> | null {
  if (directionValue == null) {
    return null
  }
  const parsed = cssParsers.flexDirection(directionValue, null)
  if (isLeft(parsed)) {
    return null
  }

  return styleProperty(flexDirectionInfo(parsed.value))
}

function parseTailwindClass(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
  { property, value }: { property: string; value: string },
): StyleProperty<StylePropInfo> | null {
  switch (property) {
    case 'gap':
      return parseTailwindGap(value, MetadataUtils.findElementByElementPath(metadata, elementPath))
    case 'flexDirection':
      return parseTailwindFlexDirection(value)
    default:
      return null
  }
}

const TailwindPropertyMapping = {
  gap: 'gap',
  flexDirection: 'flexDirection',
} as const

function isSupportedTailwindProperty(prop: unknown): prop is keyof typeof TailwindPropertyMapping {
  return typeof prop === 'string' && prop in TailwindPropertyMapping
}

function stringifiedStylePropValue(value: unknown): string | null {
  if (typeof value === 'string') {
    return value
  }
  if (typeof value === 'number') {
    return `${value}px`
  }

  return null
}

export const TailwindPlugin = (config: Config | null): StylePlugin => ({
  name: 'Tailwind',
  styleInfoFactory:
    ({ metadata, projectContents }) =>
    (elementPath) => {
      const classList = getClassNameAttribute(
        getElementFromProjectContents(elementPath, projectContents),
      )?.value

      if (classList == null || typeof classList !== 'string') {
        return []
      }

      const classes = classList.split(' ')

      const styleProps = mapDropNulls((tailwindClass) => {
        const parsed = TailwindClassParser.parse(tailwindClass, config ?? undefined)
        if (parsed.kind === 'error' || !isSupportedTailwindProperty(parsed.property)) {
          return null
        }

        return parseTailwindClass(metadata, elementPath, {
          property: parsed.property,
          value: parsed.value,
        })
      }, classes)

      return styleProps
    },
  normalizeFromInlineStyle: (editorState, elementsToNormalize) => {
    const commands = elementsToNormalize.flatMap((elementPath) => {
      const element = getJSXElementFromProjectContents(elementPath, editorState.projectContents)
      if (element == null) {
        return []
      }

      const styleAttribute = element.props.find(
        (prop): prop is JSXAttributesEntry =>
          prop.type === 'JSX_ATTRIBUTES_ENTRY' && prop.key === 'style',
      )
      if (styleAttribute == null) {
        return []
      }

      const styleValue = styleAttribute.value
      if (styleValue.type !== 'ATTRIBUTE_NESTED_OBJECT') {
        return []
      }

      const styleProps = mapDropNulls(
        (c): { key: keyof typeof TailwindPropertyMapping; value: unknown } | null =>
          c.type === 'PROPERTY_ASSIGNMENT' &&
          c.value.type === 'ATTRIBUTE_VALUE' &&
          isSupportedTailwindProperty(c.key)
            ? { key: c.key, value: c.value.value }
            : null,
        styleValue.content,
      )

      const stylePropConversions = mapDropNulls(({ key, value }) => {
        const valueString = stringifiedStylePropValue(value)
        if (valueString == null) {
          return null
        }

        const tailwindClass = TailwindClassParser.classname(
          { property: TailwindPropertyMapping[key], value: valueString },
          config ?? undefined,
        )
        if (tailwindClass == null) {
          return null
        }
        return { tailwindClass, key }
      }, styleProps)

      return [
        deleteProperties(
          'always',
          elementPath,
          Object.keys(TailwindPropertyMapping).map((prop) => PP.create('style', prop)),
        ),
        ...stylePropConversions.map(({ tailwindClass }) =>
          updateClassListCommand('always', elementPath, UCL.add(tailwindClass)),
        ),
      ]
    })
    if (commands.length === 0) {
      return editorState
    }

    return foldAndApplyCommandsSimple(editorState, commands)
  },
})
