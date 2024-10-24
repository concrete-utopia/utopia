import * as TailwindClassParser from '@xengine/tailwindcss-class-parser'
import type { JSExpression, JSXAttributesEntry, PropertyPath } from 'utopia-shared/src/types'
import { isLeft } from '../../../core/shared/either'
import { getClassNameAttribute } from '../../../core/tailwind/tailwind-options'
import {
  getElementFromProjectContents,
  getJSXElementFromProjectContents,
} from '../../editor/store/editor-state'
import type { Parser } from '../../inspector/common/css-utils'
import { cssParsers } from '../../inspector/common/css-utils'
import { foldAndApplyCommandsSimple } from './../commands/commands'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { deleteProperties } from './../commands/delete-properties-command'
import * as PP from '../../../core/shared/property-path'
import { updateClassListCommand } from './../commands/update-class-list-command'
import * as UCL from './../commands/update-class-list-command'
import type { StylePlugin } from './style-plugins'
import type { WithPropertyTag } from '../canvas-types'
import { withPropertyTag } from '../canvas-types'
import type { Config } from 'tailwindcss/types/config'
import type { PropertiesWithElementPath } from '../../editor/actions/action-utils'

function parseTailwindProperty<T>(value: unknown, parse: Parser<T>): WithPropertyTag<T> | null {
  const parsed = parse(value, null)
  if (isLeft(parsed)) {
    return null
  }
  return withPropertyTag(parsed.value)
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

function getTailwindClassMapping(classes: string[], config: Config | null): Record<string, string> {
  const mapping: Record<string, string> = {}
  classes.forEach((className) => {
    const parsed = TailwindClassParser.parse(className, config ?? undefined)
    if (parsed.kind === 'error' || !isSupportedTailwindProperty(parsed.property)) {
      return
    }
    mapping[parsed.property] = parsed.value
  })
  return mapping
}

function getStylePropContents(
  styleProp: JSExpression,
): Array<{ key: string; value: unknown }> | null {
  if (styleProp.type === 'ATTRIBUTE_NESTED_OBJECT') {
    return mapDropNulls(
      (c): { key: string; value: unknown } | null =>
        c.type === 'PROPERTY_ASSIGNMENT' &&
        c.value.type === 'ATTRIBUTE_VALUE' &&
        typeof c.key === 'string'
          ? { key: c.key, value: c.value.value }
          : null,
      styleProp.content,
    )
  }

  if (styleProp.type === 'ATTRIBUTE_VALUE' && typeof styleProp.value === 'object') {
    return mapDropNulls(
      ([key, value]) => (typeof key !== 'object' ? null : { key, value }),
      Object.entries(styleProp.value),
    )
  }

  return null
}

function getRemoveUpdates(properties: PropertyPath[]): UCL.ClassListUpdate[] {
  return mapDropNulls((property) => {
    const [maybeStyle, maybeCSSProp] = property.propertyElements
    if (
      maybeStyle !== 'style' ||
      maybeCSSProp == null ||
      !isSupportedTailwindProperty(maybeCSSProp)
    ) {
      return null
    }
    return UCL.remove(TailwindPropertyMapping[maybeCSSProp])
  }, properties)
}

function getPropertyCleanupCommands(propertiesToRemove: PropertiesWithElementPath[]) {
  return mapDropNulls(({ elementPath, properties }) => {
    const removeUpdates = getRemoveUpdates(properties)
    if (removeUpdates.length === 0) {
      return null
    }
    return updateClassListCommand('always', elementPath, removeUpdates)
  }, propertiesToRemove)
}

export const TailwindPlugin = (config: Config | null): StylePlugin => ({
  name: 'Tailwind',
  styleInfoFactory:
    ({ projectContents }) =>
    (elementPath) => {
      const classList = getClassNameAttribute(
        getElementFromProjectContents(elementPath, projectContents),
      )?.value

      if (classList == null || typeof classList !== 'string') {
        return null
      }

      const mapping = getTailwindClassMapping(classList.split(' '), config)

      return {
        gap: parseTailwindProperty(mapping[TailwindPropertyMapping.gap], cssParsers.gap),
        flexDirection: parseTailwindProperty(
          mapping[TailwindPropertyMapping.flexDirection],
          cssParsers.flexDirection,
        ),
      }
    },
  normalizeFromInlineStyle: (editorState, elementsToNormalize, propertiesToRemove) => {
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

      const styleValue = getStylePropContents(styleAttribute.value)
      if (styleValue == null) {
        return []
      }

      const styleProps = mapDropNulls(
        (c): { key: keyof typeof TailwindPropertyMapping; value: unknown } | null =>
          isSupportedTailwindProperty(c.key) ? { key: c.key, value: c.value } : null,
        styleValue,
      )

      const stylePropConversions = mapDropNulls(({ key, value }) => {
        const valueString = stringifiedStylePropValue(value)
        if (valueString == null) {
          return null
        }

        return { property: TailwindPropertyMapping[key], value: valueString }
      }, styleProps)

      return [
        deleteProperties(
          'always',
          elementPath,
          Object.keys(TailwindPropertyMapping).map((prop) => PP.create('style', prop)),
        ),
        updateClassListCommand(
          'always',
          elementPath,
          stylePropConversions.map(({ property, value }) => UCL.add({ property, value })),
        ),
      ]
    })

    const commandsWithPropertyCleanup = [
      ...commands,
      ...getPropertyCleanupCommands(propertiesToRemove),
    ]
    if (commandsWithPropertyCleanup.length === 0) {
      return editorState
    }

    return foldAndApplyCommandsSimple(editorState, commandsWithPropertyCleanup)
  },
})
