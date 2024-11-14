import * as TailwindClassParser from '@xengine/tailwindcss-class-parser'
import { isLeft } from '../../../core/shared/either'
import { getClassNameAttribute } from '../../../core/tailwind/tailwind-options'
import { getElementFromProjectContents } from '../../editor/store/editor-state'
import type { Parser } from '../../inspector/common/css-utils'
import { cssParsers } from '../../inspector/common/css-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import type { StylePlugin } from './style-plugins'
import type { Config } from 'tailwindcss/types/config'
import { cssStyleProperty, type CSSStyleProperty } from '../canvas-types'
import * as UCL from './tailwind-style-plugin-utils/update-class-list'
import { assertNever } from '../../../core/shared/utils'

function parseTailwindProperty<T>(
  value: unknown,
  parse: Parser<T>,
): CSSStyleProperty<NonNullable<T>> | null {
  const parsed = parse(value, null)
  if (isLeft(parsed) || parsed.value == null) {
    return null
  }
  return cssStyleProperty(parsed.value)
}

const TailwindPropertyMapping: Record<string, string> = {
  gap: 'gap',
  flexDirection: 'flexDirection',
  left: 'positionLeft',
  right: 'positionRight',
  top: 'positionTop',
  bottom: 'positionBottom',
  width: 'width',
  height: 'height',
  flexBasis: 'flexBasis',
  padding: 'padding',
  paddingTop: 'paddingTop',
  paddingRight: 'paddingRight',
  paddingBottom: 'paddingBottom',
  paddingLeft: 'paddingLeft',
}

function isSupportedTailwindProperty(prop: unknown): prop is keyof typeof TailwindPropertyMapping {
  return typeof prop === 'string' && prop in TailwindPropertyMapping
}

function stringifyPropertyValue(value: string | number): string {
  switch (typeof value) {
    case 'number':
      return `${value}px`
    case 'string':
      return value
    default:
      assertNever(value)
  }
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

const underscoresToSpaces = (s: string | undefined) => s?.replace(/[-_]/g, ' ')

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
        left: parseTailwindProperty(mapping[TailwindPropertyMapping.left], cssParsers.left),
        right: parseTailwindProperty(mapping[TailwindPropertyMapping.right], cssParsers.right),
        top: parseTailwindProperty(mapping[TailwindPropertyMapping.top], cssParsers.top),
        bottom: parseTailwindProperty(mapping[TailwindPropertyMapping.bottom], cssParsers.bottom),
        width: parseTailwindProperty(mapping[TailwindPropertyMapping.width], cssParsers.width),
        height: parseTailwindProperty(mapping[TailwindPropertyMapping.height], cssParsers.height),
        flexBasis: parseTailwindProperty(
          mapping[TailwindPropertyMapping.flexBasis],
          cssParsers.flexBasis,
        ),
        padding: parseTailwindProperty(
          underscoresToSpaces(mapping[TailwindPropertyMapping.padding]),
          cssParsers.padding,
        ),
        paddingTop: parseTailwindProperty(
          mapping[TailwindPropertyMapping.paddingTop],
          cssParsers.paddingTop,
        ),
        paddingRight: parseTailwindProperty(
          mapping[TailwindPropertyMapping.paddingRight],
          cssParsers.paddingRight,
        ),
        paddingBottom: parseTailwindProperty(
          mapping[TailwindPropertyMapping.paddingBottom],
          cssParsers.paddingBottom,
        ),
        paddingLeft: parseTailwindProperty(
          mapping[TailwindPropertyMapping.paddingLeft],
          cssParsers.paddingLeft,
        ),
      }
    },
  updateStyles: (editorState, elementPath, updates) => {
    const propsToDelete = mapDropNulls(
      (update) =>
        update.type !== 'delete' || TailwindPropertyMapping[update.property] == null
          ? null
          : UCL.remove(TailwindPropertyMapping[update.property]),
      updates,
    )

    const propsToSet = mapDropNulls(
      (update) =>
        update.type !== 'set' || TailwindPropertyMapping[update.property] == null
          ? null
          : UCL.add({
              property: TailwindPropertyMapping[update.property],
              value: stringifyPropertyValue(update.value),
            }),
      updates,
    )
    return UCL.runUpdateClassList(
      editorState,
      elementPath,
      [...propsToDelete, ...propsToSet],
      config,
    )
  },
})
