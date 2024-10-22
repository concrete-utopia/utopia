import * as TailwindClassParser from '@xengine/tailwindcss-class-parser'
import { isLeft } from '../../../core/shared/either'
import { getClassNameAttribute } from '../../../core/tailwind/tailwind-options'
import { getElementFromProjectContents } from '../../editor/store/editor-state'
import type { Parser } from '../../inspector/common/css-utils'
import { cssParsers } from '../../inspector/common/css-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { updateClassListCommand } from './../commands/update-class-list-command'
import * as UCL from './../commands/update-class-list-command'
import type { StylePlugin } from './style-plugins'
import type { WithPropertyTag } from '../canvas-types'
import { withPropertyTag } from '../canvas-types'
import type { Config } from 'tailwindcss/types/config'

function parseTailwindProperty<T>(value: unknown, parse: Parser<T>): WithPropertyTag<T> | null {
  const parsed = parse(value, null)
  if (isLeft(parsed)) {
    return null
  }
  return withPropertyTag(parsed.value)
}

const TailwindPropertyMapping: Record<string, string> = {
  width: 'width',
  height: 'height',
  gap: 'gap',
  padding: 'padding',
  paddingTop: 'paddingTop',
  paddingRight: 'paddingRight',
  paddingBottom: 'paddingBottom',
  paddingLeft: 'paddingLeft',
  justifyContent: 'justifyContent',
  alignItems: 'alignItems',
  flex: 'flex',
  flexDirection: 'flexDirection',
  flexGrow: 'flexGrow',
  flexShrink: 'flexShrink',
  flexBasis: 'flexBasis',
  flexWrap: 'flexWrap',
}

function isSupportedTailwindProperty(prop: unknown): prop is keyof typeof TailwindPropertyMapping {
  return typeof prop === 'string' && prop in TailwindPropertyMapping
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
              value: update.value.toString(),
            }),
      updates,
    )

    return UCL.runUpdateClassList(
      editorState,
      updateClassListCommand('always', elementPath, [...propsToDelete, ...propsToSet]),
    ).editorStatePatches
  },
})
