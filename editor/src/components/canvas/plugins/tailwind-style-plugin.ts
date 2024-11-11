import * as TailwindClassParser from '@xengine/tailwindcss-class-parser'
import {
  defaultEither,
  flatMapEither,
  foldEither,
  isLeft,
  right,
} from '../../../core/shared/either'
import { getClassNameAttribute } from '../../../core/tailwind/tailwind-options'
import { getElementFromProjectContents } from '../../editor/store/editor-state'
import type { Parser } from '../../inspector/common/css-utils'
import { cssParsers } from '../../inspector/common/css-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import * as UCL from './../commands/update-class-list-command'
import type { StylePlugin } from './style-plugins'
import type { CSSStyleProperty, StyleInfo } from '../canvas-types'
import type { Config } from 'tailwindcss/types/config'
import { emptyComments } from 'utopia-shared/src/types'
import { jsExpressionValue } from '../../../core/shared/element-template'
import type { PropertiesToUpdate } from '../../../core/tailwind/tailwind-class-list-utils'
import {
  getParsedClassList,
  removeClasses,
  updateExistingClasses,
  addNewClasses,
  getClassListFromParsedClassList,
} from '../../../core/tailwind/tailwind-class-list-utils'
import { applyValuesAtPath } from '../commands/utils/property-utils'
import * as PP from '../../../core/shared/property-path'
import { Utils } from '../../../uuiui-deps'
import {
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
  setJSXValueAtPathParts,
} from '../../../core/shared/jsx-attribute-utils'
import type { PropsOrJSXAttributes } from '../../../core/model/element-metadata-utils'

function parseTailwindProperty<T>(
  value: unknown,
  parse: Parser<T>,
): CSSStyleProperty<NonNullable<T>> | null {
  const parsed = parse(value, null)
  if (isLeft(parsed) || parsed.value == null) {
    return { type: 'not-found' }
  }
  return { type: 'property', tag: null, value: parsed.value }
}

const TailwindPropertyMapping: Record<string, string> = {
  width: 'width',
  height: 'height',
  top: 'positionTop',
  left: 'positionLeft',
  right: 'positionRight',
  bottom: 'positionBottom',
  gap: 'gap',
  padding: 'padding',
  paddingTop: 'paddingTop',
  paddingRight: 'paddingRight',
  paddingBottom: 'paddingBottom',
  paddingLeft: 'paddingLeft',
  margin: 'margin',
  marginTop: 'marginTop',
  marginRight: 'marginRight',
  marginBottom: 'marginBottom',
  marginLeft: 'marginLeft',
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

const runUpdateClassList = (
  currentClassNameAttribute: string,
  tailwindConfig: Config | null,
  classNameUpdates: UCL.ClassListUpdate[],
): string => {
  const parsedClassList = getParsedClassList(currentClassNameAttribute, tailwindConfig)

  const propertiesToRemove = mapDropNulls(
    (update) => (update.type !== 'remove' ? null : update.property),
    classNameUpdates,
  )

  const propertiesToUpdate: PropertiesToUpdate = classNameUpdates.reduce(
    (acc: { [property: string]: string }, val) =>
      val.type === 'remove' ? acc : { ...acc, [val.property]: val.value },
    {},
  )

  const updatedClassList = [
    removeClasses(propertiesToRemove),
    updateExistingClasses(propertiesToUpdate),
    addNewClasses(propertiesToUpdate),
  ].reduce((classList, fn) => fn(classList), parsedClassList)

  const newClassList = getClassListFromParsedClassList(updatedClassList, tailwindConfig)

  return newClassList
}

const underscoresToSpaces = (s: string | undefined) => s?.replace(/[-_]/g, ' ')

const stringifyPropertyValue = (value: string | number) =>
  typeof value === 'string' ? value : `${value}px`

export const TailwindPlugin = (config: Config | null): StylePlugin => ({
  name: 'Tailwind',
  readStyleFromElementProps: <T extends keyof StyleInfo>(
    propsOrAttributes: PropsOrJSXAttributes,
    key: T,
  ) => {
    if (TailwindPropertyMapping[key] == null) {
      return null
    }

    const classNameAttribute = foldEither(
      (props) => Utils.path(['className'], props),
      (attributes) =>
        defaultEither(
          null,
          flatMapEither(
            (attr) => jsxSimpleAttributeToValue(attr),
            getModifiableJSXAttributeAtPath(attributes, PP.create('className')),
          ),
        ),
      propsOrAttributes,
    )

    if (typeof classNameAttribute !== 'string') {
      return null
    }

    const mapping = getTailwindClassMapping(classNameAttribute.split(' '), config)
    return parseTailwindProperty(
      mapping[TailwindPropertyMapping[key]],
      cssParsers[key],
    ) as StyleInfo[T]
  },
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
        flexBasis: null, // @xengine/tailwindcss-class-parser doesn't support flex-basis
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
        width: parseTailwindProperty(mapping[TailwindPropertyMapping.width], cssParsers.width),
        height: parseTailwindProperty(mapping[TailwindPropertyMapping.height], cssParsers.height),
        top: parseTailwindProperty(mapping[TailwindPropertyMapping.top], cssParsers.top),
        left: parseTailwindProperty(mapping[TailwindPropertyMapping.left], cssParsers.left),
        right: parseTailwindProperty(mapping[TailwindPropertyMapping.right], cssParsers.right),
        bottom: parseTailwindProperty(mapping[TailwindPropertyMapping.bottom], cssParsers.bottom),
        flexGrow: parseTailwindProperty(
          mapping[TailwindPropertyMapping.flexGrow],
          cssParsers.flexGrow,
        ),
        flex: parseTailwindProperty(mapping[TailwindPropertyMapping.flex], cssParsers.flex),
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

    const currentClassNameAttribute =
      getClassNameAttribute(getElementFromProjectContents(elementPath, editorState.projectContents))
        ?.value ?? ''

    const newClassList = runUpdateClassList(currentClassNameAttribute, config, [
      ...propsToDelete,
      ...propsToSet,
    ])

    return applyValuesAtPath(editorState, elementPath, [
      {
        path: PP.create('className'),
        value: jsExpressionValue(newClassList, emptyComments),
      },
    ])
  },
  updateCSSPropertyInProps: (attributes, updates) => {
    const classNameAttribute = defaultEither(
      null,
      flatMapEither(
        (attr) => jsxSimpleAttributeToValue(attr),
        getModifiableJSXAttributeAtPath(attributes, PP.create('className')),
      ),
    )

    if (typeof classNameAttribute !== 'string') {
      return right(attributes)
    }

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

    const newClassList = runUpdateClassList(classNameAttribute, config, [
      ...propsToDelete,
      ...propsToSet,
    ])

    return right(
      defaultEither(
        attributes,
        setJSXValueAtPathParts(
          attributes,
          ['className'],
          0,
          jsExpressionValue(newClassList, emptyComments),
        ),
      ),
    )
  },
})
