import type { ElementPath } from 'utopia-shared/src/types'
import { emptyComments } from 'utopia-shared/src/types'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { jsExpressionValue } from '../../../../core/shared/element-template'
import type {
  PropertiesToRemove,
  PropertiesToUpdate,
} from '../../../../core/tailwind/tailwind-class-list-utils'
import {
  getParsedClassList,
  removeClasses,
  updateExistingClasses,
  addNewClasses,
  getClassListFromParsedClassList,
} from '../../../../core/tailwind/tailwind-class-list-utils'
import { getClassNameAttribute } from '../../../../core/tailwind/tailwind-options'
import type { EditorState } from '../../../editor/store/editor-state'
import { getElementFromProjectContents } from '../../../editor/store/editor-state'
import type { EditorStateWithPatch } from '../../commands/utils/property-utils'
import { applyValuesAtPath } from '../../commands/utils/property-utils'
import * as PP from '../../../../core/shared/property-path'
import type { Config } from 'tailwindcss/types/config'
import { getPropertiesToAppliedModifiersMap } from '../tailwind-style-plugin'

export type ClassListUpdate =
  | { type: 'add'; property: string; value: string }
  | { type: 'remove'; property: string }

export const add = ({ property, value }: { property: string; value: string }): ClassListUpdate => ({
  type: 'add',
  property: property,
  value: value,
})
export const remove = (property: string): ClassListUpdate => ({
  type: 'remove',
  property: property,
})

export const runUpdateClassList = (
  editorState: EditorState,
  element: ElementPath,
  classNameUpdates: ClassListUpdate[],
  config: Config | null,
  context: {
    sceneWidth?: number
  },
): EditorStateWithPatch => {
  const currentClassNameAttribute =
    getClassNameAttribute(getElementFromProjectContents(element, editorState.projectContents))
      ?.value ?? ''

  // this will tell for every property which modifiers are in action
  const propertyToAppliedModifiersMap = getPropertiesToAppliedModifiersMap(
    currentClassNameAttribute,
    classNameUpdates.map((update) => update.property),
    config,
    context,
  )

  const parsedClassList = getParsedClassList(currentClassNameAttribute, config)

  const propertiesToRemove: PropertiesToRemove = classNameUpdates.reduce(
    (acc: PropertiesToRemove, val) =>
      val.type === 'remove'
        ? {
            ...acc,
            [val.property]: {
              remove: true,
              modifiers: propertyToAppliedModifiersMap[val.property] ?? [],
            },
          }
        : acc,
    {},
  )

  const propertiesToUpdate: PropertiesToUpdate = classNameUpdates.reduce(
    (acc: PropertiesToUpdate, val) =>
      val.type === 'remove'
        ? acc
        : {
            ...acc,
            [val.property]: {
              newValue: val.value,
              modifiers: propertyToAppliedModifiersMap[val.property] ?? [],
            },
          },
    {},
  )

  const updatedClassList = [
    removeClasses(propertiesToRemove),
    updateExistingClasses(propertiesToUpdate),
    // currently we're not adding new breakpoint styles (but only editing current ones),
    // so we don't need to pass the propertyToAppliedModifiersMap here
    addNewClasses(propertiesToUpdate),
  ].reduce((classList, fn) => fn(classList), parsedClassList)

  const newClassList = getClassListFromParsedClassList(updatedClassList, config)

  return applyValuesAtPath(editorState, element, [
    {
      path: PP.create('className'),
      value: jsExpressionValue(newClassList, emptyComments),
    },
  ])
}
