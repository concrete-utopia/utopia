import type { ElementPath } from 'utopia-shared/src/types'
import { emptyComments } from 'utopia-shared/src/types'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { jsExpressionValue } from '../../../../core/shared/element-template'
import type { PropertiesToUpdate } from '../../../../core/tailwind/tailwind-class-list-utils'
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
): EditorStateWithPatch => {
  const currentClassNameAttribute =
    getClassNameAttribute(getElementFromProjectContents(element, editorState.projectContents))
      ?.value ?? ''

  const parsedClassList = getParsedClassList(currentClassNameAttribute, config)

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

  const newClassList = getClassListFromParsedClassList(updatedClassList, config)

  return applyValuesAtPath(editorState, element, [
    {
      path: PP.create('className'),
      value: jsExpressionValue(newClassList, emptyComments),
    },
  ])
}
