import { mapDropNulls } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { emptyComments, jsExpressionValue } from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import type { PropertiesToUpdate } from '../../../core/tailwind/tailwind-class-list-utils'
import {
  addNewClasses,
  getClassListFromParsedClassList,
  getParsedClassList,
  removeClasses,
  updateExistingClasses,
} from '../../../core/tailwind/tailwind-class-list-utils'
import { getTailwindConfigCached } from '../../../core/tailwind/tailwind-compilation'
import { getClassNameAttribute } from '../../../core/tailwind/tailwind-options'
import { getElementFromProjectContents, type EditorState } from '../../editor/store/editor-state'
import { applyValuesAtPath } from './adjust-number-command'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'

export interface UpdateClassList extends BaseCommand {
  type: 'UPDATE_CLASS_LIST'
  element: ElementPath
  classNameUpdates: ClassListUpdate[]
}

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

export function updateClassListCommand(
  whenToRun: WhenToRun,
  element: ElementPath,
  classNameUpdates: ClassListUpdate[],
): UpdateClassList {
  return {
    type: 'UPDATE_CLASS_LIST',
    whenToRun: whenToRun,
    element: element,
    classNameUpdates: classNameUpdates,
  }
}

export const runUpdateClassList: CommandFunction<UpdateClassList> = (
  editorState: EditorState,
  command: UpdateClassList,
) => {
  const { element, classNameUpdates } = command

  const currentClassNameAttribute =
    getClassNameAttribute(getElementFromProjectContents(element, editorState.projectContents))
      ?.value ?? ''

  const parsedClassList = getParsedClassList(
    currentClassNameAttribute,
    getTailwindConfigCached(editorState),
  )

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

  const newClassList = getClassListFromParsedClassList(
    updatedClassList,
    getTailwindConfigCached(editorState),
  )

  const { editorStatePatch } = applyValuesAtPath(editorState, element, [
    {
      path: PP.create('className'),
      value: jsExpressionValue(newClassList, emptyComments),
    },
  ])

  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Update class list for ${EP.toUid(element)} to ${newClassList}`,
  }
}
