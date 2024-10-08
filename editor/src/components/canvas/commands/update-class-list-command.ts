import * as EP from '../../../core/shared/element-path'
import { emptyComments, jsExpressionValue } from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { getElementFromProjectContents, type EditorState } from '../../editor/store/editor-state'
import { applyValuesAtPath } from './adjust-number-command'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'
import { getClassNameAttribute } from '../../../core/tailwind/tailwind-options'
import { assertNever } from '../../../core/shared/utils'

export interface UpdateClassList extends BaseCommand {
  type: 'UPDATE_CLASS_LIST'
  element: ElementPath
  classNameUpdate: ClassListUpdate
}

export type ClassListUpdate =
  | { type: 'add'; className: string }
  | { type: 'remove'; className: string }

export const add = (className: string): ClassListUpdate => ({ type: 'add', className: className })
export const remove = (className: string): ClassListUpdate => ({
  type: 'remove',
  className: className,
})

export function updateClassListCommand(
  whenToRun: WhenToRun,
  element: ElementPath,
  classNameUpdate: ClassListUpdate,
): UpdateClassList {
  return {
    type: 'UPDATE_CLASS_LIST',
    whenToRun: whenToRun,
    element: element,
    classNameUpdate: classNameUpdate,
  }
}

export const runUpdateClassList: CommandFunction<UpdateClassList> = (
  editorState: EditorState,
  command: UpdateClassList,
) => {
  const { element, classNameUpdate } = command
  const classPrefix = classNameUpdate.className.split('-')[0] + '-' // TODO: parse this with the tailwind parsing lib

  const currentClassNameAttribute =
    getClassNameAttribute(getElementFromProjectContents(element, editorState.projectContents))
      ?.value ?? ''

  const currentClasses = currentClassNameAttribute.split(' ')

  // Filter out classes with the same prefix and add the new class
  const updatedClasses = currentClasses.filter((c) => !c.startsWith(classPrefix))
  switch (classNameUpdate.type) {
    case 'add':
      updatedClasses.push(classNameUpdate.className)
      break
    case 'remove':
      break
    default:
      assertNever(classNameUpdate)
  }

  const { editorStatePatch } = applyValuesAtPath(editorState, element, [
    {
      path: PP.create('className'),
      value: jsExpressionValue(updatedClasses.join(' '), emptyComments),
    },
  ])

  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Update class list for ${EP.toUid(element)} with ${classNameUpdate}`,
  }
}
