import { setUtopiaID } from '../../../core/model/element-template-utils'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import { UTOPIA_UID_KEY } from '../../../core/model/utopia-constants'
import { foldEither } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import { emptyComments, jsxAttributeValue } from '../../../core/shared/element-template'
import { setJSXValuesAtPaths } from '../../../core/shared/jsx-attributes'
import { ElementPath } from '../../../core/shared/project-file-types'
import { fromString } from '../../../core/shared/property-path'
import {
  EditorState,
  EditorStatePatch,
  forUnderlyingTargetFromEditorState,
  insertElementAtPath,
  removeElementAtPath,
  withUnderlyingTargetFromEditorState,
} from '../../editor/store/editor-state'
import {
  BaseCommand,
  CommandFunction,
  getPatchForComponentChange,
  TransientOrNot,
} from './commands'

export interface DuplicateElement extends BaseCommand {
  type: 'DUPLICATE_ELEMENT'
  target: ElementPath
  newUid: string
}

export function duplicateElement(
  transient: TransientOrNot,
  target: ElementPath,
  newUid: string,
): DuplicateElement {
  return {
    type: 'DUPLICATE_ELEMENT',
    transient: transient,
    target: target,
    newUid: newUid,
  }
}

export const runDuplicateElement: CommandFunction<DuplicateElement> = (
  editorState: EditorState,
  command: DuplicateElement,
) => {
  const editorStatePatches: Array<EditorStatePatch> = withUnderlyingTargetFromEditorState(
    command.target,
    editorState,
    [],
    (successTarget, underlyingElementTarget, _underlyingTarget, underlyingFilePathTarget) => {
      const elementWithNewUid = setUtopiaID(underlyingElementTarget, command.newUid)
      const components = getUtopiaJSXComponentsFromSuccess(successTarget)
      const withElementDuplicated = insertElementAtPath(
        editorState.projectContents,
        underlyingFilePathTarget,
        EP.parentPath(command.target),
        elementWithNewUid,
        components,
        null,
      )

      return [
        getPatchForComponentChange(
          successTarget.topLevelElements,
          withElementDuplicated,
          successTarget.imports,
          underlyingFilePathTarget,
        ),
      ]
    },
  )

  return {
    editorStatePatches: editorStatePatches,
    commandDescription: `Duplicate Element ${EP.toUid(command.target)}`,
  }
}
