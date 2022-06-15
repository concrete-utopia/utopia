import { getZIndexOfElement, setUtopiaID } from '../../../core/model/element-template-utils'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import { UTOPIA_UID_KEY } from '../../../core/model/utopia-constants'
import { foldEither } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import {
  emptyComments,
  isUtopiaJSXComponent,
  jsxAttributeValue,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import { setJSXValuesAtPaths } from '../../../core/shared/jsx-attributes'
import { ElementPath } from '../../../core/shared/project-file-types'
import { fromString } from '../../../core/shared/property-path'
import { before } from '../../../utils/utils'
import {
  EditorState,
  EditorStatePatch,
  forUnderlyingTargetFromEditorState,
  insertElementAtPath,
  removeElementAtPath,
  withUnderlyingTargetFromEditorState,
} from '../../editor/store/editor-state'
import { duplicate } from '../canvas-utils'
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
  const targetParent = EP.parentPath(command.target)
  const newPath = EP.appendToPath(targetParent, command.newUid)

  const duplicateResult = duplicate([command.target], targetParent, editorState, [
    { originalPath: command.target, newUID: command.newUid },
  ])

  const originalParsedFile = withUnderlyingTargetFromEditorState(
    command.target,
    editorState,
    null,
    (parseSuccess, _, __, filePath) => ({ parseSuccess, filePath }),
  )

  if (duplicateResult == null || originalParsedFile == null) {
    return { editorStatePatches: [], commandDescription: `Duplicate Element Failed` }
  }

  const newUtopiaComponents = withUnderlyingTargetFromEditorState(
    newPath,
    duplicateResult.updatedEditorState,
    [],
    (parsedFile) => {
      return parsedFile.topLevelElements.filter(isUtopiaJSXComponent) // TODO why can't getPatchForComponentChange just take all top level elements?
    },
  )

  const editorStatePatch: EditorStatePatch = getPatchForComponentChange(
    originalParsedFile?.parseSuccess.topLevelElements,
    newUtopiaComponents,
    originalParsedFile?.parseSuccess.imports,
    originalParsedFile?.filePath,
  )

  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Duplicate Element ${EP.toUid(command.target)}`,
  }
}
