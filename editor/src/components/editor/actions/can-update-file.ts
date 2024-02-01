import { isOlderThan, canUpdateRevisionsState } from '../../../core/model/project-file-utils'
import type { ProjectFile } from '../../../core/shared/project-file-types'
import { isTextFile, isImageFile, isAssetFile } from '../../../core/shared/project-file-types'
import {
  ImageFileKeepDeepEquality,
  AssetFileKeepDeepEquality,
} from '../store/store-deep-equality-instances'

export function updateFileIfPossible(
  updated: ProjectFile,
  existing: ProjectFile | null,
): ProjectFile | 'cant-update' {
  if (existing == null || existing.type !== updated.type) {
    return updated
  }

  if (
    isTextFile(updated) &&
    isTextFile(existing) &&
    isOlderThan(existing, updated) &&
    canUpdateRevisionsState(
      updated.fileContents.revisionsState,
      existing.fileContents.revisionsState,
    )
  ) {
    return updated
  }

  if (isImageFile(updated) && existing != null && isImageFile(existing)) {
    if (ImageFileKeepDeepEquality(existing, updated).areEqual) {
      return 'cant-update'
    }
  }

  if (isAssetFile(updated) && existing != null && isAssetFile(existing)) {
    if (AssetFileKeepDeepEquality(existing, updated).areEqual) {
      return 'cant-update'
    }
  }

  return 'cant-update'
}
