import { ContextMenuItem } from '../../components/context-menu-items'
import { EditorDispatch } from '../../components/editor/action-types'
import { GithubRepo } from '../../components/editor/store/editor-state'
import { Conflict, resolveConflict } from './github/helpers'

export function getConflictMenuItems(
  githubRepo: GithubRepo,
  projectID: string,
  dispatch: EditorDispatch,
  path: string,
  conflict: Conflict,
  submenuName: string | undefined,
): Array<ContextMenuItem<unknown>> {
  function applyChange(whichChange: 'utopia' | 'branch'): void {
    void resolveConflict(githubRepo, projectID, path, conflict, whichChange, dispatch)
  }
  switch (conflict.type) {
    case 'DIFFERING_TYPES':
      return [
        {
          name: 'Accept what is in Utopia.',
          enabled: true,
          action: () => {
            applyChange('utopia')
          },
          submenuName: submenuName,
        },
        {
          name: 'Apply what is in Github.',
          enabled: true,
          action: () => {
            applyChange('branch')
          },
          submenuName: submenuName,
        },
      ]
    case 'CURRENT_DELETED_BRANCH_CHANGED':
      return [
        {
          name: 'Delete the file.',
          enabled: true,
          action: () => {
            applyChange('utopia')
          },
          submenuName: submenuName,
        },
        {
          name: 'Restore the file from Github.',
          enabled: true,
          action: () => {
            applyChange('branch')
          },
          submenuName: submenuName,
        },
      ]

    case 'CURRENT_CHANGED_BRANCH_DELETED':
      return [
        {
          name: 'Keep the file in Utopia.',
          enabled: true,
          action: () => {
            applyChange('utopia')
          },
          submenuName: submenuName,
        },
        {
          name: 'Delete the file.',
          enabled: true,
          action: () => {
            applyChange('branch')
          },
          submenuName: submenuName,
        },
      ]

    default:
      const _exhaustiveCheck: never = conflict
      throw new Error(`Unhandled conflict type ${JSON.stringify(conflict)}`)
  }
}
