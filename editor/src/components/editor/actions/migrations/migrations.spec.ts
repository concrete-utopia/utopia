import { applyMigrations, CURRENT_PROJECT_VERSION } from './migrations'
import { persistentModelFromEditorModel, createEditorState } from '../../store/editor-state'
import { NO_OP } from '../../../../core/shared/utils'

describe('Migrations', () => {
  it('returns with the latest project version', () => {
    const emptyProject = persistentModelFromEditorModel(createEditorState(NO_OP))
    const migrated = applyMigrations(emptyProject)
    expect(migrated.projectVersion).toEqual(CURRENT_PROJECT_VERSION)
  })
})
