import { applyMigrations, CURRENT_PROJECT_VERSION } from './migrations'
import { persistentModelFromEditorModel, createEditorState } from '../../store/editor-state'

describe('Migrations', () => {
  it('returns with the latest project version', () => {
    const emptyProject = persistentModelFromEditorModel(createEditorState())
    const migrated = applyMigrations(emptyProject)
    expect(migrated.projectVersion).toEqual(CURRENT_PROJECT_VERSION)
  })

  it('returns with the latest project version for a non-versioned project', () => {
    const emptyProject = persistentModelFromEditorModel(createEditorState())
    delete emptyProject.projectVersion
    const migrated = applyMigrations(emptyProject)
    expect(migrated.projectVersion).toEqual(CURRENT_PROJECT_VERSION)
  })
})
