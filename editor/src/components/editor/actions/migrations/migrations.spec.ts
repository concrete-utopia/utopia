import { applyMigrations, CURRENT_PROJECT_VERSION } from './migrations'
import { persistentModelFromEditorModel, createEditorState } from '../../store/editor-state'
import { NO_OP } from '../../../../core/shared/utils'
import { generateCodeResultCache } from '../../../custom-code/code-file'

describe('Migrations', () => {
  it('returns with the latest project version', () => {
    const emptyProject = persistentModelFromEditorModel(
      createEditorState(generateCodeResultCache({}, {}, [], {}, NO_OP, [], 'full-build')),
    )
    const migrated = applyMigrations(emptyProject)
    expect(migrated.projectVersion).toEqual(CURRENT_PROJECT_VERSION)
  })

  it('returns with the latest project version for a non-versioned project', () => {
    const emptyProject = persistentModelFromEditorModel(
      createEditorState(generateCodeResultCache({}, {}, [], {}, NO_OP, [], 'full-build')),
    )
    delete emptyProject.projectVersion
    const migrated = applyMigrations(emptyProject)
    expect(migrated.projectVersion).toEqual(CURRENT_PROJECT_VERSION)
  })
})
