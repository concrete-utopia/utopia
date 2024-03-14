import { NO_OP } from '../../../core/shared/utils'
import { PersistenceMachine } from './persistence'
import { createDummyPersistenceBackend } from './generic/dummy-persistence-backend'
import type { PersistentModel } from '../store/editor-state'
import type { ProjectFile } from '../../../core/shared/project-file-types'
import type { PersistenceBackendAPI } from './generic/persistence-types'

const DummyPersistenceBackend: PersistenceBackendAPI<PersistentModel, ProjectFile> =
  createDummyPersistenceBackend()

export const DummyPersistenceMachine: PersistenceMachine = new PersistenceMachine(
  DummyPersistenceBackend,
  NO_OP,
  NO_OP,
  NO_OP,
  NO_OP,
)
