import { NO_OP } from '../../../core/shared/utils'
import { PersistenceMachine } from './persistence'
import { createDummyPersistenceBackend } from './generic/dummy-persistence-backend'
import { PersistentModel } from '../store/editor-state'
import { ProjectFile } from '../../../core/shared/project-file-types'
import { PersistenceBackendAPI } from './generic/persistence-types'

const DummyPersistenceBackend: PersistenceBackendAPI<
  PersistentModel,
  ProjectFile
> = createDummyPersistenceBackend()

export const DummyPersistenceMachine: PersistenceMachine = new PersistenceMachine(
  DummyPersistenceBackend,
  NO_OP,
  NO_OP,
  NO_OP,
)
