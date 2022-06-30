import {
  actions,
  assign,
  createMachine,
  DoneInvokeEvent,
  interpret,
  Interpreter,
  send,
} from 'xstate'
import type { Model } from 'xstate/lib/model.types'
import { ProjectFile } from '../../../core/shared/project-file-types'
import { projectURLForProject } from '../../../core/shared/utils'
import { notice } from '../../common/notice'
import { EditorAction, EditorDispatch } from '../action-types'
import {
  setForkedFromProjectID,
  setProjectID,
  setProjectName,
  showToast,
  updateFile,
} from '../actions/action-creators'
import { createNewProjectName, PersistentModel } from '../store/editor-state'
import {
  PersistenceContext,
  PersistenceEvent,
  SaveEvent,
  createPersistenceMachine,
  LoggedIn,
  Forking,
  CreatingProjectId,
  Ready,
  loadEvent,
  newEvent,
  saveEvent,
  forkEvent,
  userLogInEvent,
  userLogOutEvent,
} from './generic/persistence-machine'
import {
  PersistenceBackendAPI,
  ProjectLoadResult,
  ProjectModel,
  ProjectWithFileChanges,
} from './generic/persistence-types'

export class PersistenceMachine {
  private interpreter: Interpreter<
    PersistenceContext<PersistentModel>,
    any,
    PersistenceEvent<PersistentModel, ProjectFile>
  >
  private lastSavedTS: number = 0
  private throttledSaveTimeoutId: NodeJS.Timer | null = null
  private waitingThrottledSaveEvent: SaveEvent<PersistentModel> | null = null
  private queuedActions: Array<EditorAction> = [] // Queue up actions during events and transitions, then dispatch when ready
  private projectCreatedOrLoadedThisTick: boolean = false

  constructor(
    backendAPI: PersistenceBackendAPI<PersistentModel, ProjectFile>,
    dispatch: EditorDispatch,
    onProjectNotFound: () => void,
    onCreatedOrLoadedProject: (
      projectId: string,
      projectName: string,
      project: PersistentModel,
    ) => void,
    private saveThrottle: number = 30000,
  ) {
    this.interpreter = interpret(createPersistenceMachine<PersistentModel, ProjectFile>(backendAPI))

    this.interpreter.onTransition((state, event) => {
      if (state.changed) {
        switch (event.type) {
          case 'NEW_PROJECT_CREATED':
            if (state.matches({ user: LoggedIn })) {
              this.queuedActions.push(showToast(notice('Project successfully uploaded!')))
            } else {
              this.queuedActions.push(
                showToast(notice('Locally cached project. Sign in to share!')),
              )
            }

            this.projectCreatedOrLoadedThisTick = true
            break
          case 'LOAD_COMPLETE':
            this.projectCreatedOrLoadedThisTick = true
            break
          case 'PROJECT_ID_CREATED':
            this.queuedActions.push(setProjectID(event.projectId))
            break
          case 'LOAD_FAILED':
            onProjectNotFound()
            break
          case 'DOWNLOAD_ASSETS_COMPLETE': {
            if (state.matches({ core: { [Forking]: CreatingProjectId } })) {
              this.queuedActions.push(setForkedFromProjectID(state.context.projectId!))
              this.queuedActions.push(setProjectName(state.context.project!.name))
              this.queuedActions.push(showToast(notice('Project successfully forked!')))
            }

            const updateFileActions = event.downloadAssetsResult.filesWithFileNames.map(
              ({ fileName, file }) => updateFile(fileName, file, true),
            )
            this.queuedActions.push(...updateFileActions)
            break
          }
          case 'SAVE_COMPLETE':
            const updateFileActions = event.saveResult.filesWithFileNames.map(
              ({ fileName, file }) => updateFile(fileName, file, true),
            )
            this.queuedActions.push(...updateFileActions)
            this.lastSavedTS = Date.now()
            // TODO Show toasts after first server save of a previously local project
            break
        }

        if (state.matches({ core: Ready })) {
          if (this.projectCreatedOrLoadedThisTick) {
            this.projectCreatedOrLoadedThisTick = false

            onCreatedOrLoadedProject(
              state.context.projectId!,
              state.context.project!.name,
              state.context.project!.content,
            )
          }

          if (this.queuedActions.length > 0) {
            const actionsToDispatch = this.queuedActions
            this.queuedActions = []
            dispatch(actionsToDispatch)
          }
        }
      }
    })

    this.interpreter.start()

    window.addEventListener('beforeunload', (e) => {
      if (!this.isSafeToClose()) {
        this.sendThrottledSave()
        e.preventDefault()
        e.returnValue = ''
      }
    })
  }

  private isSafeToClose = (): boolean => {
    return this.waitingThrottledSaveEvent == null
  }

  private clearThrottledSave = (): void => {
    if (this.throttledSaveTimeoutId != null) {
      clearTimeout(this.throttledSaveTimeoutId)
      this.throttledSaveTimeoutId = null
    }

    this.waitingThrottledSaveEvent = null
  }

  private getRemainingSaveDelay = (): number => {
    return Math.max(0, this.lastSavedTS + this.saveThrottle - Date.now())
  }

  private shouldThrottle = (forceOrThrottle: 'force' | 'throttle'): boolean => {
    return forceOrThrottle === 'throttle' && this.getRemainingSaveDelay() > 0
  }

  // Exposed for testing
  sendThrottledSave = (): void => {
    if (this.waitingThrottledSaveEvent != null) {
      this.interpreter.send(this.waitingThrottledSaveEvent)
    }
    this.clearThrottledSave()
  }

  // API

  save = (
    projectName: string,
    project: PersistentModel,
    forceOrThrottle: 'force' | 'throttle' = 'throttle',
  ): void => {
    const eventToFire = saveEvent({ name: projectName, content: project })

    if (this.shouldThrottle(forceOrThrottle)) {
      this.waitingThrottledSaveEvent = eventToFire
      this.throttledSaveTimeoutId = setTimeout(this.sendThrottledSave, this.getRemainingSaveDelay())
    } else {
      this.interpreter.send(eventToFire)
    }
  }

  load = (projectId: string): void => {
    this.interpreter.send(loadEvent(projectId))
  }

  createNew = (projectName: string, project: PersistentModel): void => {
    this.interpreter.send(newEvent({ name: projectName, content: project }))
  }

  fork = (): void => {
    this.interpreter.send(forkEvent())
  }

  login = (): void => {
    this.interpreter.send(userLogInEvent())
  }

  logout = (): void => {
    this.interpreter.send(userLogOutEvent())
  }

  stop = (): void => {
    this.interpreter.stop()
    this.clearThrottledSave()
  }
}
