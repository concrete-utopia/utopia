import type { CreationDataFromProject } from '../model/storyboard-utils'
import type { FrameworkHooks } from './framework-hooks'

export class DefaultFrameworkHooks implements FrameworkHooks {
  detect(): boolean {
    return true
  }

  onProjectImport(): CreationDataFromProject | null {
    return null
  }

  onResolveModuleNotPresent(): string | null {
    return null
  }
}
