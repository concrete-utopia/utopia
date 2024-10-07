import type { ProjectContentTreeRoot } from 'utopia-shared/src/types'
import type { CreationDataFromProject } from '../model/storyboard-utils'
import type { NodeModules } from '../shared/project-file-types'
import { ViteFrameworkHooks } from './framework-hooks-vite'
import { DefaultFrameworkHooks } from './framework-hooks-default'

export interface FrameworkHooks {
  detect: (projectContents: ProjectContentTreeRoot) => boolean
  onProjectImport: (projectContents: ProjectContentTreeRoot) => CreationDataFromProject | null
  onResolveModuleNotPresent: (
    projectContents: ProjectContentTreeRoot,
    nodeModules: NodeModules,
    importOrigin: string,
    toImport: string,
  ) => string | null
}

const defaultFrameworkHooks = new DefaultFrameworkHooks()
const frameworkHooks: Array<FrameworkHooks> = [new ViteFrameworkHooks(), defaultFrameworkHooks]

export function getFrameworkHooks(projectContents: ProjectContentTreeRoot): FrameworkHooks {
  return (
    frameworkHooks.find((framework) => framework.detect(projectContents)) ?? defaultFrameworkHooks
  )
}
