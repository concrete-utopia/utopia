import type { ProjectContentTreeRoot } from '../../../../components/assets'
import type { UtopiaTsWorkers } from '../../../workers/common/worker-types'

export type GithubFetch = (url: string, options: RequestInit) => Promise<Response>

export interface GithubOperationContext {
  fetch: GithubFetch
  updateProjectContentsWithParseResults: (
    workers: UtopiaTsWorkers,
    projectContents: ProjectContentTreeRoot,
  ) => Promise<ProjectContentTreeRoot>
}
