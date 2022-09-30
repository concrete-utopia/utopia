import { UTOPIA_BACKEND } from '../../common/env-vars'
import urljoin from 'url-join'
import { GithubRepo, PersistentModel } from '../../components/editor/store/editor-state'
import { trimUpToAndIncluding } from './string-utils'
import { HEADERS, MODE } from '../../common/server'

export function parseGithubProjectString(maybeProject: string): GithubRepo | null {
  const withoutGithubPrefix = trimUpToAndIncluding('github.com/', maybeProject)

  const repoParts = withoutGithubPrefix.split('/')
  const owner = repoParts[0] ?? ''
  const repo = repoParts[1] ?? ''

  if (owner === '' || repo === '') {
    return null
  } else {
    return {
      owner: owner,
      repository: repo,
    }
  }
}

export async function saveProjectToGithub(persistentModel: PersistentModel): Promise<void> {
  const url = urljoin(UTOPIA_BACKEND, 'github', 'save')

  const postBody = JSON.stringify(persistentModel)
  const response = await fetch(url, {
    method: 'POST',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
    body: postBody,
  })
  if (!response.ok) {
    throw new Error(`Unexpected status returned from endpoint: ${response.status}`)
  }
}
