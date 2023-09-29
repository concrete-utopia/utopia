import { mouseClickAtPoint } from '../../../components/canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../../../components/canvas/ui-jsx.test-utils'
import {
  makeTestProjectCodeWithSnippet,
  optOutFromCheckFileTimestamps,
  renderTestEditorWithCode,
} from '../../../components/canvas/ui-jsx.test-utils'
import { createTestProjectWithCode } from '../../../sample-projects/sample-project-utils.test-utils'
import type { GetBranchContentResponse } from './helpers'
import { loginUserToGithubForTests } from './operations/github-operations.test-utils'
import type { GetBranchesSuccess } from './operations/list-branches'
import type { GetUsersPublicRepositoriesSuccess } from './operations/load-repositories'

describe('Github integration', () => {
  const mock = new MockGithubOperations().mock({
    fakeResolvedProjectContents: createTestProjectWithCode(
      makeTestProjectCodeWithSnippet(`
    <h1>Editor from Github</h1>
    `),
    ).projectContents,

    githubEndpoints: {
      repositories: async () =>
        fakeResponse<GetUsersPublicRepositoriesSuccess>({
          type: 'SUCCESS',
          repositories: [
            {
              fullName: 'bob/awesome-project',
              name: 'Awesome Project',
              avatarUrl: null,
              isPrivate: false,
              description: 'An Awesome Project',
              updatedAt: '1',
              defaultBranch: 'main',
              permissions: { admin: true, push: true, pull: true },
            },
          ],
        }),
      getBranches: async () =>
        fakeResponse<GetBranchesSuccess>({
          type: 'SUCCESS',
          branches: [{ name: 'main' }, { name: 'dev' }],
        }),
      branchContents: async () =>
        fakeResponse<GetBranchContentResponse>({
          type: 'SUCCESS',
          branch: {
            originCommit: 'initial',
            content: createTestProjectWithCode(
              makeTestProjectCodeWithSnippet(`
          <h1>Editor from Github</h1>
          `),
            ).projectContents,
          },
        }),
      save: async () => {
        throw new Error('fake data missing for `save` endpoint')
      },
      updatePullRequests: async () => {
        throw new Error('fake data missing for `updatePullRequests` endpoint')
      },
      asset: async () => {
        throw new Error('fake data missing for `asset` endpoint')
      },
    },
  })

  optOutFromCheckFileTimestamps()

  it('can clone a branch', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
          <h1>Starting Editor</h1>
          `),
      'await-first-dom-report',
    )

    await loginUserToGithubForTests(renderResult.dispatch)

    await clickTextOnScreen(renderResult, 'Github')
    await mock.getUsersPublicGithubRepositories

    await clickTextOnScreen(renderResult, 'bob/awesome-project')
    await mock.getBranchesForGithubRepository

    await clickTextOnScreen(renderResult, 'main')
    await clickTextOnScreen(renderResult, 'Load from Branch')
    await clickTextOnScreen(renderResult, 'Yes, Load from this Branch.')
    await mock.updateProjectWithBranchContent

    expect(renderResult.renderedDOM.getByText('Editor from Github')).toBeDefined()
  })
})

async function clickTextOnScreen(renderResult: EditorRenderResult, text: string) {
  const element = renderResult.renderedDOM.getByText(text)
  await mouseClickAtPoint(element, { x: 2, y: 2 })
}
