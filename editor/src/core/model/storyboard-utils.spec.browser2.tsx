import { getProjectFileByFilePath } from '../../components/assets'
import { mouseClickAtPoint } from '../../components/canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../../components/canvas/ui-jsx.test-utils'
import {
  makeTestProjectCodeWithSnippet,
  optOutFromCheckFileTimestamps,
  renderTestEditorWithCode,
  renderTestEditorWithModel,
} from '../../components/canvas/ui-jsx.test-utils'
import { StoryboardFilePath } from '../../components/editor/store/editor-state'
import { emptyDefaultProject } from '../../sample-projects/sample-project-utils'
import { createModifiedProject } from '../../sample-projects/sample-project-utils.test-utils'
import { GithubEndpoints } from '../shared/github/endpoints'
import type { GetGithubUserSuccess, GetBranchContentResponse } from '../shared/github/helpers'
import {
  MockGithubOperations,
  fakeResponse,
  loginUserToGithubForTests,
} from '../shared/github/operations/github-operations.test-utils'
import type { GetBranchesSuccess } from '../shared/github/operations/list-branches'
import type { GetUsersPublicRepositoriesSuccess } from '../shared/github/operations/load-repositories'

/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "expectStoryboardFileExists"] }] */

describe('Storyboard utils', () => {
  describe("Create a storyboard file if one doesn't exist in the project", () => {
    it('Remix project', async () => {
      const RemixRootComponentTestId = 'remix-root-component'
      const renderResult = await renderTestEditorWithModel(
        createModifiedProject(
          {
            '/package.json': `{
                "name": "Utopia Project",
                "version": "0.1.0",
                "utopia": {
                  "main-ui": "utopia/storyboard.js",
                  "html": "public/index.html",
                  "js": "src/index.js"
                },
                "dependencies": {
                  "react": "16.13.1",
                  "react-dom": "16.13.1",
                  "utopia-api": "0.4.1",
                  "react-spring": "8.0.27",
                  "@heroicons/react": "1.0.1",
                  "@emotion/react": "11.9.3",
                  "@remix-run/react": "1.19.3"
                }
              }`,
            '/app/root.js': `import React from 'react'
              
              export default function Root() {
                return (
                  <div data-testid="${RemixRootComponentTestId}">
                    "Hello Remix!"
                  </div>
                )
              }
              `,
          },
          emptyDefaultProject(),
        ),
        'await-first-dom-report',
      )

      expectStoryboardFileExists(renderResult)
      expect(renderResult.renderedDOM.queryByTestId(RemixRootComponentTestId)).not.toBeNull()
    })
    it('project with an exported App component', async () => {
      const AppTestId = 'App'
      const renderResult = await renderTestEditorWithModel(
        createModifiedProject(
          {
            '/src/app.js': `import React from 'react'
                  
                  export function App() {
                    return (
                      <div data-testid="${AppTestId}">
                        This is App
                      </div>
                    )
                  }
                  `,
          },
          emptyDefaultProject(),
        ),
        'await-first-dom-report',
      )

      expectStoryboardFileExists(renderResult)
      expect(renderResult.renderedDOM.queryAllByTestId(AppTestId)).not.toBeNull()
    })
    it('project with no main component', async () => {
      const renderResult = await renderTestEditorWithModel(
        createModifiedProject(
          {
            '/src/editor.js': `import React from 'react'
                  
                  export function Editor() {
                    return (
                      <div>
                        This is the entry point, but Utopia doesn't know that
                      </div>
                    )
                  }
                  `,
          },
          emptyDefaultProject(),
        ),
        'await-first-dom-report',
      )

      expectStoryboardFileExists(renderResult)
    })
  })
  describe('Create storyboard file on github import', () => {
    const RemixRootComponentTestId = 'remix-root-component'
    const { projectContents } = createModifiedProject(
      {
        '/package.json': `{
            "name": "Utopia Project",
            "version": "0.1.0",
            "utopia": {
              "main-ui": "utopia/storyboard.js",
              "html": "public/index.html",
              "js": "src/index.js"
            },
            "dependencies": {
              "react": "16.13.1",
              "react-dom": "16.13.1",
              "utopia-api": "0.4.1",
              "react-spring": "8.0.27",
              "@heroicons/react": "1.0.1",
              "@emotion/react": "11.9.3",
              "@remix-run/react": "1.19.3"
            }
          }`,
        '/app/root.js': `import React from 'react'
          
          export default function Root() {
            return (
              <div data-testid="${RemixRootComponentTestId}">
                "Hello Remix!"
              </div>
            )
          }
          `,
      },
      emptyDefaultProject(),
    )
    const mock = new MockGithubOperations().mock({
      fakeResolvedProjectContents: projectContents,
      apiConfig: {
        [GithubEndpoints.userDetails()]: fakeResponse<GetGithubUserSuccess>({
          type: 'SUCCESS',
          user: { login: 'login', name: 'Bob', avatarURL: 'avatar', htmlURL: 'www.example.com' },
        }),
        [GithubEndpoints.repositories()]: fakeResponse<GetUsersPublicRepositoriesSuccess>({
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
        [GithubEndpoints.getBranches({ owner: 'bob', repository: 'awesome-project' })]:
          fakeResponse<GetBranchesSuccess>({
            type: 'SUCCESS',
            branches: [{ name: 'main' }, { name: 'dev' }],
          }),
        [GithubEndpoints.branchContents({ owner: 'bob', repository: 'awesome-project' }, 'main')]:
          fakeResponse<GetBranchContentResponse>({
            type: 'SUCCESS',
            branch: {
              originCommit: 'initial',
              content: projectContents,
            },
          }),
      },
    })

    optOutFromCheckFileTimestamps()

    it('storyboard file is added after cloning a github project', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
            <h1>Starting Editor</h1>
            `),
        'await-first-dom-report',
      )

      await loginUserToGithubForTests(renderResult.dispatch)

      await clickTextOnScreen(renderResult, 'Github')
      await clickTextOnScreen(renderResult, 'Refresh list')

      await mock.getUsersPublicGithubRepositories

      await clickTextOnScreen(renderResult, 'bob/awesome-project')
      await mock.getBranchesForGithubRepository

      await clickTextOnScreen(renderResult, 'main')
      await clickTextOnScreen(renderResult, 'Load from Branch')
      await clickTextOnScreen(renderResult, 'Yes, Load from this Branch.')
      await mock.updateProjectWithBranchContent

      expectStoryboardFileExists(renderResult)
      expect(renderResult.renderedDOM.queryByTestId(RemixRootComponentTestId)).not.toBeNull()
    })
  })
})

function expectStoryboardFileExists(renderResult: EditorRenderResult) {
  const storyboardFile = getProjectFileByFilePath(
    renderResult.getEditorState().editor.projectContents,
    StoryboardFilePath,
  )
  expect(storyboardFile).not.toBeNull()
}

async function clickTextOnScreen(renderResult: EditorRenderResult, text: string) {
  const element = renderResult.renderedDOM.getByText(text)
  await mouseClickAtPoint(element, { x: 2, y: 2 })
}
