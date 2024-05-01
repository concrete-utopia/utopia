import { mouseClickAtPoint } from '../../../components/canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../../../components/canvas/ui-jsx.test-utils'
import {
  makeTestProjectCodeWithSnippet,
  optOutFromCheckFileTimestamps,
  renderTestEditorWithCode,
} from '../../../components/canvas/ui-jsx.test-utils'
import {
  createModifiedProject,
  createTestProjectWithCode,
} from '../../../sample-projects/sample-project-utils.test-utils'
import { wait } from '../../model/performance-scripts'
import { GithubEndpoints } from './endpoints'
import type { GetBranchContentResponse, GetGithubUserSuccess } from './helpers'
import {
  MockGithubOperations,
  fakeResponse,
  loginUserToGithubForTests,
} from './operations/github-operations.test-utils'
import type { GetBranchesSuccess } from './operations/list-branches'
import type { GetUsersPublicRepositoriesSuccess } from './operations/load-repositories'

describe('Github integration', () => {
  const mock = new MockGithubOperations().mock({
    fakeResolvedProjectContents: createTestProjectWithCode(
      makeTestProjectCodeWithSnippet(`
    <h1>Editor from Github</h1>
    `),
    ).projectContents,

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
            content: createModifiedProject({
              ['/utopia/storyboard.js']: makeTestProjectCodeWithSnippet(`
            <h1>Editor from Github</h1>
                `),
              ['/src/card.js']: `import React from 'react'

                export const Card = ({ label }) => {
                  return <div>{label}</div>
                }
                `,
              ['/utopia/components.utopia.js']: `import { Card } from '../src/card'
        
                const Components = {
              '/src/card': {
                Card: {
                  component: Card,
                  properties: {
                    label: {
                      control: 'string-input',
                    },
                    background: {
                      control: 'color',
                    },
                    visible: {
                      control: 'checkbox',
                      defaultValue: true,
                    },
                  },
                  focus: 'default',
                  inspector: ['visual', 'typography'],
                  emphasis: 'regular',
                  icon: 'regular',
                  variants: [
                    {
                      code: '<Card />',
                      imports: 'import { Card } from "/src/card"',
                      label: 'Card',
                    },
                    {
                      code: '<Card person={DefaultPerson} />',
                      label: 'ID Card',
                      imports: [
                        'import { Card } from "/src/card"',
                        "import { DefaultPerson } from '/src/defaults';",
                      ],
                    },
                  ],
                },
              },
            }
            
            export default Components
          `,
            }).projectContents,
          },
        }),
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
    await clickTextOnScreen(renderResult, 'Refresh list')

    await mock.getUsersPublicGithubRepositories

    await clickTextOnScreen(renderResult, 'bob/awesome-project')
    await mock.getBranchesForGithubRepository

    await clickTextOnScreen(renderResult, 'main')
    await clickTextOnScreen(renderResult, 'Load from Branch')
    await clickTextOnScreen(renderResult, 'Yes, Load from this Branch.')
    await mock.updateProjectWithBranchContent

    expect(renderResult.renderedDOM.getByText('Editor from Github')).toBeDefined()
  })

  it('updates property controls after cloning the branch', async () => {
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

    expect(renderResult.getEditorState().editor.propertyControlsInfo).toHaveProperty('/src/card')
    expect(renderResult.getEditorState().editor.propertyControlsInfo['/src/card']).toHaveProperty(
      'Card',
    )
    expect(
      renderResult.getEditorState().editor.propertyControlsInfo['/src/card']['Card'].properties,
    ).toHaveProperty('label')
  })
})

async function clickTextOnScreen(renderResult: EditorRenderResult, text: string) {
  const element = renderResult.renderedDOM.getByText(text)
  await mouseClickAtPoint(element, { x: 2, y: 2 })
}
