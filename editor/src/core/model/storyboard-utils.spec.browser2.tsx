import { getProjectFileByFilePath } from '../../components/assets'
import type { EditorRenderResult } from '../../components/canvas/ui-jsx.test-utils'
import { renderTestEditorWithModel } from '../../components/canvas/ui-jsx.test-utils'
import { StoryboardFilePath } from '../../components/editor/store/editor-state'
import { emptyDefaultProject } from '../../sample-projects/sample-project-utils'
import { createModifiedProject } from '../../sample-projects/sample-project-utils.test-utils'

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
})

function expectStoryboardFileExists(renderResult: EditorRenderResult) {
  const storyboardFile = getProjectFileByFilePath(
    renderResult.getEditorState().editor.projectContents,
    StoryboardFilePath,
  )
  expect(storyboardFile).not.toBeNull()
}
