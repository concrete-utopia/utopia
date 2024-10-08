import { Project, TailwindConfigFileContents } from './tailwind.test-utils'
import { renderTestEditorWithModel } from '../../components/canvas/ui-jsx.test-utils'
import { TailwindConfigPath } from './tailwind-config'
import { updateFromCodeEditor } from '../../components/editor/actions/actions-from-vscode'

describe('tailwind config file in the editor', () => {
  it('is set during editor load', async () => {
    const editor = await renderTestEditorWithModel(Project, 'await-first-dom-report')

    expect(editor.getEditorState().editor.tailwindConfig).toMatchInlineSnapshot(`
      Object {
        "plugins": Array [
          [Function],
        ],
        "theme": Object {
          "colors": Object {
            "bermuda": "#78dcca",
            "bubble-gum": "#ff77e9",
            "current": "currentColor",
            "metal": "#565584",
            "midnight": "#121063",
            "purple": "#3f3cbb",
            "silver": "#ecebff",
            "tahiti": "#3ab7bf",
            "transparent": "transparent",
            "white": "#ffffff",
          },
        },
      }
    `)
  })
  it('is updated in the editor state when the tailwind config is updated', async () => {
    const editor = await renderTestEditorWithModel(Project, 'await-first-dom-report')

    await editor.dispatch(
      [
        updateFromCodeEditor(
          TailwindConfigPath,
          TailwindConfigFileContents,
          `
const Tailwind = {
    theme: {
      colors: {
        transparent: 'transparent',
        current: 'currentColor',
        white: '#ffffff',
      },
    },
    plugins: [ ],
  }
  export default Tailwind
`,
        ),
      ],
      true,
    )

    expect(editor.getEditorState().editor.tailwindConfig).toMatchInlineSnapshot(`
      Object {
        "plugins": Array [],
        "theme": Object {
          "colors": Object {
            "current": "currentColor",
            "transparent": "transparent",
            "white": "#ffffff",
          },
        },
      }
    `)
  })
})
