import { windowPoint } from '../../core/shared/math-utils'
import { DefaultPanels, dragPaneToNewPosition } from './floating-panels-state'

describe('dragPaneToNewPosition', () => {
  it('drag navigator to the left of the code editor', () => {
    const result = dragPaneToNewPosition(
      DefaultPanels,
      { width: 1000, height: 1000 },
      'navigator',
      'leftMenu2',
      windowPoint({
        x: 20,
        y: 500,
      }),
    )
    expect(result).toMatchInlineSnapshot(`
      Object {
        "panelContent": Object {
          "leftMenu1": Array [
            Object {
              "frame": Object {
                "height": 980,
                "width": 260,
                "x": 10,
                "y": 0,
              },
              "name": "navigator",
              "type": "menu",
            },
          ],
          "leftMenu2": Array [
            Object {
              "frame": Object {
                "height": 600,
                "width": 500,
                "x": 280,
                "y": 0,
              },
              "name": "code-editor",
              "type": "pane",
            },
          ],
          "rightMenu1": Array [
            Object {
              "frame": Object {
                "height": 980,
                "width": 255,
                "x": 735,
                "y": 0,
              },
              "name": "inspector",
              "type": "menu",
            },
          ],
          "rightMenu2": Array [],
        },
      }
    `)
  })
})
