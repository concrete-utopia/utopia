import { windowPoint } from '../../core/shared/math-utils'
import {
  DefaultPanels,
  dragPaneToNewPosition,
  updatePanelPositionsBasedOnLocationAndSize,
  updatePanelsToDefaultSizes,
} from './floating-panels-state'

describe('dragPaneToNewPosition', () => {
  const canvasSize = { width: 1000, height: 1000 }
  it('drag navigator to the left of the code editor, then drag the code editor to the left of the navigator', () => {
    const firstDragResult = dragPaneToNewPosition(
      updatePanelPositionsBasedOnLocationAndSize(
        updatePanelsToDefaultSizes(DefaultPanels, canvasSize),
        canvasSize,
      ),
      canvasSize,
      'navigator',
      'leftMenu1',
      windowPoint({
        x: 20,
        y: 500,
      }),
    )
    const secondDragResult = dragPaneToNewPosition(
      firstDragResult,
      canvasSize,
      'code-editor',
      'leftMenu2',
      windowPoint({
        x: 20,
        y: 500,
      }),
    )
    expect(secondDragResult).toMatchInlineSnapshot(`
      Object {
        "panelContent": Object {
          "leftMenu1": Array [
            Object {
              "frame": Object {
                "height": 1000,
                "width": 500,
                "x": 10,
                "y": 0,
              },
              "name": "code-editor",
              "type": "pane",
            },
          ],
          "leftMenu2": Array [
            Object {
              "frame": Object {
                "height": 980,
                "width": 260,
                "x": 520,
                "y": 0,
              },
              "name": "navigator",
              "type": "menu",
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
  it('drag navigator to the left of the code editor', () => {
    const result = dragPaneToNewPosition(
      updatePanelPositionsBasedOnLocationAndSize(
        updatePanelsToDefaultSizes(DefaultPanels, canvasSize),
        canvasSize,
      ),
      canvasSize,
      'navigator',
      'leftMenu1',
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
  it('drag navigator to the left of the inspector', () => {
    const result = dragPaneToNewPosition(
      updatePanelPositionsBasedOnLocationAndSize(
        updatePanelsToDefaultSizes(DefaultPanels, canvasSize),
        canvasSize,
      ),
      canvasSize,
      'navigator',
      'leftMenu1',
      windowPoint({
        x: 900,
        y: 500,
      }),
    )

    expect(result).toMatchInlineSnapshot(`
      Object {
        "panelContent": Object {
          "leftMenu1": Array [
            Object {
              "frame": Object {
                "height": 1000,
                "width": 500,
                "x": 10,
                "y": 0,
              },
              "name": "code-editor",
              "type": "pane",
            },
          ],
          "leftMenu2": Array [],
          "rightMenu1": Array [
            Object {
              "frame": Object {
                "height": 646.6666666666666,
                "width": 255,
                "x": 735,
                "y": 0,
              },
              "name": "inspector",
              "type": "menu",
            },
            Object {
              "frame": Object {
                "height": 313.3333333333333,
                "width": 255,
                "x": 735,
                "y": 656.6666666666666,
              },
              "name": "navigator",
              "type": "menu",
            },
          ],
          "rightMenu2": Array [],
        },
      }
    `)
  })
})
