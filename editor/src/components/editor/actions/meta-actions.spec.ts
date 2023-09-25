import { cancelInsertModeActions } from './meta-actions'

describe('cancelInsertModeActions', () => {
  it(`returns the correct actions for 'apply-changes'`, () => {
    const result = cancelInsertModeActions('apply-changes')
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "action": "CLEAR_INTERACTION_SESSION",
          "applyChanges": true,
        },
        Object {
          "action": "SWITCH_EDITOR_MODE",
          "mode": Object {
            "area": false,
            "controlId": null,
            "toolbarMode": "none",
            "type": "select",
          },
          "unlessMode": "textEdit",
        },
        Object {
          "action": "SET_RIGHT_MENU_TAB",
          "tab": "inspector",
        },
        Object {
          "action": "CLEAR_HIGHLIGHTED_VIEWS",
        },
      ]
    `)
  })
  it(`returns the correct actions for 'do-not-apply-changes'`, () => {
    const result = cancelInsertModeActions('do-not-apply-changes')
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "action": "CLEAR_INTERACTION_SESSION",
          "applyChanges": false,
        },
        Object {
          "action": "SWITCH_EDITOR_MODE",
          "mode": Object {
            "area": false,
            "controlId": null,
            "toolbarMode": "none",
            "type": "select",
          },
          "unlessMode": "textEdit",
        },
        Object {
          "action": "SET_RIGHT_MENU_TAB",
          "tab": "inspector",
        },
        Object {
          "action": "CLEAR_HIGHLIGHTED_VIEWS",
        },
      ]
    `)
  })
  it(`returns the correct actions for 'ignore-it-completely'`, () => {
    const result = cancelInsertModeActions('ignore-it-completely')
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "action": "SWITCH_EDITOR_MODE",
          "mode": Object {
            "area": false,
            "controlId": null,
            "toolbarMode": "none",
            "type": "select",
          },
          "unlessMode": "textEdit",
        },
        Object {
          "action": "SET_RIGHT_MENU_TAB",
          "tab": "inspector",
        },
        Object {
          "action": "CLEAR_HIGHLIGHTED_VIEWS",
        },
      ]
    `)
  })
})
