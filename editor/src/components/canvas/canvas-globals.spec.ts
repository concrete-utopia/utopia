import { right } from '../../core/shared/either'
import {
  emptyComments,
  jsxAttributesEntry,
  jsExpressionValue,
  jsxElementWithoutUID,
  clearJSXElementWithoutUIDUniqueIDs,
} from '../../core/shared/element-template'
import { importAlias, importDetails } from '../../core/shared/project-file-types'
import type { DispatchPriority, EditorAction, EditorDispatch } from '../editor/action-types'
import type { ControlsToCheck } from './canvas-globals'
import {
  addRegisteredControls,
  clearAllRegisteredControls,
  validateControlsToCheck,
} from './canvas-globals'
import type {
  ComponentDescriptor,
  ComponentDescriptorWithName,
  PropertyControlsInfo,
} from '../custom-code/code-file'

const cardComponentDescriptor: ComponentDescriptor = {
  properties: {
    title: {
      control: 'string-input',
      label: 'Title',
    },
  },
  supportsChildren: false,
  preferredChildComponents: [],
  variants: [
    {
      insertMenuLabel: 'Card Default',
      elementToInsert: () =>
        clearJSXElementWithoutUIDUniqueIDs(
          jsxElementWithoutUID(
            'Card',
            [
              jsxAttributesEntry(
                'title',
                jsExpressionValue('Default', emptyComments),
                emptyComments,
              ),
            ],
            [],
          ),
        ),
      importsToAdd: {
        ['/src/card']: importDetails(null, [importAlias('Card')], null),
      },
    },
  ],
}

const cardComponentDescriptorWithName: ComponentDescriptorWithName = {
  ...cardComponentDescriptor,
  componentName: 'Card',
}

const cardControlsToCheck: ControlsToCheck = Promise.resolve(
  right([cardComponentDescriptorWithName]),
)

const cardPropertyControlsInfo: PropertyControlsInfo = {
  ['/src/card']: {
    Card: cardComponentDescriptor,
  },
}

const modifiedCardComponentDescriptor: ComponentDescriptor = {
  properties: {
    title: {
      control: 'string-input',
      label: 'Title',
    },
    border: {
      control: 'string-input',
      label: 'Border',
    },
  },
  supportsChildren: false,
  preferredChildComponents: [],
  variants: [
    {
      insertMenuLabel: 'Card Default',
      elementToInsert: () =>
        clearJSXElementWithoutUIDUniqueIDs(
          jsxElementWithoutUID(
            'Card',
            [
              jsxAttributesEntry(
                'title',
                jsExpressionValue('Default', emptyComments),
                emptyComments,
              ),
              jsxAttributesEntry(
                'border',
                jsExpressionValue('shiny', emptyComments),
                emptyComments,
              ),
            ],
            [],
          ),
        ),
      importsToAdd: {
        ['/src/card']: importDetails(null, [importAlias('Card')], null),
      },
    },
  ],
}

const modifiedCardComponentDescriptorWithName: ComponentDescriptorWithName = {
  ...modifiedCardComponentDescriptor,
  componentName: 'Card',
}

const modifiedCardControlsToCheck: ControlsToCheck = Promise.resolve(
  right([modifiedCardComponentDescriptorWithName]),
)

const selectorComponentDescriptor: ComponentDescriptor = {
  properties: {
    value: {
      control: 'popuplist',
      label: 'Value',
      options: ['True', 'False', 'FileNotFound'],
    },
  },
  supportsChildren: false,
  preferredChildComponents: [],
  variants: [
    {
      insertMenuLabel: 'True False Selector',
      elementToInsert: () =>
        clearJSXElementWithoutUIDUniqueIDs(
          jsxElementWithoutUID(
            'Selector',
            [
              jsxAttributesEntry(
                'value',
                jsExpressionValue(`'FileNotFound'`, emptyComments),
                emptyComments,
              ),
            ],
            [],
          ),
        ),
      importsToAdd: {
        ['/src/selector']: importDetails(null, [importAlias('Selector')], null),
      },
    },
  ],
}

const selectorComponentDescriptorWithName: ComponentDescriptorWithName = {
  ...selectorComponentDescriptor,
  componentName: 'Selector',
}

const selectorControlsToCheck: ControlsToCheck = Promise.resolve(
  right([selectorComponentDescriptorWithName]),
)

const selectorPropertyControlsInfo: PropertyControlsInfo = {
  ['/src/selector']: {
    Selector: selectorComponentDescriptor,
  },
}

const otherCardComponentDescriptorWithName: ComponentDescriptorWithName = {
  ...cardComponentDescriptor,
  componentName: 'Other Card',
}

const otherCardControlsToCheck: ControlsToCheck = Promise.resolve(
  right([otherCardComponentDescriptorWithName]),
)

describe('validateControlsToCheck', () => {
  beforeEach(() => {
    clearAllRegisteredControls()
  })
  afterAll(() => {
    clearAllRegisteredControls()
  })
  it('does nothing if no controls are added', async () => {
    let actionsDispatched: Array<EditorAction> = []
    const dispatch: EditorDispatch = (
      actions: ReadonlyArray<EditorAction>,
      priority?: DispatchPriority,
    ) => {
      actionsDispatched.push(...actions)
    }
    await validateControlsToCheck(dispatch, {}, [], [])
    expect(actionsDispatched).toMatchInlineSnapshot(`Array []`)
  })
  it('includes some controls added', async () => {
    let actionsDispatched: Array<EditorAction> = []
    const dispatch: EditorDispatch = (
      actions: ReadonlyArray<EditorAction>,
      priority?: DispatchPriority,
    ) => {
      actionsDispatched.push(...actions)
    }
    addRegisteredControls('test.js', '/src/card', cardControlsToCheck)
    await validateControlsToCheck(dispatch, {}, ['test.js'], ['test.js'])
    expect(actionsDispatched).toMatchInlineSnapshot(`
      Array [
        Object {
          "action": "UPDATE_PROPERTY_CONTROLS_INFO",
          "moduleNamesOrPathsToDelete": Array [],
          "propertyControlsInfo": Object {
            "/src/card": Object {
              "Card": Object {
                "preferredChildComponents": Array [],
                "properties": Object {
                  "title": Object {
                    "control": "string-input",
                    "label": "Title",
                  },
                },
                "supportsChildren": false,
                "variants": Array [
                  Object {
                    "elementToInsert": [Function],
                    "importsToAdd": Object {
                      "/src/card": Object {
                        "importedAs": null,
                        "importedFromWithin": Array [
                          Object {
                            "alias": "Card",
                            "name": "Card",
                          },
                        ],
                        "importedWithName": null,
                      },
                    },
                    "insertMenuLabel": "Card Default",
                  },
                ],
              },
            },
          },
        },
      ]
    `)
  })
  it('deletes the controls removed from a file', async () => {
    let actionsDispatched: Array<EditorAction> = []
    const dispatch: EditorDispatch = (
      actions: ReadonlyArray<EditorAction>,
      priority?: DispatchPriority,
    ) => {
      actionsDispatched.push(...actions)
    }
    // First add the controls
    addRegisteredControls('test.js', '/src/card', cardControlsToCheck)
    await validateControlsToCheck(dispatch, {}, ['test.js'], ['test.js'])

    // Clear the captured actions because we only care about actions dispatched by the next call
    actionsDispatched = []

    // As the second "evaluation" of 'test.js' didn't register controls, controls registered by the previous
    // evaluation will be removed
    await validateControlsToCheck(dispatch, cardPropertyControlsInfo, ['test.js'], ['test.js'])
    expect(actionsDispatched).toMatchInlineSnapshot(`
      Array [
        Object {
          "action": "UPDATE_PROPERTY_CONTROLS_INFO",
          "moduleNamesOrPathsToDelete": Array [
            "/src/card",
          ],
          "propertyControlsInfo": Object {},
        },
      ]
    `)
  })
  it('deletes the controls no longer imported', async () => {
    let actionsDispatched: Array<EditorAction> = []
    const dispatch: EditorDispatch = (
      actions: ReadonlyArray<EditorAction>,
      priority?: DispatchPriority,
    ) => {
      actionsDispatched.push(...actions)
    }
    // First add the controls
    addRegisteredControls('test.js', '/src/card', cardControlsToCheck)
    await validateControlsToCheck(dispatch, {}, ['test.js'], ['test.js'])

    // Clear the captured actions because we only care about actions dispatched by the next call
    actionsDispatched = []

    // As 'test.js' is no longer imported, it removes the controls registered by 'test.js' previously
    await validateControlsToCheck(dispatch, cardPropertyControlsInfo, [], [])
    expect(actionsDispatched).toMatchInlineSnapshot(`
      Array [
        Object {
          "action": "UPDATE_PROPERTY_CONTROLS_INFO",
          "moduleNamesOrPathsToDelete": Array [
            "/src/card",
          ],
          "propertyControlsInfo": Object {},
        },
      ]
    `)
  })
  it('not evaluating a file will not remove the controls registered by it', async () => {
    let actionsDispatched: Array<EditorAction> = []
    const dispatch: EditorDispatch = (
      actions: ReadonlyArray<EditorAction>,
      priority?: DispatchPriority,
    ) => {
      actionsDispatched.push(...actions)
    }
    // First add the controls
    addRegisteredControls('test.js', '/src/card', cardControlsToCheck)
    await validateControlsToCheck(dispatch, {}, ['test.js'], ['test.js'])

    // Clear the captured actions because we only care about actions dispatched by the next call
    actionsDispatched = []

    // 'test.js' is still imported, but not evaluated this time
    await validateControlsToCheck(dispatch, cardPropertyControlsInfo, ['test.js'], [])
    expect(actionsDispatched).toMatchInlineSnapshot(`Array []`)
  }),
    it('importing a file, then removing that import, then adding it again will register the controls even if not evaluated', async () => {
      let actionsDispatched: Array<EditorAction> = []
      const dispatch: EditorDispatch = (
        actions: ReadonlyArray<EditorAction>,
        priority?: DispatchPriority,
      ) => {
        actionsDispatched.push(...actions)
      }
      // First add the controls
      addRegisteredControls('test.js', '/src/card', cardControlsToCheck)
      await validateControlsToCheck(dispatch, {}, ['test.js'], ['test.js'])

      // As 'test.js' is no longer imported, it removes the controls registered by 'test.js' previously
      await validateControlsToCheck(dispatch, cardPropertyControlsInfo, [], [])

      // Clear the captured actions because we only care about actions dispatched by the next call
      actionsDispatched = []

      // 'test.js' is now imported again, but not evaluated this time as it hasn't changed
      await validateControlsToCheck(dispatch, {}, ['test.js'], [])

      expect(actionsDispatched).toMatchInlineSnapshot(`
        Array [
          Object {
            "action": "UPDATE_PROPERTY_CONTROLS_INFO",
            "moduleNamesOrPathsToDelete": Array [],
            "propertyControlsInfo": Object {
              "/src/card": Object {
                "Card": Object {
                  "preferredChildComponents": Array [],
                  "properties": Object {
                    "title": Object {
                      "control": "string-input",
                      "label": "Title",
                    },
                  },
                  "supportsChildren": false,
                  "variants": Array [
                    Object {
                      "elementToInsert": [Function],
                      "importsToAdd": Object {
                        "/src/card": Object {
                          "importedAs": null,
                          "importedFromWithin": Array [
                            Object {
                              "alias": "Card",
                              "name": "Card",
                            },
                          ],
                          "importedWithName": null,
                        },
                      },
                      "insertMenuLabel": "Card Default",
                    },
                  ],
                },
              },
            },
          },
        ]
      `)
    })
  it('includes newly added controls', async () => {
    let actionsDispatched: Array<EditorAction> = []
    const dispatch: EditorDispatch = (
      actions: ReadonlyArray<EditorAction>,
      priority?: DispatchPriority,
    ) => {
      actionsDispatched.push(...actions)
    }
    addRegisteredControls('test.js', '/src/card', cardControlsToCheck)
    addRegisteredControls('test.js', '/src/selector', selectorControlsToCheck)
    await validateControlsToCheck(dispatch, cardPropertyControlsInfo, ['test.js'], ['test.js'])
    expect(actionsDispatched).toMatchInlineSnapshot(`
      Array [
        Object {
          "action": "UPDATE_PROPERTY_CONTROLS_INFO",
          "moduleNamesOrPathsToDelete": Array [],
          "propertyControlsInfo": Object {
            "/src/selector": Object {
              "Selector": Object {
                "preferredChildComponents": Array [],
                "properties": Object {
                  "value": Object {
                    "control": "popuplist",
                    "label": "Value",
                    "options": Array [
                      "True",
                      "False",
                      "FileNotFound",
                    ],
                  },
                },
                "supportsChildren": false,
                "variants": Array [
                  Object {
                    "elementToInsert": [Function],
                    "importsToAdd": Object {
                      "/src/selector": Object {
                        "importedAs": null,
                        "importedFromWithin": Array [
                          Object {
                            "alias": "Selector",
                            "name": "Selector",
                          },
                        ],
                        "importedWithName": null,
                      },
                    },
                    "insertMenuLabel": "True False Selector",
                  },
                ],
              },
            },
          },
        },
      ]
    `)
  })
  it('includes modified controls', async () => {
    let actionsDispatched: Array<EditorAction> = []
    const dispatch: EditorDispatch = (
      actions: ReadonlyArray<EditorAction>,
      priority?: DispatchPriority,
    ) => {
      actionsDispatched.push(...actions)
    }
    addRegisteredControls('test.js', '/src/card', modifiedCardControlsToCheck)
    addRegisteredControls('test.js', '/src/selector', selectorControlsToCheck)
    await validateControlsToCheck(
      dispatch,
      {
        ...cardPropertyControlsInfo,
        ...selectorPropertyControlsInfo,
      },
      ['test.js'],
      ['test.js'],
    )
    expect(actionsDispatched).toMatchInlineSnapshot(`
      Array [
        Object {
          "action": "UPDATE_PROPERTY_CONTROLS_INFO",
          "moduleNamesOrPathsToDelete": Array [],
          "propertyControlsInfo": Object {
            "/src/card": Object {
              "Card": Object {
                "preferredChildComponents": Array [],
                "properties": Object {
                  "border": Object {
                    "control": "string-input",
                    "label": "Border",
                  },
                  "title": Object {
                    "control": "string-input",
                    "label": "Title",
                  },
                },
                "supportsChildren": false,
                "variants": Array [
                  Object {
                    "elementToInsert": [Function],
                    "importsToAdd": Object {
                      "/src/card": Object {
                        "importedAs": null,
                        "importedFromWithin": Array [
                          Object {
                            "alias": "Card",
                            "name": "Card",
                          },
                        ],
                        "importedWithName": null,
                      },
                    },
                    "insertMenuLabel": "Card Default",
                  },
                ],
              },
            },
          },
        },
      ]
    `)
  })
  it('merges multiple calls for the same module', async () => {
    let actionsDispatched: Array<EditorAction> = []
    const dispatch: EditorDispatch = (
      actions: ReadonlyArray<EditorAction>,
      priority?: DispatchPriority,
    ) => {
      actionsDispatched.push(...actions)
    }
    addRegisteredControls('test.js', '/src/card', cardControlsToCheck)
    addRegisteredControls('test.js', '/src/card', otherCardControlsToCheck)
    await validateControlsToCheck(dispatch, {}, ['test.js'], ['test.js'])
    expect(actionsDispatched).toMatchInlineSnapshot(`
      Array [
        Object {
          "action": "UPDATE_PROPERTY_CONTROLS_INFO",
          "moduleNamesOrPathsToDelete": Array [],
          "propertyControlsInfo": Object {
            "/src/card": Object {
              "Card": Object {
                "preferredChildComponents": Array [],
                "properties": Object {
                  "title": Object {
                    "control": "string-input",
                    "label": "Title",
                  },
                },
                "supportsChildren": false,
                "variants": Array [
                  Object {
                    "elementToInsert": [Function],
                    "importsToAdd": Object {
                      "/src/card": Object {
                        "importedAs": null,
                        "importedFromWithin": Array [
                          Object {
                            "alias": "Card",
                            "name": "Card",
                          },
                        ],
                        "importedWithName": null,
                      },
                    },
                    "insertMenuLabel": "Card Default",
                  },
                ],
              },
              "Other Card": Object {
                "preferredChildComponents": Array [],
                "properties": Object {
                  "title": Object {
                    "control": "string-input",
                    "label": "Title",
                  },
                },
                "supportsChildren": false,
                "variants": Array [
                  Object {
                    "elementToInsert": [Function],
                    "importsToAdd": Object {
                      "/src/card": Object {
                        "importedAs": null,
                        "importedFromWithin": Array [
                          Object {
                            "alias": "Card",
                            "name": "Card",
                          },
                        ],
                        "importedWithName": null,
                      },
                    },
                    "insertMenuLabel": "Card Default",
                  },
                ],
              },
            },
          },
        },
      ]
    `)
  })
})
