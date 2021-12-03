import { right } from '../../core/shared/either'
import {
  emptyComments,
  jsxAttributesEntry,
  jsxAttributeValue,
  jsxElementWithoutUID,
} from '../../core/shared/element-template'
import { importAlias, importDetails } from '../../core/shared/project-file-types'
import { DispatchPriority, EditorAction, EditorDispatch } from '../editor/action-types'
import {
  addControlsToCheck,
  ControlsToCheck,
  resetControlsToCheck,
  validateControlsToCheck,
} from './canvas-globals'
import {
  ComponentDescriptor,
  ComponentDescriptorWithName,
  PropertyControlsInfo,
} from '../custom-code/code-file'

const cardComponentDescriptor: ComponentDescriptor = {
  propertyControls: right({
    title: right({
      control: 'string-input',
      label: 'Title',
    }),
  }),
  insertOptions: [
    {
      insertMenuLabel: 'Card Default',
      elementToInsert: jsxElementWithoutUID(
        'Card',
        [jsxAttributesEntry('title', jsxAttributeValue('Default', emptyComments), emptyComments)],
        [],
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
  propertyControls: right({
    title: right({
      control: 'string-input',
      label: 'Title',
    }),
    border: right({
      control: 'string-input',
      label: 'Border',
    }),
  }),
  insertOptions: [
    {
      insertMenuLabel: 'Card Default',
      elementToInsert: jsxElementWithoutUID(
        'Card',
        [
          jsxAttributesEntry('title', jsxAttributeValue('Default', emptyComments), emptyComments),
          jsxAttributesEntry('border', jsxAttributeValue('shiny', emptyComments), emptyComments),
        ],
        [],
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
  propertyControls: right({
    value: right({
      control: 'popuplist',
      label: 'Value',
      options: ['True', 'False', 'FileNotFound'],
    }),
  }),
  insertOptions: [
    {
      insertMenuLabel: 'True False Selector',
      elementToInsert: jsxElementWithoutUID(
        'Selector',
        [
          jsxAttributesEntry(
            'value',
            jsxAttributeValue(`'FileNotFound'`, emptyComments),
            emptyComments,
          ),
        ],
        [],
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

describe('validateControlsToCheck', () => {
  beforeEach(() => {
    // Twice because the first will leave some data in `previousModuleNamesOrPaths`.
    resetControlsToCheck()
    resetControlsToCheck()
  })
  it('does nothing if no controls are added', async () => {
    let actionsDispatched: Array<EditorAction> = []
    const dispatch: EditorDispatch = (
      actions: ReadonlyArray<EditorAction>,
      priority?: DispatchPriority,
    ) => {
      actionsDispatched.push(...actions)
    }
    await validateControlsToCheck(dispatch, {})
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
    addControlsToCheck('/src/card', cardControlsToCheck)
    await validateControlsToCheck(dispatch, {})
    expect(actionsDispatched).toMatchInlineSnapshot(`
      Array [
        Object {
          "action": "UPDATE_PROPERTY_CONTROLS_INFO",
          "moduleNamesOrPathsToDelete": Array [],
          "propertyControlsInfo": Object {
            "/src/card": Object {
              "Card": Object {
                "insertOptions": Array [
                  Object {
                    "elementToInsert": Object {
                      "children": Array [],
                      "name": Object {
                        "baseVariable": "Card",
                        "propertyPath": Object {
                          "propertyElements": Array [],
                        },
                      },
                      "props": Array [
                        Object {
                          "comments": Object {
                            "leadingComments": Array [],
                            "trailingComments": Array [],
                          },
                          "key": "title",
                          "type": "JSX_ATTRIBUTES_ENTRY",
                          "value": Object {
                            "comments": Object {
                              "leadingComments": Array [],
                              "trailingComments": Array [],
                            },
                            "type": "ATTRIBUTE_VALUE",
                            "value": "Default",
                          },
                        },
                      ],
                    },
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
                "propertyControls": Object {
                  "type": "RIGHT",
                  "value": Object {
                    "title": Object {
                      "type": "RIGHT",
                      "value": Object {
                        "control": "string-input",
                        "label": "Title",
                      },
                    },
                  },
                },
              },
            },
          },
        },
      ]
    `)
  })
  it('deletes the controls removed', async () => {
    let actionsDispatched: Array<EditorAction> = []
    const dispatch: EditorDispatch = (
      actions: ReadonlyArray<EditorAction>,
      priority?: DispatchPriority,
    ) => {
      actionsDispatched.push(...actions)
    }
    addControlsToCheck('/src/card', cardControlsToCheck)
    resetControlsToCheck()
    await validateControlsToCheck(dispatch, cardPropertyControlsInfo)
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
  it('includes newly added controls', async () => {
    let actionsDispatched: Array<EditorAction> = []
    const dispatch: EditorDispatch = (
      actions: ReadonlyArray<EditorAction>,
      priority?: DispatchPriority,
    ) => {
      actionsDispatched.push(...actions)
    }
    addControlsToCheck('/src/card', cardControlsToCheck)
    addControlsToCheck('/src/selector', selectorControlsToCheck)
    await validateControlsToCheck(dispatch, cardPropertyControlsInfo)
    expect(actionsDispatched).toMatchInlineSnapshot(`
      Array [
        Object {
          "action": "UPDATE_PROPERTY_CONTROLS_INFO",
          "moduleNamesOrPathsToDelete": Array [],
          "propertyControlsInfo": Object {
            "/src/selector": Object {
              "Selector": Object {
                "insertOptions": Array [
                  Object {
                    "elementToInsert": Object {
                      "children": Array [],
                      "name": Object {
                        "baseVariable": "Selector",
                        "propertyPath": Object {
                          "propertyElements": Array [],
                        },
                      },
                      "props": Array [
                        Object {
                          "comments": Object {
                            "leadingComments": Array [],
                            "trailingComments": Array [],
                          },
                          "key": "value",
                          "type": "JSX_ATTRIBUTES_ENTRY",
                          "value": Object {
                            "comments": Object {
                              "leadingComments": Array [],
                              "trailingComments": Array [],
                            },
                            "type": "ATTRIBUTE_VALUE",
                            "value": "'FileNotFound'",
                          },
                        },
                      ],
                    },
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
                "propertyControls": Object {
                  "type": "RIGHT",
                  "value": Object {
                    "value": Object {
                      "type": "RIGHT",
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
                  },
                },
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
    addControlsToCheck('/src/card', modifiedCardControlsToCheck)
    addControlsToCheck('/src/selector', selectorControlsToCheck)
    await validateControlsToCheck(dispatch, {
      ...cardPropertyControlsInfo,
      ...selectorPropertyControlsInfo,
    })
    expect(actionsDispatched).toMatchInlineSnapshot(`
      Array [
        Object {
          "action": "UPDATE_PROPERTY_CONTROLS_INFO",
          "moduleNamesOrPathsToDelete": Array [],
          "propertyControlsInfo": Object {
            "/src/card": Object {
              "Card": Object {
                "insertOptions": Array [
                  Object {
                    "elementToInsert": Object {
                      "children": Array [],
                      "name": Object {
                        "baseVariable": "Card",
                        "propertyPath": Object {
                          "propertyElements": Array [],
                        },
                      },
                      "props": Array [
                        Object {
                          "comments": Object {
                            "leadingComments": Array [],
                            "trailingComments": Array [],
                          },
                          "key": "title",
                          "type": "JSX_ATTRIBUTES_ENTRY",
                          "value": Object {
                            "comments": Object {
                              "leadingComments": Array [],
                              "trailingComments": Array [],
                            },
                            "type": "ATTRIBUTE_VALUE",
                            "value": "Default",
                          },
                        },
                        Object {
                          "comments": Object {
                            "leadingComments": Array [],
                            "trailingComments": Array [],
                          },
                          "key": "border",
                          "type": "JSX_ATTRIBUTES_ENTRY",
                          "value": Object {
                            "comments": Object {
                              "leadingComments": Array [],
                              "trailingComments": Array [],
                            },
                            "type": "ATTRIBUTE_VALUE",
                            "value": "shiny",
                          },
                        },
                      ],
                    },
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
                "propertyControls": Object {
                  "type": "RIGHT",
                  "value": Object {
                    "border": Object {
                      "type": "RIGHT",
                      "value": Object {
                        "control": "string-input",
                        "label": "Border",
                      },
                    },
                    "title": Object {
                      "type": "RIGHT",
                      "value": Object {
                        "control": "string-input",
                        "label": "Title",
                      },
                    },
                  },
                },
              },
            },
          },
        },
      ]
    `)
  })
})
