import * as EP from '../../../../core/shared/element-path'
import { maybeToArray } from '../../../../core/shared/optional-utils'
import type { ControlDescription } from '../../../custom-code/internal-property-controls'
import type { PropertyValue, VariableInfo } from './variables-in-scope-utils'
import { orderVariablesForRelevance, variableInfoFromValue } from './variables-in-scope-utils'

describe('orderVariablesForRelevance', () => {
  it('should be able to target a given property', () => {
    const variableNamesInScope: Array<VariableInfo> = maybeToArray(
      variableInfoFromValue(
        'style',
        'style',
        { left: 300, position: 'relative' },
        EP.fromString('aaa'),
      ),
    )
    const controlDescription: ControlDescription = {
      control: 'object',
      object: {
        left: {
          control: 'number-input',
        },
      },
    }
    const currentPropertyValue: PropertyValue = {
      type: 'existing',
      value: {
        left: 200,
      },
    }
    const targetPropertyName = 'left'
    const actualResult = orderVariablesForRelevance(
      variableNamesInScope,
      controlDescription,
      currentPropertyValue,
      targetPropertyName,
      'all',
    )
    expect(actualResult).toMatchInlineSnapshot(`
      Array [
        Object {
          "expression": "style",
          "expressionPathPart": "style",
          "insertionCeiling": Object {
            "parts": Array [
              Array [
                "aaa",
              ],
            ],
            "type": "elementpath",
          },
          "matches": false,
          "props": Array [
            Object {
              "expression": "style['left']",
              "expressionPathPart": "left",
              "insertionCeiling": Object {
                "parts": Array [
                  Array [
                    "aaa",
                  ],
                ],
                "type": "elementpath",
              },
              "matches": true,
              "sourceElement": Object {
                "parts": Array [],
                "type": "elementpath",
              },
              "type": "primitive",
              "value": 300,
            },
            Object {
              "expression": "style['position']",
              "expressionPathPart": "position",
              "insertionCeiling": Object {
                "parts": Array [
                  Array [
                    "aaa",
                  ],
                ],
                "type": "elementpath",
              },
              "matches": false,
              "sourceElement": Object {
                "parts": Array [],
                "type": "elementpath",
              },
              "type": "primitive",
              "value": "relative",
            },
          ],
          "sourceElement": Object {
            "parts": Array [],
            "type": "elementpath",
          },
          "type": "object",
          "value": Object {
            "left": 300,
            "position": "relative",
          },
        },
      ]
    `)
  })
  it('handles the case when not targeting a specific property', () => {
    const variableNamesInScope: Array<VariableInfo> = maybeToArray(
      variableInfoFromValue(
        'style',
        'style',
        { left: 300, position: 'relative' },
        EP.fromString('aaa'),
      ),
    )
    const controlDescription: ControlDescription = {
      control: 'object',
      object: {
        left: {
          control: 'number-input',
        },
      },
    }
    const currentPropertyValue: PropertyValue = {
      type: 'existing',
      value: {
        left: 200,
      },
    }
    const targetPropertyName = null
    const actualResult = orderVariablesForRelevance(
      variableNamesInScope,
      controlDescription,
      currentPropertyValue,
      targetPropertyName,
      'all',
    )
    expect(actualResult).toMatchInlineSnapshot(`
      Array [
        Object {
          "expression": "style",
          "expressionPathPart": "style",
          "insertionCeiling": Object {
            "parts": Array [
              Array [
                "aaa",
              ],
            ],
            "type": "elementpath",
          },
          "matches": true,
          "props": Array [
            Object {
              "expression": "style['left']",
              "expressionPathPart": "left",
              "insertionCeiling": Object {
                "parts": Array [
                  Array [
                    "aaa",
                  ],
                ],
                "type": "elementpath",
              },
              "matches": false,
              "sourceElement": Object {
                "parts": Array [],
                "type": "elementpath",
              },
              "type": "primitive",
              "value": 300,
            },
            Object {
              "expression": "style['position']",
              "expressionPathPart": "position",
              "insertionCeiling": Object {
                "parts": Array [
                  Array [
                    "aaa",
                  ],
                ],
                "type": "elementpath",
              },
              "matches": false,
              "sourceElement": Object {
                "parts": Array [],
                "type": "elementpath",
              },
              "type": "primitive",
              "value": "relative",
            },
          ],
          "sourceElement": Object {
            "parts": Array [],
            "type": "elementpath",
          },
          "type": "object",
          "value": Object {
            "left": 300,
            "position": "relative",
          },
        },
      ]
    `)
  })
})
