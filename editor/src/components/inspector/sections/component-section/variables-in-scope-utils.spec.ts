import React from 'react'
import { stripNulls } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import { maybeToArray } from '../../../../core/shared/optional-utils'
import { emptySet } from '../../../../core/shared/set-utils'
import type { ControlDescription } from '../../../custom-code/internal-property-controls'
import type { PropertyValue, VariableInfo } from './variables-in-scope-utils'
import { matchForPropertyValue, variableInfoFromValue } from './variables-in-scope-utils'

describe('matchForPropertyValue', () => {
  it('should be able to target a given property', () => {
    const variableNamesInScope: Array<VariableInfo> = maybeToArray(
      variableInfoFromValue(
        'style',
        'style',
        { left: 300, position: 'relative' },
        EP.fromString('aaa'),
        emptySet(),
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
    const actualResult = matchForPropertyValue(
      controlDescription,
      currentPropertyValue,
      targetPropertyName,
    )(variableNamesInScope)
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
          "matches": "child-matches",
          "props": Array [
            Object {
              "expression": "style.left",
              "expressionPathPart": "left",
              "insertionCeiling": Object {
                "parts": Array [
                  Array [
                    "aaa",
                  ],
                ],
                "type": "elementpath",
              },
              "matches": "matches",
              "type": "primitive",
              "value": 300,
            },
            Object {
              "expression": "style.position",
              "expressionPathPart": "position",
              "insertionCeiling": Object {
                "parts": Array [
                  Array [
                    "aaa",
                  ],
                ],
                "type": "elementpath",
              },
              "matches": "does-not-match",
              "type": "primitive",
              "value": "relative",
            },
          ],
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
        emptySet(),
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
    const actualResult = matchForPropertyValue(
      controlDescription,
      currentPropertyValue,
      targetPropertyName,
    )(variableNamesInScope)
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
          "matches": "matches",
          "props": Array [
            Object {
              "expression": "style.left",
              "expressionPathPart": "left",
              "insertionCeiling": Object {
                "parts": Array [
                  Array [
                    "aaa",
                  ],
                ],
                "type": "elementpath",
              },
              "matches": "does-not-match",
              "type": "primitive",
              "value": 300,
            },
            Object {
              "expression": "style.position",
              "expressionPathPart": "position",
              "insertionCeiling": Object {
                "parts": Array [
                  Array [
                    "aaa",
                  ],
                ],
                "type": "elementpath",
              },
              "matches": "does-not-match",
              "type": "primitive",
              "value": "relative",
            },
          ],
          "type": "object",
          "value": Object {
            "left": 300,
            "position": "relative",
          },
        },
      ]
    `)
  })

  it('matches strings and numbers too if the value of the property is a jsx element', () => {
    const variableNamesInScope: Array<VariableInfo> = stripNulls([
      variableInfoFromValue(
        'data',
        'data',
        {
          size: 300,
          description: 'relative',
          open: false,
          label: React.createElement('div'),
          likes: ['Alice', 'Bob'],
          quote: {
            author: 'E.Poe',
            text: 'I became insane, with long intervals of horrible sanity.',
          },
        },
        EP.fromString('aaa'),
        emptySet(),
      ),
    ])

    const currentPropertyValue: PropertyValue = {
      type: 'existing',
      value: React.createElement('div'),
    }
    const targetPropertyName = 'label'
    const actualResult = matchForPropertyValue(
      null,
      currentPropertyValue,
      targetPropertyName,
    )(variableNamesInScope)

    const matching = actualResult
      .flatMap((r) => [
        r,
        ...(r.type === 'array' ? r.elements : r.type === 'object' ? r.props : []),
      ])
      .filter((r) => r.matches === 'matches')
      .map((r) => r.expression)

    expect(matching).toMatchInlineSnapshot(`
      Array [
        "data.size",
        "data.description",
        "data.label",
      ]
    `)
  })
})
