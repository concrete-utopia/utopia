import { parseBorder } from './css-parser-border'

describe('parseBorder', () => {
  it('parses a CSSBorder', () => {
    const validValue: Array<string> = [
      'solid 1px',
      'solid 1px #000',
      '1px #000 solid',
      '#000 solid 1px',
    ]

    expect(validValue.map((valid) => parseBorder(valid))).toMatchInlineSnapshot(`
      Array [
        Object {
          "type": "RIGHT",
          "value": Object {
            "style": Object {
              "type": "line-style",
              "value": Object {
                "type": "keyword",
                "value": "solid",
              },
            },
            "type": "border",
            "width": Object {
              "type": "line-width",
              "value": Object {
                "unit": "px",
                "value": 1,
              },
            },
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "color": Object {
              "hex": "#000",
              "type": "Hex",
            },
            "style": Object {
              "type": "line-style",
              "value": Object {
                "type": "keyword",
                "value": "solid",
              },
            },
            "type": "border",
            "width": Object {
              "type": "line-width",
              "value": Object {
                "unit": "px",
                "value": 1,
              },
            },
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "color": Object {
              "hex": "#000",
              "type": "Hex",
            },
            "style": Object {
              "type": "line-style",
              "value": Object {
                "type": "keyword",
                "value": "solid",
              },
            },
            "type": "border",
            "width": Object {
              "type": "line-width",
              "value": Object {
                "unit": "px",
                "value": 1,
              },
            },
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "color": Object {
              "hex": "#000",
              "type": "Hex",
            },
            "style": Object {
              "type": "line-style",
              "value": Object {
                "type": "keyword",
                "value": "solid",
              },
            },
            "type": "border",
            "width": Object {
              "type": "line-width",
              "value": Object {
                "unit": "px",
                "value": 1,
              },
            },
          },
        },
      ]
    `)
  })
})
