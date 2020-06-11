import { syntaxParsers } from './css-parser-map'
import { parseLexedColor, getLexerMatches } from './css-parser-utils'
import { eitherToMaybe, isRight } from '../../core/shared/either'
import { parseBorder } from './css-parser-border'

describe('CSS PARSER', () => {
  it('backgroundSize', () => {
    const value = 'auto, contain, 100% 100%, 100px auto, cover, 0 0'
    const parseResults = syntaxParsers["<'background-size'>"](value)
    expect(parseResults).toMatchInlineSnapshot(`
      Object {
        "type": "RIGHT",
        "value": Array [
          Object {
            "size": Object {
              "default": true,
              "value": Object {
                "type": "parsed-curly-brace",
                "value": Array [
                  Object {
                    "type": "keyword",
                    "value": "auto",
                  },
                ],
              },
            },
            "type": "bg-size",
          },
          Object {
            "size": Object {
              "default": true,
              "value": Object {
                "type": "keyword",
                "value": "contain",
              },
            },
            "type": "bg-size",
          },
          Object {
            "size": Object {
              "default": true,
              "value": Object {
                "type": "parsed-curly-brace",
                "value": Array [
                  Object {
                    "unit": "%",
                    "value": 100,
                  },
                  Object {
                    "unit": "%",
                    "value": 100,
                  },
                ],
              },
            },
            "type": "bg-size",
          },
          Object {
            "size": Object {
              "default": true,
              "value": Object {
                "type": "parsed-curly-brace",
                "value": Array [
                  Object {
                    "unit": "px",
                    "value": 100,
                  },
                  Object {
                    "type": "keyword",
                    "value": "auto",
                  },
                ],
              },
            },
            "type": "bg-size",
          },
          Object {
            "size": Object {
              "default": true,
              "value": Object {
                "type": "keyword",
                "value": "cover",
              },
            },
            "type": "bg-size",
          },
          Object {
            "size": Object {
              "default": true,
              "value": Object {
                "type": "parsed-curly-brace",
                "value": Array [
                  Object {
                    "unit": null,
                    "value": 0,
                  },
                  Object {
                    "unit": null,
                    "value": 0,
                  },
                ],
              },
            },
            "type": "bg-size",
          },
        ],
      }
    `)
  })
})

describe('parseLexedColor', () => {
  it('parses a CSSColor from lexed value', () => {
    const validValue: Array<string> = [
      'black',
      '#000',
      '#000F',
      '#000000',
      '#000000FF',
      'rgb(0, 0, 0)',
      'rgba(0, 0, 0, 1)',
      'rgba(0 0 0)',
      'rgba(0 0 0 / 1)',
      'rgba(0 0 0 / 100%)',
      'hsl(0, 0%, 0%)',
      'hsl(0, 0%, 0%, 100%)',
      'hsl(0deg, 0%, 0%)',
    ]

    expect(
      validValue.map((valid) => {
        const lexer = getLexerMatches('color', valid)
        if (isRight(lexer)) {
          return parseLexedColor(lexer.value[0])
        } else {
          fail()
        }
      }),
    ).toMatchInlineSnapshot(`
      Array [
        Object {
          "type": "RIGHT",
          "value": Object {
            "keyword": "black",
            "type": "Keyword",
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "hex": "#000",
            "type": "Hex",
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "hex": "#000F",
            "type": "Hex",
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "hex": "#000000",
            "type": "Hex",
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "hex": "#000000FF",
            "type": "Hex",
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "a": 1,
            "b": 0,
            "g": 0,
            "percentageAlpha": false,
            "percentagesUsed": false,
            "r": 0,
            "type": "RGB",
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "a": 1,
            "b": 0,
            "g": 0,
            "percentageAlpha": false,
            "percentagesUsed": false,
            "r": 0,
            "type": "RGB",
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "a": 1,
            "b": 0,
            "g": 0,
            "percentageAlpha": false,
            "percentagesUsed": false,
            "r": 0,
            "type": "RGB",
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "a": 1,
            "b": 0,
            "g": 0,
            "percentageAlpha": false,
            "percentagesUsed": false,
            "r": 0,
            "type": "RGB",
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "a": 1,
            "b": 0,
            "g": 0,
            "percentageAlpha": true,
            "percentagesUsed": false,
            "r": 0,
            "type": "RGB",
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "a": 1,
            "h": 0,
            "l": 0,
            "percentageAlpha": false,
            "s": 0,
            "type": "HSL",
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "a": 1,
            "h": 0,
            "l": 0,
            "percentageAlpha": true,
            "s": 0,
            "type": "HSL",
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "a": 1,
            "h": 0,
            "l": 0,
            "percentageAlpha": false,
            "s": 0,
            "type": "HSL",
          },
        },
      ]
    `)
  })
})

describe('parseBorder', () => {
  it('parses a CSSBorder', () => {
    const validValue: Array<string> = [
      'thin solid black',
      'thick solid #000',
      'medium solid #000F',
      '10px none #000000',
      '10px hidden #000000FF',
      '10px dotted rgb(0, 0, 0)',
      '10px dashed rgba(0, 0, 0, 1)',
      '10px solid rgba(0 0 0)',
      '10px double rgba(0 0 0 / 1)',
      '10px groove rgba(0 0 0 / 100%)',
      '10px ridge hsl(0, 0%, 0%)',
      '10px inset hsl(0, 0%, 0%, 100%)',
      '10px outset hsl(0deg, 0%, 0%)',
    ]

    expect(validValue.map((valid) => syntaxParsers["<'border'>"](valid))).toMatchInlineSnapshot(`
      Array [
        Object {
          "type": "RIGHT",
          "value": Object {
            "color": Object {
              "keyword": "black",
              "type": "Keyword",
            },
            "style": Object {
              "style": Object {
                "type": "keyword",
                "value": "solid",
              },
              "type": "line-style",
            },
            "width": Object {
              "type": "line-width",
              "width": Object {
                "type": "keyword",
                "value": "thin",
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
              "style": Object {
                "type": "keyword",
                "value": "solid",
              },
              "type": "line-style",
            },
            "width": Object {
              "type": "line-width",
              "width": Object {
                "type": "keyword",
                "value": "thick",
              },
            },
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "color": Object {
              "hex": "#000F",
              "type": "Hex",
            },
            "style": Object {
              "style": Object {
                "type": "keyword",
                "value": "solid",
              },
              "type": "line-style",
            },
            "width": Object {
              "type": "line-width",
              "width": Object {
                "type": "keyword",
                "value": "medium",
              },
            },
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "color": Object {
              "hex": "#000000",
              "type": "Hex",
            },
            "style": Object {
              "style": Object {
                "type": "keyword",
                "value": "none",
              },
              "type": "line-style",
            },
            "width": Object {
              "type": "line-width",
              "width": Object {
                "unit": "px",
                "value": 10,
              },
            },
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "color": Object {
              "hex": "#000000FF",
              "type": "Hex",
            },
            "style": Object {
              "style": Object {
                "type": "keyword",
                "value": "hidden",
              },
              "type": "line-style",
            },
            "width": Object {
              "type": "line-width",
              "width": Object {
                "unit": "px",
                "value": 10,
              },
            },
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "color": Object {
              "a": 1,
              "b": 0,
              "g": 0,
              "percentageAlpha": false,
              "percentagesUsed": false,
              "r": 0,
              "type": "RGB",
            },
            "style": Object {
              "style": Object {
                "type": "keyword",
                "value": "dotted",
              },
              "type": "line-style",
            },
            "width": Object {
              "type": "line-width",
              "width": Object {
                "unit": "px",
                "value": 10,
              },
            },
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "color": Object {
              "a": 1,
              "b": 0,
              "g": 0,
              "percentageAlpha": false,
              "percentagesUsed": false,
              "r": 0,
              "type": "RGB",
            },
            "style": Object {
              "style": Object {
                "type": "keyword",
                "value": "dashed",
              },
              "type": "line-style",
            },
            "width": Object {
              "type": "line-width",
              "width": Object {
                "unit": "px",
                "value": 10,
              },
            },
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "color": Object {
              "a": 1,
              "b": 0,
              "g": 0,
              "percentageAlpha": false,
              "percentagesUsed": false,
              "r": 0,
              "type": "RGB",
            },
            "style": Object {
              "style": Object {
                "type": "keyword",
                "value": "solid",
              },
              "type": "line-style",
            },
            "width": Object {
              "type": "line-width",
              "width": Object {
                "unit": "px",
                "value": 10,
              },
            },
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "color": Object {
              "a": 1,
              "b": 0,
              "g": 0,
              "percentageAlpha": false,
              "percentagesUsed": false,
              "r": 0,
              "type": "RGB",
            },
            "style": Object {
              "style": Object {
                "type": "keyword",
                "value": "double",
              },
              "type": "line-style",
            },
            "width": Object {
              "type": "line-width",
              "width": Object {
                "unit": "px",
                "value": 10,
              },
            },
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "color": Object {
              "a": 1,
              "b": 0,
              "g": 0,
              "percentageAlpha": true,
              "percentagesUsed": false,
              "r": 0,
              "type": "RGB",
            },
            "style": Object {
              "style": Object {
                "type": "keyword",
                "value": "groove",
              },
              "type": "line-style",
            },
            "width": Object {
              "type": "line-width",
              "width": Object {
                "unit": "px",
                "value": 10,
              },
            },
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "color": Object {
              "a": 1,
              "h": 0,
              "l": 0,
              "percentageAlpha": false,
              "s": 0,
              "type": "HSL",
            },
            "style": Object {
              "style": Object {
                "type": "keyword",
                "value": "ridge",
              },
              "type": "line-style",
            },
            "width": Object {
              "type": "line-width",
              "width": Object {
                "unit": "px",
                "value": 10,
              },
            },
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "color": Object {
              "a": 1,
              "h": 0,
              "l": 0,
              "percentageAlpha": true,
              "s": 0,
              "type": "HSL",
            },
            "style": Object {
              "style": Object {
                "type": "keyword",
                "value": "inset",
              },
              "type": "line-style",
            },
            "width": Object {
              "type": "line-width",
              "width": Object {
                "unit": "px",
                "value": 10,
              },
            },
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "color": Object {
              "a": 1,
              "h": 0,
              "l": 0,
              "percentageAlpha": false,
              "s": 0,
              "type": "HSL",
            },
            "style": Object {
              "style": Object {
                "type": "keyword",
                "value": "outset",
              },
              "type": "line-style",
            },
            "width": Object {
              "type": "line-width",
              "width": Object {
                "unit": "px",
                "value": 10,
              },
            },
          },
        },
      ]
    `)
  })
})
