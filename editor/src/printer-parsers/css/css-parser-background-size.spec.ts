import { syntaxParsers } from './css-parser-map'

describe('backgroundSize', () => {
  it("parses the <'background-size'> property with comments", () => {
    const value =
      'auto, /*auto*/ /*auto auto*/ /*100px 100px*/ contain, 100% 100%, 100px auto, /* auto */ cover, 0 0 /*cover*/'
    const parseResults = syntaxParsers["<'background-size'>"](value)
    expect(parseResults).toMatchInlineSnapshot(`
      Object {
        "type": "RIGHT",
        "value": Array [
          Object {
            "enabled": true,
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
            "enabled": false,
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
            "enabled": false,
            "size": Object {
              "default": false,
              "value": Object {
                "type": "parsed-curly-brace",
                "value": Array [
                  Object {
                    "type": "keyword",
                    "value": "auto",
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
            "enabled": false,
            "size": Object {
              "default": false,
              "value": Object {
                "type": "parsed-curly-brace",
                "value": Array [
                  Object {
                    "unit": "px",
                    "value": 100,
                  },
                  Object {
                    "unit": "px",
                    "value": 100,
                  },
                ],
              },
            },
            "type": "bg-size",
          },
          Object {
            "enabled": true,
            "size": Object {
              "default": false,
              "value": Object {
                "type": "keyword",
                "value": "contain",
              },
            },
            "type": "bg-size",
          },
          Object {
            "enabled": true,
            "size": Object {
              "default": false,
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
            "enabled": true,
            "size": Object {
              "default": false,
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
            "enabled": false,
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
            "enabled": true,
            "size": Object {
              "default": false,
              "value": Object {
                "type": "keyword",
                "value": "cover",
              },
            },
            "type": "bg-size",
          },
          Object {
            "enabled": true,
            "size": Object {
              "default": false,
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
          Object {
            "enabled": false,
            "size": Object {
              "default": false,
              "value": Object {
                "type": "keyword",
                "value": "cover",
              },
            },
            "type": "bg-size",
          },
        ],
      }
    `)
  })
})
