import { syntaxParsers } from './css-parser-map'

describe('padding', () => {
  it("parses a simple number <'padding'> property", () => {
    const value = 155
    const parseResults = syntaxParsers['<padding>'](value)
    expect(parseResults).toMatchInlineSnapshot(`
      Object {
        "type": "RIGHT",
        "value": Object {
          "paddingBottom": Object {
            "unit": "px",
            "value": 155,
          },
          "paddingLeft": Object {
            "unit": "px",
            "value": 155,
          },
          "paddingRight": Object {
            "unit": "px",
            "value": 155,
          },
          "paddingTop": Object {
            "unit": "px",
            "value": 155,
          },
        },
      }
    `)
  })
  it("parses a <'padding'> property, percentage value", () => {
    const value = '10%'
    const parseResults = syntaxParsers['<padding>'](value)
    expect(parseResults).toMatchInlineSnapshot(`
      Object {
        "type": "RIGHT",
        "value": Object {
          "paddingBottom": Object {
            "unit": "%",
            "value": 10,
          },
          "paddingLeft": Object {
            "unit": "%",
            "value": 10,
          },
          "paddingRight": Object {
            "unit": "%",
            "value": 10,
          },
          "paddingTop": Object {
            "unit": "%",
            "value": 10,
          },
        },
      }
    `)
  })
  it("parses shorthand(2) <'padding'> property, number values", () => {
    const value = '3px 4px'
    const parseResults = syntaxParsers['<padding>'](value)
    expect(parseResults).toMatchInlineSnapshot(`
      Object {
        "type": "RIGHT",
        "value": Object {
          "paddingBottom": Object {
            "unit": "px",
            "value": 3,
          },
          "paddingLeft": Object {
            "unit": "px",
            "value": 4,
          },
          "paddingRight": Object {
            "unit": "px",
            "value": 4,
          },
          "paddingTop": Object {
            "unit": "px",
            "value": 3,
          },
        },
      }
    `)
  })
  it("parses shorthand(2) <'padding'> property, percentage values", () => {
    const value = '3% 4%'
    const parseResults = syntaxParsers['<padding>'](value)
    expect(parseResults).toMatchInlineSnapshot(`
      Object {
        "type": "RIGHT",
        "value": Object {
          "paddingBottom": Object {
            "unit": "%",
            "value": 3,
          },
          "paddingLeft": Object {
            "unit": "%",
            "value": 4,
          },
          "paddingRight": Object {
            "unit": "%",
            "value": 4,
          },
          "paddingTop": Object {
            "unit": "%",
            "value": 3,
          },
        },
      }
    `)
  })
  it("parses shorthand(3) <'padding'> property", () => {
    const value = '2px 4px 8px'
    const parseResults = syntaxParsers['<padding>'](value)
    expect(parseResults).toMatchInlineSnapshot(`
      Object {
        "type": "RIGHT",
        "value": Object {
          "paddingBottom": Object {
            "unit": "px",
            "value": 8,
          },
          "paddingLeft": Object {
            "unit": "px",
            "value": 4,
          },
          "paddingRight": Object {
            "unit": "px",
            "value": 4,
          },
          "paddingTop": Object {
            "unit": "px",
            "value": 2,
          },
        },
      }
    `)
  })
  it("parses the full <'padding'> property, number values", () => {
    const value = '4px 6px 12px 8px'
    const parseResults = syntaxParsers['<padding>'](value)
    expect(parseResults).toMatchInlineSnapshot(`
      Object {
        "type": "RIGHT",
        "value": Object {
          "paddingBottom": Object {
            "unit": "px",
            "value": 12,
          },
          "paddingLeft": Object {
            "unit": "px",
            "value": 8,
          },
          "paddingRight": Object {
            "unit": "px",
            "value": 6,
          },
          "paddingTop": Object {
            "unit": "px",
            "value": 4,
          },
        },
      }
    `)
  })
  it("parses the full <'padding'> property, percentage values", () => {
    const value = '4% 6% 12% 8%'
    const parseResults = syntaxParsers['<padding>'](value)
    expect(parseResults).toMatchInlineSnapshot(`
      Object {
        "type": "RIGHT",
        "value": Object {
          "paddingBottom": Object {
            "unit": "%",
            "value": 12,
          },
          "paddingLeft": Object {
            "unit": "%",
            "value": 8,
          },
          "paddingRight": Object {
            "unit": "%",
            "value": 6,
          },
          "paddingTop": Object {
            "unit": "%",
            "value": 4,
          },
        },
      }
    `)
  })
})
