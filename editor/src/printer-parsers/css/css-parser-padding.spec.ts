import { clearExpressionUniqueIDs } from '../../core/shared/element-template'
import { syntaxParsers } from './css-parser-map'
import { printPaddingAsAttributeValue } from './css-parser-padding'

describe('parse padding', () => {
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
  it("parses shorthand(2-value-syntax) <'padding'> property, number values", () => {
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
  it("parses shorthand(2-value-syntax) <'padding'> property, percentage values", () => {
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
  it("parses shorthand(3-value-syntax) <'padding'> property", () => {
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

describe('print padding', () => {
  it('4 different padding values', () => {
    const cssPadding = {
      paddingTop: {
        value: 4,
        unit: 'px' as const,
      },
      paddingRight: {
        value: 24,
        unit: 'px' as const,
      },
      paddingBottom: {
        value: 12,
        unit: 'px' as const,
      },
      paddingLeft: {
        value: 8,
        unit: 'px' as const,
      },
    }
    const printResult = clearExpressionUniqueIDs(printPaddingAsAttributeValue(cssPadding))
    expect(printResult).toMatchInlineSnapshot(`
      Object {
        "comments": Object {
          "leadingComments": Array [],
          "trailingComments": Array [],
        },
        "type": "ATTRIBUTE_VALUE",
        "uid": "",
        "value": "4px 24px 12px 8px",
      }
    `)
  })
  it('4 different padding values, missing unit', () => {
    const cssPadding = {
      paddingTop: {
        value: 4,
        unit: null,
      },
      paddingRight: {
        value: 24,
        unit: null,
      },
      paddingBottom: {
        value: 12,
        unit: null,
      },
      paddingLeft: {
        value: 8,
        unit: null,
      },
    }
    const printResult = clearExpressionUniqueIDs(printPaddingAsAttributeValue(cssPadding))
    expect(printResult).toMatchInlineSnapshot(`
      Object {
        "comments": Object {
          "leadingComments": Array [],
          "trailingComments": Array [],
        },
        "type": "ATTRIBUTE_VALUE",
        "uid": "",
        "value": "4px 24px 12px 8px",
      }
    `)
  })
  it('2-2 sides matching, 2-value-syntax', () => {
    const cssPadding = {
      paddingTop: {
        value: 4,
        unit: 'px' as const,
      },
      paddingRight: {
        value: 24,
        unit: 'px' as const,
      },
      paddingBottom: {
        value: 4,
        unit: 'px' as const,
      },
      paddingLeft: {
        value: 24,
        unit: 'px' as const,
      },
    }
    const printResult = clearExpressionUniqueIDs(printPaddingAsAttributeValue(cssPadding))
    expect(printResult).toMatchInlineSnapshot(`
      Object {
        "comments": Object {
          "leadingComments": Array [],
          "trailingComments": Array [],
        },
        "type": "ATTRIBUTE_VALUE",
        "uid": "",
        "value": "4px 24px",
      }
    `)
  })
  it('2-2 sides matching percent values, 2-value-syntax', () => {
    const cssPadding = {
      paddingTop: {
        value: 4,
        unit: '%' as const,
      },
      paddingRight: {
        value: 24,
        unit: '%' as const,
      },
      paddingBottom: {
        value: 4,
        unit: '%' as const,
      },
      paddingLeft: {
        value: 24,
        unit: '%' as const,
      },
    }
    const printResult = clearExpressionUniqueIDs(printPaddingAsAttributeValue(cssPadding))
    expect(printResult).toMatchInlineSnapshot(`
      Object {
        "comments": Object {
          "leadingComments": Array [],
          "trailingComments": Array [],
        },
        "type": "ATTRIBUTE_VALUE",
        "uid": "",
        "value": "4% 24%",
      }
    `)
  })
  it('left and right is the same, BUT we DO NOT use the 3-value-syntax', () => {
    const cssPadding = {
      paddingTop: {
        value: 4,
        unit: null,
      },
      paddingRight: {
        value: 8,
        unit: null,
      },
      paddingBottom: {
        value: 12,
        unit: null,
      },
      paddingLeft: {
        value: 8,
        unit: null,
      },
    }
    const printResult = clearExpressionUniqueIDs(printPaddingAsAttributeValue(cssPadding))
    expect(printResult).toMatchInlineSnapshot(`
      Object {
        "comments": Object {
          "leadingComments": Array [],
          "trailingComments": Array [],
        },
        "type": "ATTRIBUTE_VALUE",
        "uid": "",
        "value": "4px 8px 12px 8px",
      }
    `)
  })
  it('all padding values are the same, 1-value-syntax', () => {
    const cssPadding = {
      paddingTop: {
        value: 6,
        unit: null,
      },
      paddingRight: {
        value: 6,
        unit: null,
      },
      paddingBottom: {
        value: 6,
        unit: null,
      },
      paddingLeft: {
        value: 6,
        unit: null,
      },
    }
    const printResult = clearExpressionUniqueIDs(printPaddingAsAttributeValue(cssPadding))
    expect(printResult).toMatchInlineSnapshot(`
      Object {
        "comments": Object {
          "leadingComments": Array [],
          "trailingComments": Array [],
        },
        "type": "ATTRIBUTE_VALUE",
        "uid": "",
        "value": 6,
      }
    `)
  })
  it('all padding values are the same percent values, 1-value-syntax', () => {
    const cssPadding = {
      paddingTop: {
        value: 6,
        unit: '%' as const,
      },
      paddingRight: {
        value: 6,
        unit: '%' as const,
      },
      paddingBottom: {
        value: 6,
        unit: '%' as const,
      },
      paddingLeft: {
        value: 6,
        unit: '%' as const,
      },
    }
    const printResult = clearExpressionUniqueIDs(printPaddingAsAttributeValue(cssPadding))
    expect(printResult).toMatchInlineSnapshot(`
      Object {
        "comments": Object {
          "leadingComments": Array [],
          "trailingComments": Array [],
        },
        "type": "ATTRIBUTE_VALUE",
        "uid": "",
        "value": "6%",
      }
    `)
  })
})
