import { clearModifiableAttributeUniqueIDs } from '../../../core/shared/jsx-attributes'
import type { Either } from '../../../core/shared/either'
import { isLeft, isRight, right } from '../../../core/shared/either'
import {
  emptyComments,
  jsExpressionFunctionCall,
  jsxAttributeNestedObjectSimple,
  jsxAttributesFromMap,
  jsExpressionValue,
  jsxTestElement,
  clearExpressionUniqueIDs,
  clearJSXElementChildUniqueIDs,
} from '../../../core/shared/element-template'
import * as PP from '../../../core/shared/property-path'
import type {
  CSSBackground,
  CSSBackgroundSize,
  CSSBorderRadius,
  CSSColor,
  CSSTextShadows,
  CSSTransforms,
} from './css-utils'
import {
  cssAngle,
  cssBGSize,
  cssColor,
  cssColorHSL,
  cssColorToChromaColor,
  cssDefault,
  cssKeyword,
  cssNumber,
  cssPixelLength,
  cssPixelLengthZero,
  cssTransformRotate,
  cssTransformRotateX,
  cssTransformRotateY,
  cssTransformRotateZ,
  cssTransformScale,
  cssTransformScaleX,
  cssTransformScaleY,
  cssTransformScaleZ,
  cssTransformSkew,
  cssTransformSkewX,
  cssTransformSkewY,
  cssTransformTranslate,
  cssTransformTranslateX,
  cssTransformTranslateY,
  cssTransformTranslateZ,
  cssUnitlessLength,
  defaultBGSize,
  defaultCSSGradientStops,
  defaultCSSRadialGradientSize,
  defaultCSSRadialOrConicGradientCenter,
  disabledFunctionName,
  gridCSSKeyword,
  gridCSSMinmax,
  gridCSSNumber,
  gridCSSRepeat,
  parseBackgroundColor,
  parseBackgroundImage,
  parseBorderRadius,
  parseBoxShadow,
  parseColor,
  parseConicGradient,
  parseCSSURLFunction,
  parsedCurlyBrace,
  parseLinearGradient,
  parseRadialGradient,
  parseTextShadow,
  parseTransform,
  printBackgroundImage,
  printBackgroundSize,
  printGridDimension,
  RegExpLibrary,
  stringifyGridDimension,
  toggleSimple,
  toggleStylePropPath,
} from './css-utils'

describe('toggleStyleProp', () => {
  const simpleToggleProp = toggleStylePropPath(PP.create('style', 'backgroundColor'), toggleSimple)

  it('disables simple value', () => {
    const element = jsxTestElement(
      'View',
      jsxAttributesFromMap({
        style: jsExpressionValue(
          {
            backgroundColor: 'red',
          },
          emptyComments,
        ),
      }),
      [],
    )

    const expectedElement = clearJSXElementChildUniqueIDs(
      jsxTestElement(
        'View',
        jsxAttributesFromMap({
          style: jsxAttributeNestedObjectSimple(
            jsxAttributesFromMap({
              backgroundColor: jsExpressionFunctionCall(disabledFunctionName, [
                jsExpressionValue('red', emptyComments),
              ]),
            }),
            emptyComments,
          ),
        }),
        [],
      ),
    )
    const toggledElement = clearJSXElementChildUniqueIDs(simpleToggleProp(element))
    expect(toggledElement).toEqual(expectedElement)
  })

  it('enables simple value', () => {
    const element = jsxTestElement(
      'View',
      jsxAttributesFromMap({
        style: jsxAttributeNestedObjectSimple(
          jsxAttributesFromMap({
            backgroundColor: jsExpressionFunctionCall(disabledFunctionName, [
              jsExpressionValue('red', emptyComments),
            ]),
          }),
          emptyComments,
        ),
      }),
      [],
    )

    const expectedElement = clearJSXElementChildUniqueIDs(
      jsxTestElement(
        'View',
        jsxAttributesFromMap({
          style: jsxAttributeNestedObjectSimple(
            jsxAttributesFromMap({
              backgroundColor: jsExpressionValue('red', emptyComments),
            }),
            emptyComments,
          ),
        }),
        [],
      ),
    )
    const toggledElement = clearJSXElementChildUniqueIDs(simpleToggleProp(element))
    expect(toggledElement).toEqual(expectedElement)
  })

  it('works with nested objects', () => {
    const element = jsxTestElement(
      'View',
      jsxAttributesFromMap({
        style: jsxAttributeNestedObjectSimple(
          jsxAttributesFromMap({
            backgroundColor: jsExpressionValue('red', emptyComments),
          }),
          emptyComments,
        ),
      }),
      [],
    )

    const expectedElement = clearJSXElementChildUniqueIDs(
      jsxTestElement(
        'View',
        jsxAttributesFromMap({
          style: jsxAttributeNestedObjectSimple(
            jsxAttributesFromMap({
              backgroundColor: jsExpressionFunctionCall(disabledFunctionName, [
                jsExpressionValue('red', emptyComments),
              ]),
            }),
            emptyComments,
          ),
        }),
        [],
      ),
    )
    const toggledElement = clearJSXElementChildUniqueIDs(simpleToggleProp(element))
    expect(toggledElement).toEqual(expectedElement)
  })
})

describe('toggleSimple', () => {
  it('disables the attribute', () => {
    const attribute = jsExpressionValue('colorValue', emptyComments)
    const expectedAttribute = jsExpressionFunctionCall(disabledFunctionName, [
      jsExpressionValue('colorValue', emptyComments),
    ])
    const result = toggleSimple(attribute)
    expect(clearModifiableAttributeUniqueIDs(result)).toEqual(
      clearModifiableAttributeUniqueIDs(expectedAttribute),
    )
  })

  it('enables the attribute', () => {
    const attribute = jsExpressionFunctionCall(disabledFunctionName, [
      jsExpressionValue('colorValue', emptyComments),
    ])
    const expectedAttribute = jsExpressionValue('colorValue', emptyComments)
    const result = toggleSimple(attribute)
    expect(clearModifiableAttributeUniqueIDs(result)).toEqual(
      clearModifiableAttributeUniqueIDs(expectedAttribute),
    )
  })

  it('disables a nested object attribute', () => {
    const attribute = jsxAttributeNestedObjectSimple(
      jsxAttributesFromMap({
        aParameter: jsExpressionFunctionCall('aHelperFunction', [
          jsExpressionValue('hello', emptyComments),
        ]),
      }),
      emptyComments,
    )
    const expectedAttribute = jsExpressionFunctionCall(disabledFunctionName, [
      jsxAttributeNestedObjectSimple(
        jsxAttributesFromMap({
          aParameter: jsExpressionFunctionCall('aHelperFunction', [
            jsExpressionValue('hello', emptyComments),
          ]),
        }),
        emptyComments,
      ),
    ])
    const result = toggleSimple(attribute)
    expect(clearModifiableAttributeUniqueIDs(result)).toEqual(
      clearModifiableAttributeUniqueIDs(expectedAttribute),
    )
  })

  it('enables a nested object attribute', () => {
    const attribute = jsExpressionFunctionCall(disabledFunctionName, [
      jsxAttributeNestedObjectSimple(
        jsxAttributesFromMap({
          aParameter: jsExpressionFunctionCall('aHelperFunction', [
            jsExpressionValue('hello', emptyComments),
          ]),
        }),
        emptyComments,
      ),
    ])
    const expectedAttribute = jsxAttributeNestedObjectSimple(
      jsxAttributesFromMap({
        aParameter: jsExpressionFunctionCall('aHelperFunction', [
          jsExpressionValue('hello', emptyComments),
        ]),
      }),
      emptyComments,
    )
    const result = toggleSimple(attribute)
    expect(clearModifiableAttributeUniqueIDs(result)).toEqual(
      clearModifiableAttributeUniqueIDs(expectedAttribute),
    )
  })
})

function testRegex(regex: RegExp, validStrings: Array<string>, invalidStrings: Array<string>) {
  for (const validString of validStrings) {
    expect(validString.match(regex)![0]).toEqual(validString)
  }
  for (const invalidString of invalidStrings) {
    expect(invalidString.match(regex)).toBeNull()
  }
}

function testRegexMatches(
  regex: RegExp,
  testStringsAndMatches: Array<{
    testString: string
    matchGroups: Array<null | string>
  }>,
  invalidStrings: Array<string>,
) {
  for (const stringAndMatch of testStringsAndMatches) {
    for (let i = 0; i < stringAndMatch.matchGroups.length; i++) {
      const matchGroup = stringAndMatch.matchGroups[i]
      if (matchGroup != null) {
        if (stringAndMatch.testString.match(regex)![i] !== matchGroup) {
          throw new Error(
            `${stringAndMatch.testString}: ${
              stringAndMatch.testString.match(regex)![i]
            } doesn't equal ${matchGroup}`,
          )
        }
      }
    }
  }
  for (const invalidString of invalidStrings) {
    expect(invalidString.match(regex)).toBeNull()
  }
}

describe('GradientRegExpLibrary', () => {
  it('regex parses a comma', () => {
    testRegex(RegExpLibrary.comma, [',', ', ', ' ,', '  ,  '], ['', '.'])
  })
  it('regex parses a rgba? color’s components', () => {
    testRegexMatches(
      RegExpLibrary.colorRGBaComponents,
      [
        {
          testString: 'rgba(51, 170, 51, .1)',
          matchGroups: ['rgba(51, 170, 51, .1)', '51', '170', '51', '.1'],
        },
        {
          testString: 'rgba(51, 170, 51, 1)',
          matchGroups: ['rgba(51, 170, 51, 1)', '51', '170', '51', '1'],
        },
        {
          testString: 'rgb(255,0,153)',
          matchGroups: ['rgb(255,0,153)', '255', '0', '153'],
        },
        {
          testString: 'rgb(255, 0, 153)',
          matchGroups: ['rgb(255, 0, 153)', '255', '0', '153'],
        },
        {
          testString: 'rgb(255, 0, 153.0)',
          matchGroups: ['rgb(255, 0, 153.0)', '255', '0', '153.0'],
        },
        {
          testString: 'rgb(255, 0, 153, 100%)',
          matchGroups: ['rgb(255, 0, 153, 100%)', '255', '0', '153', '100%'],
        },
        {
          testString: 'rgba(51 170 51 / 0.4)',
          matchGroups: [
            'rgba(51 170 51 / 0.4)',
            ...new Array(4).fill(undefined),
            '51',
            '170',
            '51',
            '0.4',
          ],
        },
        {
          testString: 'rgba(51 170 51 / 40%)',
          matchGroups: [
            'rgba(51 170 51 / 40%)',
            ...new Array(4).fill(undefined),
            '51',
            '170',
            '51',
            '40%',
          ],
        },
        {
          testString: 'rgb(255 0 153)',
          matchGroups: ['rgb(255 0 153)', ...new Array(4).fill(undefined), '255', '0', '153'],
        },
        {
          testString: 'rgb(100%,0%,60%)',
          matchGroups: ['rgb(100%,0%,60%)', ...new Array(8).fill(undefined), '100%', '0%', '60%'],
        },
        {
          testString: 'rgb(100%, 0%, 60%)',
          matchGroups: ['rgb(100%, 0%, 60%)', ...new Array(8).fill(undefined), '100%', '0%', '60%'],
        },
        {
          testString: 'rgba(100%,0%,60%,100%)',
          matchGroups: [
            'rgba(100%,0%,60%,100%)',
            ...new Array(8).fill(undefined),
            '100%',
            '0%',
            '60%',
            '100%',
          ],
        },
      ],
      [
        'rgb(100%, 0, 60%)',
        'rgba(0, 0, 0,)',
        'rgb(10.00.0, 0, 0)',
        'rgba(0., 0, 0)',
        'rgba(a, 0, 0)',
        'rgba(0, 0, 0_)',
      ],
    )
  })
})

function testBackgroundLayers(
  parseFunction: (value: string) => Either<string, CSSBackground>,
  validStrings: Array<string>,
  invalidStrings: Array<string>,
) {
  for (const validString of validStrings) {
    const parsed = parseFunction(validString)
    if (isLeft(parsed)) {
      throw new Error(`${parsed.value}: ${validString}`)
    }
  }
  for (const invalidString of invalidStrings) {
    const parsed = parseFunction(invalidString)
    if (isRight(parsed)) {
      throw new Error(`${parsed.value}: ${invalidString}`)
    }
  }
}

describe('parseLinearGradient', () => {
  it('parses linear gradients', () => {
    const valid = [
      'linear-gradient(#000 0%, #fff 100%)',
      'linear-gradient(#000 0%, #888 50%, #fff 100%)',
      'linear-gradient(#000 0%, #444 25%, #888 50%, #bbb 75%, #fff 100%)',
      'linear-gradient(0deg, #000 0%, #fff 100%)',
      'linear-gradient(45.5deg, #000 0%, #fff 100%)',
      'linear-gradient(rgb(0, 0, 0, 1) 0%, rgb(255, 255, 255, 1) 100%)',
      'linear-gradient(hsl(0, 100%, 0%, 1) 0%, hsl(0, 100%, 100%, 1) 100%)',
      'linear-gradient(rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%)',
      'linear-gradient(0deg, rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%)',
      'linear-gradient(0deg, rgb(0 0 0) 0%, rgb(255 255 255) 100%)',
      'linear-gradient(0deg, rgb(0 0 0 / 1) 0%, rgb(255 255 255 / 1) 100%)',
      'linear-gradient(0deg, rgb(0, 0, 0, 1) 0%, rgb(255, 255, 255, 1) 100%)',
      'linear-gradient(1turn, rgb(0, 0, 0, 1) 0%, rgb(255, 255, 255, 1) 100%)',
      'linear-gradient(10.0rad, rgb(0, 0, 0, 1) 0%, rgb(255, 255, 255, 1) 100%)',
      'linear-gradient(10.0grad, rgb(0, 0, 0, 1) 0%, rgb(255, 255, 255, 1) 100%)',
    ]
    const invalid = [
      'linear-gradient(#000)',
      'linear-gradient(#000, #fff)',
      'linear-gradient(#000 #fff)',
      'linear-gradient(0%,100%)',
      'linear-gradient(#000 0% #fff 100%)',
      'linear-gradient(0deg, #fff 100%)',
      'linear-gradient(0deg, #000 0% #fff 100%)',
      'linear-gradient(0deg #000 0%, #fff 100%)',
    ]
    testBackgroundLayers(parseLinearGradient, valid, invalid)
  })
})

describe('parseRadialGradient', () => {
  it('parses radial gradients', () => {
    const valid = [
      'radial-gradient(#000 0%, #fff 100%)',
      'radial-gradient(50% 50%, #000 0%, #fff 100%)',
      'radial-gradient(at 50% 50%, #000 0%, #fff 100%)',
      'radial-gradient(50% 50% at 50% 50%, #000 0%, #fff 100%)',
    ]
    const invalid = [
      'radial-gradient(#e66465, #9198e5)',
      'radial-gradient(closest-corner at 50% 50%, #000 0%, #fff 100%)',
      'radial-gradient(circle, #000 0%, #fff 100%)',
      'radial-gradient(circle 50px, #000 0%, #fff 100%)',
      'radial-gradient(circle closest-corner, #000 0%, #fff 100%)',
      'radial-gradient(circle closest-side, #000 0%, #fff 100%)',
      'radial-gradient(circle farthest-corner, #000 0%, #fff 100%)',
      'radial-gradient(circle farthest-side, #000 0%, #fff 100%)',
      'radial-gradient(circle at 50%, #000 0%, #fff 100%)',
      'radial-gradient(circle at 50% 50%, #000 0%, #fff 100%)',
      'radial-gradient(circle 50px at 50%, #000 0%, #fff 100%)',
      'radial-gradient(circle 50px at 50% 50%, #000 0%, #fff 100%)',
      'radial-gradient(circle, #000 0%, #444 25%, #888 50%, #bbb 75%, #fff 100%)',
      'radial-gradient(ellipse 50px 50px, #000 0%, #fff 100%)',
      'radial-gradient(ellipse at 50%, #000 0%, #fff 100%)',
      'radial-gradient(50px, #000 0%, #fff 100%)',
      'radial-gradient(ellipse 50px 50px, #000 0%, #fff 100%)',
      'radial-gradient(ellipse closest-corner, #000 0%, #fff 100%)',
      'radial-gradient(ellipse closest-side, #000 0%, #fff 100%)',
      'radial-gradient(ellipse farthest-corner, #000 0%, #fff 100%)',
      'radial-gradient(ellipse farthest-side, #000 0%, #fff 100%)',
      'radial-gradient(closest-corner, #000 0%, #fff 100%)',
      'radial-gradient(closest-side, #000 0%, #fff 100%)',
      'radial-gradient(farthest-corner, #000 0%, #fff 100%)',
      'radial-gradient(farthest-side, #000 0%, #fff 100%)',
      'radial-gradient(closest-corner, #000 0%, #fff 100%)',
      'radial-gradient(ellipse closest-corner, #000 0%, #fff 100%)',
      'radial-gradient(#000 0deg, #fff 360deg)',
      'radial-gradient(circle at 50px 50px, #000 0%, #fff 100%)',
      'radial-gradient(circle 50%, #000 0%, #fff 100%)',
      'radial-gradient(circle 50px 50px, #000 0%, #fff 100%)',
      'radial-gradient(ellipse 50px, #000 0%, #fff 100%)',
      'radial-gradient(50%, #000 0%, #fff 100%)',
      'radial-gradient(ellipsefarthest-side, #000 0%, #fff 100%)',
      'radial-gradient(circle50px, #000 0%, #fff 100%)',
      'radial-gradient(ellipse50% 50%, #000 0%, #fff 100%)',
    ]
    testBackgroundLayers(parseRadialGradient, valid, invalid)
  })
})

describe('parseConicGradient', () => {
  it('parses conic gradients', () => {
    const valid = [
      'conic-gradient(#000 0%, #fff 100%)',
      'conic-gradient(from 0%, #000 0%, #fff 100%)',
      'conic-gradient(from 0deg, #000 0%, #fff 100%)',
      'conic-gradient(at 50% 50%, #000 0%, #fff 100%)',
      'conic-gradient(from 0% at 50% 50%, #000 0%, #fff 100%)',
      'conic-gradient(from 0deg at 50% 50%, #000 0%, #fff 100%)',
      'conic-gradient(at 50% 50%, #000 0%, #fff 100%)',
      'conic-gradient(at 50% 50%, #000 0%, #444 25%, #888 50%, #bbb 75%, #fff 100%)',
    ]
    const invalid = [
      'conic-gradient(#000 0% #fff 100%)',
      'conic-gradient(0, #000 0%, #fff 100%)',
      'conic-gradient(at 50%, #000 0%, #fff 100%)',
      'conic-gradient(at 50px, #000 0%, #fff 100%)',
      'conic-gradient(from 0deg at 50, #000 0%, #fff 100%)',
      'conic-gradient(0deg, #000 0%, #fff 100%)',
    ]
    testBackgroundLayers(parseConicGradient, valid, invalid)
  })
})

describe('parseCSSURLFunction', () => {
  it('parses conic gradients', () => {
    const valid = ['url(crazy-george.png)', "url('crazy-george.png')", 'url("crazy-george.png")']
    const invalid = ['ur(nope.png)']
    testBackgroundLayers(parseCSSURLFunction, valid, invalid)
  })
})

describe('parseBackgroundImage', () => {
  it('parses the backgroundImage CSS property', () => {
    const validStrings = [
      'radial-gradient(#000 0%, #fff 100%)',
      'radial-gradient(#000 0%, #fff 100%), linear-gradient(90deg, #000 0%, #fff 100%), /*radial-gradient(#000 0%, #fff 100%),*/ linear-gradient(#000 0%, #fff 100%), linear-gradient(#000 0%, #000 100%)',
      'url("crazy-george.jpg")',
    ]
    const validCSSBackgroundImageValuesWeDoNotSupportSadly = [
      'repeating-linear-gradient(45deg, #3f87a6, #ebf8e1 15%, #f69d3c 20%)',
      'image-set( "crazy-george.png" 1x, "crazy-george-2x.png" 2x, "crazy-george-print.png" 600dpi)',
    ]
    expect(validStrings.map((valid) => parseBackgroundImage(valid))).toMatchInlineSnapshot(`
      Array [
        Object {
          "type": "RIGHT",
          "value": Array [
            Object {
              "center": Object {
                "x": Object {
                  "default": true,
                  "value": Object {
                    "unit": "%",
                    "value": 50,
                  },
                },
                "y": Object {
                  "default": true,
                  "value": Object {
                    "unit": "%",
                    "value": 50,
                  },
                },
              },
              "enabled": true,
              "gradientSize": Object {
                "height": Object {
                  "default": true,
                  "value": Object {
                    "unit": "%",
                    "value": 100,
                  },
                },
                "width": Object {
                  "default": true,
                  "value": Object {
                    "unit": "%",
                    "value": 100,
                  },
                },
              },
              "stops": Array [
                Object {
                  "color": Object {
                    "hex": "#000",
                    "type": "Hex",
                  },
                  "position": Object {
                    "unit": "%",
                    "value": 0,
                  },
                },
                Object {
                  "color": Object {
                    "hex": "#fff",
                    "type": "Hex",
                  },
                  "position": Object {
                    "unit": "%",
                    "value": 100,
                  },
                },
              ],
              "type": "radial-gradient",
            },
          ],
        },
        Object {
          "type": "RIGHT",
          "value": Array [
            Object {
              "center": Object {
                "x": Object {
                  "default": true,
                  "value": Object {
                    "unit": "%",
                    "value": 50,
                  },
                },
                "y": Object {
                  "default": true,
                  "value": Object {
                    "unit": "%",
                    "value": 50,
                  },
                },
              },
              "enabled": true,
              "gradientSize": Object {
                "height": Object {
                  "default": true,
                  "value": Object {
                    "unit": "%",
                    "value": 100,
                  },
                },
                "width": Object {
                  "default": true,
                  "value": Object {
                    "unit": "%",
                    "value": 100,
                  },
                },
              },
              "stops": Array [
                Object {
                  "color": Object {
                    "hex": "#000",
                    "type": "Hex",
                  },
                  "position": Object {
                    "unit": "%",
                    "value": 0,
                  },
                },
                Object {
                  "color": Object {
                    "hex": "#fff",
                    "type": "Hex",
                  },
                  "position": Object {
                    "unit": "%",
                    "value": 100,
                  },
                },
              ],
              "type": "radial-gradient",
            },
            Object {
              "angle": Object {
                "default": false,
                "value": Object {
                  "unit": "deg",
                  "value": 90,
                },
              },
              "enabled": true,
              "stops": Array [
                Object {
                  "color": Object {
                    "hex": "#000",
                    "type": "Hex",
                  },
                  "position": Object {
                    "unit": "%",
                    "value": 0,
                  },
                },
                Object {
                  "color": Object {
                    "hex": "#fff",
                    "type": "Hex",
                  },
                  "position": Object {
                    "unit": "%",
                    "value": 100,
                  },
                },
              ],
              "type": "linear-gradient",
            },
            Object {
              "center": Object {
                "x": Object {
                  "default": true,
                  "value": Object {
                    "unit": "%",
                    "value": 50,
                  },
                },
                "y": Object {
                  "default": true,
                  "value": Object {
                    "unit": "%",
                    "value": 50,
                  },
                },
              },
              "enabled": false,
              "gradientSize": Object {
                "height": Object {
                  "default": true,
                  "value": Object {
                    "unit": "%",
                    "value": 100,
                  },
                },
                "width": Object {
                  "default": true,
                  "value": Object {
                    "unit": "%",
                    "value": 100,
                  },
                },
              },
              "stops": Array [
                Object {
                  "color": Object {
                    "hex": "#000",
                    "type": "Hex",
                  },
                  "position": Object {
                    "unit": "%",
                    "value": 0,
                  },
                },
                Object {
                  "color": Object {
                    "hex": "#fff",
                    "type": "Hex",
                  },
                  "position": Object {
                    "unit": "%",
                    "value": 100,
                  },
                },
              ],
              "type": "radial-gradient",
            },
            Object {
              "angle": Object {
                "default": true,
                "value": Object {
                  "unit": "deg",
                  "value": 0,
                },
              },
              "enabled": true,
              "stops": Array [
                Object {
                  "color": Object {
                    "hex": "#000",
                    "type": "Hex",
                  },
                  "position": Object {
                    "unit": "%",
                    "value": 0,
                  },
                },
                Object {
                  "color": Object {
                    "hex": "#fff",
                    "type": "Hex",
                  },
                  "position": Object {
                    "unit": "%",
                    "value": 100,
                  },
                },
              ],
              "type": "linear-gradient",
            },
            Object {
              "color": Object {
                "hex": "#000",
                "type": "Hex",
              },
              "enabled": true,
              "type": "solid",
            },
          ],
        },
        Object {
          "type": "RIGHT",
          "value": Array [
            Object {
              "enabled": true,
              "type": "url-function",
              "url": "crazy-george.jpg",
            },
          ],
        },
      ]
    `)
    expect(
      validCSSBackgroundImageValuesWeDoNotSupportSadly.map((valid) => parseBackgroundImage(valid)),
    ).toMatchInlineSnapshot(`
      Array [
        Object {
          "type": "RIGHT",
          "value": Array [
            Object {
              "enabled": true,
              "type": "unknown-array-item",
              "value": "repeating-linear-gradient(45deg, #3f87a6, #ebf8e1 15%, #f69d3c 20%)",
            },
          ],
        },
        Object {
          "type": "RIGHT",
          "value": Array [
            Object {
              "enabled": true,
              "type": "unknown-array-item",
              "value": "image-set( \\"crazy-george.png\\" 1x, \\"crazy-george-2x.png\\" 2x, \\"crazy-george-print.png\\" 600dpi)",
            },
          ],
        },
      ]
    `)

    const invalidStrings = ['a garbage', 'gradient(0deg, #000 0%, #fff 100%)']
    invalidStrings.forEach((invalid) => {
      expect(parseBackgroundImage(invalid).type).toEqual('LEFT')
    })
  })
})

describe('printBackgroundImage', () => {
  it('prints the backgroundImage CSS property', () => {
    const validValues: Array<Array<CSSBackground>> = [
      [
        {
          type: 'radial-gradient',
          enabled: true,
          gradientSize: { ...defaultCSSRadialGradientSize },
          center: { ...defaultCSSRadialOrConicGradientCenter },
          stops: [...defaultCSSGradientStops],
        },
      ],
      [
        {
          type: 'linear-gradient',
          enabled: true,
          angle: {
            default: false,
            value: {
              unit: 'deg',
              value: 90,
            },
          },
          stops: [...defaultCSSGradientStops],
        },
        {
          color: {
            type: 'Hex',
            hex: '#000',
          },
          enabled: true,
          type: 'solid',
        },
      ],
      [
        {
          color: {
            type: 'Hex',
            hex: '#000',
          },
          enabled: true,
          type: 'solid',
        },
        {
          type: 'linear-gradient',
          enabled: true,
          angle: {
            default: true,
            value: {
              unit: 'deg',
              value: 0,
            },
          },
          stops: [...defaultCSSGradientStops],
        },
        {
          type: 'radial-gradient',
          enabled: false,
          gradientSize: { ...defaultCSSRadialGradientSize },
          center: { ...defaultCSSRadialOrConicGradientCenter },
          stops: [...defaultCSSGradientStops],
        },
        {
          type: 'linear-gradient',
          enabled: true,
          angle: {
            default: false,
            value: {
              unit: 'deg',
              value: 90,
            },
          },
          stops: [...defaultCSSGradientStops],
        },
        {
          color: {
            type: 'Hex',
            hex: '#000',
          },
          enabled: false,
          type: 'solid',
        },
        {
          type: 'radial-gradient',
          enabled: true,
          gradientSize: { ...defaultCSSRadialGradientSize },
          center: { ...defaultCSSRadialOrConicGradientCenter },
          stops: [...defaultCSSGradientStops],
        },
      ],
    ]
    const actualResult = validValues.map((valid) =>
      clearExpressionUniqueIDs(printBackgroundImage(valid)),
    )
    expect(actualResult).toMatchInlineSnapshot(`
      Array [
        Object {
          "comments": Object {
            "leadingComments": Array [],
            "trailingComments": Array [],
          },
          "type": "ATTRIBUTE_VALUE",
          "uid": "",
          "value": "radial-gradient(#000 0%, #fff 100%)",
        },
        Object {
          "comments": Object {
            "leadingComments": Array [],
            "trailingComments": Array [],
          },
          "type": "ATTRIBUTE_VALUE",
          "uid": "",
          "value": "linear-gradient(90deg, #000 0%, #fff 100%), linear-gradient(#000 0%, #000 100%)",
        },
        Object {
          "comments": Object {
            "leadingComments": Array [],
            "trailingComments": Array [],
          },
          "type": "ATTRIBUTE_VALUE",
          "uid": "",
          "value": "linear-gradient(#000 0%, #000 100%), linear-gradient(#000 0%, #fff 100%), /*radial-gradient(#000 0%, #fff 100%)*/ linear-gradient(90deg, #000 0%, #fff 100%), /*linear-gradient(#000 0%, #000 100%)*/ radial-gradient(#000 0%, #fff 100%)",
        },
      ]
    `)
  })
})

describe('parseBackgroundColor', () => {
  it('parses background color fills from a string', () => {
    const validStrings: Array<string> = [
      '/*#fff*/',
      '#fff',
      '/*rgba(0, 0, 0, 1)*/',
      'rgba(0, 0, 0, 1)',
      '/*hsl(0, 100%, 0%, 1)*/',
      'hsl(0, 100%, 0%, 1)',
    ]
    expect(validStrings.map((valid) => parseBackgroundColor(valid))).toMatchInlineSnapshot(`
      Array [
        Object {
          "type": "RIGHT",
          "value": Object {
            "default": false,
            "value": Object {
              "color": Object {
                "hex": "#fff",
                "type": "Hex",
              },
              "enabled": false,
              "type": "solid",
            },
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "default": false,
            "value": Object {
              "color": Object {
                "hex": "#fff",
                "type": "Hex",
              },
              "enabled": true,
              "type": "solid",
            },
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "default": false,
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
              "enabled": false,
              "type": "solid",
            },
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "default": false,
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
              "enabled": true,
              "type": "solid",
            },
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "default": false,
            "value": Object {
              "color": Object {
                "a": 1,
                "h": 0,
                "l": 0,
                "percentageAlpha": false,
                "s": 100,
                "type": "HSL",
              },
              "enabled": false,
              "type": "solid",
            },
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "default": false,
            "value": Object {
              "color": Object {
                "a": 1,
                "h": 0,
                "l": 0,
                "percentageAlpha": false,
                "s": 100,
                "type": "HSL",
              },
              "enabled": true,
              "type": "solid",
            },
          },
        },
      ]
    `)
  })
})

describe('parseColor', () => {
  it('parses colors from a string', () => {
    const validStrings: Array<string> = [
      '#fff',
      '#ffff',
      '#ffffff',
      '#ffffffff',
      '#bbbbbbbb',
      'fff',
      'ffff',
      'ffffff',
      'ffffffff',
      'bbbbbbbb',
      'rgba(0, 0, 0, 1)',
      'rgba(0, 0, 0, 100%)',
      'rgba(0%, 0%, 0%, 100%)',
      'hsl(0, 100%, 0%, 1)',
      'hsl(0, 100%, 0%, 100%)',
      'transparent',
    ]
    const expectedValues: Array<CSSColor> = [
      {
        type: 'Hex',
        hex: '#fff',
      },
      {
        type: 'Hex',
        hex: '#ffff',
      },
      {
        type: 'Hex',
        hex: '#ffffff',
      },
      {
        type: 'Hex',
        hex: '#ffffffff',
      },
      {
        type: 'Hex',
        hex: '#bbbbbbbb',
      },
      {
        type: 'Hex',
        hex: '#fff',
      },
      {
        type: 'Hex',
        hex: '#ffff',
      },
      {
        type: 'Hex',
        hex: '#ffffff',
      },
      {
        type: 'Hex',
        hex: '#ffffffff',
      },
      {
        type: 'Hex',
        hex: '#bbbbbbbb',
      },
      {
        type: 'RGB',
        r: 0,
        g: 0,
        b: 0,
        a: 1,
        percentagesUsed: false,
        percentageAlpha: false,
      },
      {
        type: 'RGB',
        r: 0,
        g: 0,
        b: 0,
        a: 1,
        percentagesUsed: false,
        percentageAlpha: true,
      },
      {
        type: 'RGB',
        r: 0,
        g: 0,
        b: 0,
        a: 1,
        percentagesUsed: true,
        percentageAlpha: true,
      },
      {
        type: 'HSL',
        h: 0,
        s: 100,
        l: 0,
        a: 1,
        percentageAlpha: false,
      },
      {
        type: 'HSL',
        h: 0,
        s: 100,
        l: 0,
        a: 1,
        percentageAlpha: true,
      },
      {
        type: 'Hex',
        hex: '#0000',
      },
    ]
    validStrings.forEach((valid, i) => {
      expect(parseColor(valid, 'hex-hash-optional')).toEqual(right(expectedValues[i]))
    })

    const invalidStrings: Array<string> = [
      '#fffff',
      '#fffffffff',
      'rgb(0, 1)',
      'burple',
      'hsla()',
      'orangey',
    ]
    invalidStrings.forEach((invalid, i) => {
      expect(parseColor(invalid, 'hex-hash-optional').type).toEqual('LEFT')
    })
  })
})

describe('parseBoxShadow', () => {
  it('parses a box shadow', () => {
    const validStrings = [
      '1px 1px #fff, 1px 1px 1px #fff, 1px 1px 1px 1px #fff, 1px 1px 0 1px #fff, 1px 1px 0 0 #fff /*1px 1px #fff*/',
    ]
    expect(validStrings.map((valid) => parseBoxShadow(valid))).toMatchInlineSnapshot(`
      Array [
        Object {
          "type": "RIGHT",
          "value": Array [
            Object {
              "blurRadius": Object {
                "default": true,
                "value": Object {
                  "unit": "px",
                  "value": 0,
                },
              },
              "color": Object {
                "hex": "#fff",
                "type": "Hex",
              },
              "enabled": true,
              "inset": false,
              "offsetX": Object {
                "unit": "px",
                "value": 1,
              },
              "offsetY": Object {
                "unit": "px",
                "value": 1,
              },
              "spreadRadius": Object {
                "default": true,
                "value": Object {
                  "unit": "px",
                  "value": 0,
                },
              },
              "type": "box-shadow",
            },
            Object {
              "blurRadius": Object {
                "default": false,
                "value": Object {
                  "unit": "px",
                  "value": 1,
                },
              },
              "color": Object {
                "hex": "#fff",
                "type": "Hex",
              },
              "enabled": true,
              "inset": false,
              "offsetX": Object {
                "unit": "px",
                "value": 1,
              },
              "offsetY": Object {
                "unit": "px",
                "value": 1,
              },
              "spreadRadius": Object {
                "default": true,
                "value": Object {
                  "unit": "px",
                  "value": 0,
                },
              },
              "type": "box-shadow",
            },
            Object {
              "blurRadius": Object {
                "default": false,
                "value": Object {
                  "unit": "px",
                  "value": 1,
                },
              },
              "color": Object {
                "hex": "#fff",
                "type": "Hex",
              },
              "enabled": true,
              "inset": false,
              "offsetX": Object {
                "unit": "px",
                "value": 1,
              },
              "offsetY": Object {
                "unit": "px",
                "value": 1,
              },
              "spreadRadius": Object {
                "default": false,
                "value": Object {
                  "unit": "px",
                  "value": 1,
                },
              },
              "type": "box-shadow",
            },
            Object {
              "blurRadius": Object {
                "default": false,
                "value": Object {
                  "unit": null,
                  "value": 0,
                },
              },
              "color": Object {
                "hex": "#fff",
                "type": "Hex",
              },
              "enabled": true,
              "inset": false,
              "offsetX": Object {
                "unit": "px",
                "value": 1,
              },
              "offsetY": Object {
                "unit": "px",
                "value": 1,
              },
              "spreadRadius": Object {
                "default": false,
                "value": Object {
                  "unit": "px",
                  "value": 1,
                },
              },
              "type": "box-shadow",
            },
            Object {
              "blurRadius": Object {
                "default": false,
                "value": Object {
                  "unit": null,
                  "value": 0,
                },
              },
              "color": Object {
                "hex": "#fff",
                "type": "Hex",
              },
              "enabled": true,
              "inset": false,
              "offsetX": Object {
                "unit": "px",
                "value": 1,
              },
              "offsetY": Object {
                "unit": "px",
                "value": 1,
              },
              "spreadRadius": Object {
                "default": false,
                "value": Object {
                  "unit": null,
                  "value": 0,
                },
              },
              "type": "box-shadow",
            },
            Object {
              "blurRadius": Object {
                "default": true,
                "value": Object {
                  "unit": "px",
                  "value": 0,
                },
              },
              "color": Object {
                "hex": "#fff",
                "type": "Hex",
              },
              "enabled": false,
              "inset": false,
              "offsetX": Object {
                "unit": "px",
                "value": 1,
              },
              "offsetY": Object {
                "unit": "px",
                "value": 1,
              },
              "spreadRadius": Object {
                "default": true,
                "value": Object {
                  "unit": "px",
                  "value": 0,
                },
              },
              "type": "box-shadow",
            },
          ],
        },
      ]
    `)

    const invalidStrings = ['1px 1px burple', '1px #fff', '#fff']
    expect(invalidStrings.map((invalid) => parseBoxShadow(invalid))).toMatchInlineSnapshot(`
      Array [
        Object {
          "type": "LEFT",
          "value": "No box shadows found",
        },
        Object {
          "type": "LEFT",
          "value": "No box shadows found",
        },
        Object {
          "type": "LEFT",
          "value": "No box shadows found",
        },
      ]
    `)
  })
})

describe('parseTextShadow', () => {
  it('parses a text shadow', () => {
    const validStrings = ['0px 0px 0px #fff, /*0 0 1px #999*/ 0 0 #111']
    const expectedValue: Array<CSSTextShadows> = [
      [
        {
          enabled: true,
          offsetX: cssPixelLengthZero,
          offsetY: cssPixelLengthZero,
          blurRadius: cssDefault(cssPixelLengthZero, false),
          color: cssColor('#fff'),
        },
        {
          enabled: false,
          offsetX: cssUnitlessLength(0),
          offsetY: cssUnitlessLength(0),
          blurRadius: cssDefault(cssPixelLength(1), false),
          color: cssColor('#999'),
        },
        {
          enabled: true,
          offsetX: cssUnitlessLength(0),
          offsetY: cssUnitlessLength(0),
          blurRadius: cssDefault(cssPixelLengthZero),
          color: cssColor('#111'),
        },
      ],
    ]
    validStrings.forEach((valid, i) => {
      expect(parseTextShadow(valid)).toEqual(right(expectedValue[i]))
    })
  })
})

describe('parseBorderRadius', () => {
  it('parses a border radius', () => {
    const validStrings = ['12px', '11%']
    const expectedValue: Array<CSSBorderRadius> = [
      {
        type: 'LEFT',
        value: {
          unit: 'px',
          value: 12,
        },
      },
      {
        type: 'LEFT',
        value: {
          unit: '%',
          value: 11,
        },
      },
    ]
    validStrings.forEach((valid, i) => {
      expect(parseBorderRadius(valid)).toEqual(right(expectedValue[i]))
    })
  })
})

describe('cssColorToChromaColor', () => {
  it('parses a color keyword', () => {
    const validStrings = [
      'aliceblue',
      'antiquewhite',
      'aqua',
      'aquamarine',
      'azure',
      'beige',
      'bisque',
      'black',
      'blanchedalmond',
      'blue',
      'blueviolet',
      'brown',
      'burlywood',
      'cadetblue',
      'chartreuse',
      'chocolate',
      'coral',
      'cornflowerblue',
      'cornsilk',
      'crimson',
      'cyan',
      'darkblue',
      'darkcyan',
      'darkgoldenrod',
      'darkgray',
      'darkgreen',
      'darkgrey',
      'darkkhaki',
      'darkmagenta',
      'darkolivegreen',
      'darkorange',
      'darkorchid',
      'darkred',
      'darksalmon',
      'darkseagreen',
      'darkslateblue',
      'darkslategray',
      'darkslategrey',
      'darkturquoise',
      'darkviolet',
      'deeppink',
      'deepskyblue',
      'dimgray',
      'dimgrey',
      'dodgerblue',
      'firebrick',
      'floralwhite',
      'forestgreen',
      'fuchsia',
      'gainsboro',
      'ghostwhite',
      'gold',
      'goldenrod',
      'gray',
      'green',
      'greenyellow',
      'grey',
      'honeydew',
      'hotpink',
      'indianred',
      'indigo',
      'ivory',
      'khaki',
      'lavender',
      'lavenderblush',
      'lawngreen',
      'lemonchiffon',
      'lightblue',
      'lightcoral',
      'lightcyan',
      'lightgoldenrodyellow',
      'lightgray',
      'lightgreen',
      'lightgrey',
      'lightpink',
      'lightsalmon',
      'lightseagreen',
      'lightskyblue',
      'lightslategray',
      'lightslategrey',
      'lightsteelblue',
      'lightyellow',
      'lime',
      'limegreen',
      'linen',
      'magenta',
      'maroon',
      'mediumaquamarine',
      'mediumblue',
      'mediumorchid',
      'mediumpurple',
      'mediumseagreen',
      'mediumslateblue',
      'mediumspringgreen',
      'mediumturquoise',
      'mediumvioletred',
      'midnightblue',
      'mintcream',
      'mistyrose',
      'moccasin',
      'navajowhite',
      'navy',
      'oldlace',
      'olive',
      'olivedrab',
      'orange',
      'orangered',
      'orchid',
      'palegoldenrod',
      'palegreen',
      'paleturquoise',
      'palevioletred',
      'papayawhip',
      'peachpuff',
      'peru',
      'pink',
      'plum',
      'powderblue',
      'purple',
      'rebeccapurple',
      'red',
      'rosybrown',
      'royalblue',
      'saddlebrown',
      'salmon',
      'sandybrown',
      'seagreen',
      'seashell',
      'sienna',
      'silver',
      'skyblue',
      'slateblue',
      'slategray',
      'slategrey',
      'snow',
      'springgreen',
      'steelblue',
      'tan',
      'teal',
      'thistle',
      'tomato',
      'turquoise',
      'violet',
      'wheat',
      'white',
      'whitesmoke',
      'yellow',
      'yellowgreen',
    ]

    const invalidStrings = ['transparent', 'burple']

    validStrings.forEach((valid) => {
      expect(isRight(cssColorToChromaColor({ type: 'Keyword', keyword: valid }))).toEqual(true)
    })
    invalidStrings.forEach((invalid) => {
      expect(isLeft(cssColorToChromaColor({ type: 'Keyword', keyword: invalid }))).toEqual(true)
    })
  })
  it('parses hsl values correctly', () => {
    expect(cssColorToChromaColor(cssColorHSL(0, 0, 95, 1, false))).toMatchInlineSnapshot(`
      Object {
        "type": "RIGHT",
        "value": Color {
          "_rgb": Array [
            242.25,
            242.25,
            242.25,
            1,
          ],
        },
      }
    `)
  })
})

describe('parseBackgroundColor 2', () => {
  it('parses a background color', () => {
    const validStrings = ['#fff', 'rgba(255 255 255 / 1)']

    expect(validStrings.map((valid) => parseBackgroundColor(valid))).toMatchInlineSnapshot(`
      Array [
        Object {
          "type": "RIGHT",
          "value": Object {
            "default": false,
            "value": Object {
              "color": Object {
                "hex": "#fff",
                "type": "Hex",
              },
              "enabled": true,
              "type": "solid",
            },
          },
        },
        Object {
          "type": "RIGHT",
          "value": Object {
            "default": false,
            "value": Object {
              "color": Object {
                "a": 1,
                "b": 255,
                "g": 255,
                "percentageAlpha": false,
                "percentagesUsed": false,
                "r": 255,
                "type": "RGB",
              },
              "enabled": true,
              "type": "solid",
            },
          },
        },
      ]
    `)
  })
})

describe('parseTransform', () => {
  it('parses transforms', () => {
    const validStrings = [
      'rotate(45deg)',
      'rotateX(45deg)',
      'rotateY(45deg)',
      'rotateZ(45deg)',
      'scale(2)',
      'scale(2, 2)',
      'scaleX(2)',
      'scaleY(2)',
      'scaleZ(2)',
      'skew(45deg)',
      'skew(45deg, 45deg)',
      'skewX(45deg)',
      'skewY(45deg)',
      'translate(10px)',
      'translate(10px, 10px)',
      'translateX(10px)',
      'translateY(10px)',
      'translateZ(10px)',
      'rotateZ(45deg) scale(2)',
      'rotate(45deg) rotateX(45deg) rotateY(45deg) rotateZ(45deg) scale(2) scale(2, 2) scaleX(2) scaleY(2) scaleZ(2) skew(45deg) skew(45deg, 45deg) skewX(45deg) skewY(45deg) translate(10px) translate(10px, 10px) translateX(10px) translateY(10px) translateZ(10px)',
      '/*rotate(45deg)*/',
      'rotateZ(45deg) /*scale(2)*/',
    ]
    const expectedValue: Array<CSSTransforms> = [
      [cssTransformRotate(cssAngle(45))],
      [cssTransformRotateX(cssAngle(45))],
      [cssTransformRotateY(cssAngle(45))],
      [cssTransformRotateZ(cssAngle(45))],
      [cssTransformScale(cssUnitlessLength(2))],
      [cssTransformScale(cssUnitlessLength(2), cssDefault(cssUnitlessLength(2), false))],
      [cssTransformScaleX(cssUnitlessLength(2))],
      [cssTransformScaleY(cssUnitlessLength(2))],
      [cssTransformScaleZ(cssUnitlessLength(2))],
      [cssTransformSkew(cssAngle(45))],
      [cssTransformSkew(cssAngle(45), cssDefault(cssAngle(45), false))],
      [cssTransformSkewX(cssAngle(45))],
      [cssTransformSkewY(cssAngle(45))],
      [cssTransformTranslate(cssPixelLength(10))],
      [cssTransformTranslate(cssPixelLength(10), cssDefault(cssPixelLength(10), false))],
      [cssTransformTranslateX(cssPixelLength(10))],
      [cssTransformTranslateY(cssPixelLength(10))],
      [cssTransformTranslateZ(cssPixelLength(10))],
      [cssTransformRotateZ(cssAngle(45)), cssTransformScale(cssUnitlessLength(2))],
      [
        cssTransformRotate(cssAngle(45)),
        cssTransformRotateX(cssAngle(45)),
        cssTransformRotateY(cssAngle(45)),
        cssTransformRotateZ(cssAngle(45)),
        cssTransformScale(cssUnitlessLength(2)),
        cssTransformScale(cssUnitlessLength(2), cssDefault(cssUnitlessLength(2), false)),
        cssTransformScaleX(cssUnitlessLength(2)),
        cssTransformScaleY(cssUnitlessLength(2)),
        cssTransformScaleZ(cssUnitlessLength(2)),
        cssTransformSkew(cssAngle(45)),
        cssTransformSkew(cssAngle(45), cssDefault(cssAngle(45), false)),
        cssTransformSkewX(cssAngle(45)),
        cssTransformSkewY(cssAngle(45)),
        cssTransformTranslate(cssPixelLength(10)),
        cssTransformTranslate(cssPixelLength(10), cssDefault(cssPixelLength(10), false)),
        cssTransformTranslateX(cssPixelLength(10)),
        cssTransformTranslateY(cssPixelLength(10)),
        cssTransformTranslateZ(cssPixelLength(10)),
      ],
      [cssTransformRotate(cssAngle(45), false)],
      [
        cssTransformRotateZ(cssAngle(45)),
        cssTransformScale(cssUnitlessLength(2), undefined, false),
      ],
    ]

    validStrings.forEach((valid, i) => {
      expect(parseTransform(valid)).toEqual(right(expectedValue[i]))
    })
  })
})

describe('printBackgroundSize', () => {
  it('prints basic values', () => {
    const backgroundSize: CSSBackgroundSize = [
      { ...defaultBGSize },
      cssBGSize(cssDefault(parsedCurlyBrace([cssKeyword('auto'), cssKeyword('auto')]), false)),
      cssBGSize(cssDefault(parsedCurlyBrace([cssNumber(100, 'px')]), false)),
      cssBGSize(cssDefault(parsedCurlyBrace([cssNumber(100, '%'), cssNumber(100, '%')]), false)),
    ]
    expect(clearExpressionUniqueIDs(printBackgroundSize(backgroundSize))).toMatchInlineSnapshot(`
      Object {
        "comments": Object {
          "leadingComments": Array [],
          "trailingComments": Array [],
        },
        "type": "ATTRIBUTE_VALUE",
        "uid": "",
        "value": "auto, auto auto, 100px, 100% 100%",
      }
    `)
  })
})

describe('stringifyGridDimension', () => {
  it('keyword', async () => {
    expect(stringifyGridDimension(gridCSSKeyword(cssKeyword('auto'), null))).toBe('auto')
    expect(stringifyGridDimension(gridCSSKeyword(cssKeyword('auto'), 'the-area'))).toBe('auto')
  })

  it('number', async () => {
    expect(stringifyGridDimension(gridCSSNumber(cssNumber(123), null))).toBe('123')
    expect(stringifyGridDimension(gridCSSNumber(cssNumber(123, 'px'), null))).toBe('123px')
    expect(stringifyGridDimension(gridCSSNumber(cssNumber(123), 'the-area'))).toBe('123')
  })

  it('repeat', async () => {
    expect(
      stringifyGridDimension(
        gridCSSRepeat(3, [
          gridCSSKeyword(cssKeyword('auto'), null),
          gridCSSKeyword(cssKeyword('min-content'), null),
          gridCSSNumber(cssNumber(123, 'px'), null),
        ]),
      ),
    ).toBe(`repeat(3, auto min-content 123px)`)

    expect(
      stringifyGridDimension(
        gridCSSRepeat(3, [
          gridCSSKeyword(cssKeyword('auto'), 'foo'),
          gridCSSKeyword(cssKeyword('min-content'), 'bar'),
          gridCSSNumber(cssNumber(123, 'px'), null),
        ]),
      ),
    ).toBe(`repeat(3, [foo] auto [bar] min-content 123px)`)

    expect(
      stringifyGridDimension(
        gridCSSRepeat(cssKeyword('auto-fit'), [
          gridCSSMinmax(
            gridCSSNumber(cssNumber(400, 'px'), null),
            gridCSSNumber(cssNumber(1, 'fr'), null),
            null,
          ),
        ]),
      ),
    ).toBe(`repeat(auto-fit, minmax(400px, 1fr))`)
  })

  it('minmax', async () => {
    expect(
      stringifyGridDimension(
        gridCSSMinmax(
          gridCSSKeyword(cssKeyword('auto'), null),
          gridCSSKeyword(cssKeyword('min-content'), null),
          null,
        ),
      ),
    ).toBe('minmax(auto, min-content)')

    expect(
      stringifyGridDimension(
        gridCSSMinmax(
          gridCSSKeyword(cssKeyword('auto'), null),
          gridCSSKeyword(cssKeyword('min-content'), null),
          'the-area',
        ),
      ),
    ).toBe('minmax(auto, min-content)')
  })
})

describe('printGridDimension', () => {
  it('keyword', async () => {
    expect(printGridDimension(gridCSSKeyword(cssKeyword('auto'), null))).toBe('auto')
    expect(printGridDimension(gridCSSKeyword(cssKeyword('auto'), 'the-area'))).toBe(
      '[the-area] auto',
    )
  })

  it('number', async () => {
    expect(printGridDimension(gridCSSNumber(cssNumber(123), null))).toBe('123')
    expect(printGridDimension(gridCSSNumber(cssNumber(123, 'px'), null))).toBe('123px')
    expect(printGridDimension(gridCSSNumber(cssNumber(123), 'the-area'))).toBe('[the-area] 123')
  })

  it('repeat', async () => {
    expect(
      printGridDimension(
        gridCSSRepeat(3, [
          gridCSSKeyword(cssKeyword('auto'), null),
          gridCSSKeyword(cssKeyword('min-content'), null),
          gridCSSNumber(cssNumber(123, 'px'), null),
        ]),
      ),
    ).toBe(`repeat(3, auto min-content 123px)`)

    expect(
      printGridDimension(
        gridCSSRepeat(3, [
          gridCSSKeyword(cssKeyword('auto'), 'foo'),
          gridCSSKeyword(cssKeyword('min-content'), 'bar'),
          gridCSSNumber(cssNumber(123, 'px'), null),
        ]),
      ),
    ).toBe(`repeat(3, [foo] auto [bar] min-content 123px)`)

    expect(
      printGridDimension(
        gridCSSRepeat(cssKeyword('auto-fit'), [
          gridCSSMinmax(
            gridCSSNumber(cssNumber(400, 'px'), null),
            gridCSSNumber(cssNumber(1, 'fr'), null),
            null,
          ),
        ]),
      ),
    ).toBe(`repeat(auto-fit, minmax(400px, 1fr))`)
  })

  it('minmax', async () => {
    expect(
      printGridDimension(
        gridCSSMinmax(
          gridCSSKeyword(cssKeyword('auto'), null),
          gridCSSKeyword(cssKeyword('min-content'), null),
          null,
        ),
      ),
    ).toBe('minmax(auto, min-content)')

    expect(
      printGridDimension(
        gridCSSMinmax(
          gridCSSKeyword(cssKeyword('auto'), null),
          gridCSSKeyword(cssKeyword('min-content'), null),
          'the-area',
        ),
      ),
    ).toBe('[the-area] minmax(auto, min-content)')

    expect(
      printGridDimension(
        gridCSSMinmax(
          gridCSSKeyword(cssKeyword('auto'), 'foo'),
          gridCSSKeyword(cssKeyword('min-content'), 'bar'),
          'the-area',
        ),
      ),
    ).toBe('[the-area] minmax(auto, min-content)')
  })
})
