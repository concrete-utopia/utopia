import { render } from '@testing-library/react'
import { renderHook } from '@testing-library/react-hooks'
import * as React from 'react'
import { isRight } from '../../../core/shared/either'
import {
  isJSXElement,
  isUtopiaJSXComponent,
  JSXAttributes,
  jsxAttributeValue,
  ComputedStyle,
} from '../../../core/shared/element-template'
import { CanvasMetadataName } from '../../../core/workers/parser-printer/parser-printer-parsing'
import { testParseCode } from '../../../core/workers/parser-printer/parser-printer-test-utils'
import {
  enableWhyDidYouRenderOnComponent,
  setupReactWhyDidYouRender,
} from '../../../utils/react-memoize-test-utils'
import utils from '../../../utils/utils'
import {
  backgroundImagesAndColorToCSSBackgroundLayerArray,
  cssBackgroundLayerArrayToBackgroundImagesAndColor,
  backgroundLonghandPaths,
} from '../sections/style-section/background-subsection/background-subsection'
import { ControlStatus } from './control-status'
import {
  blackHexCSSColor,
  CSSBackgroundLayers,
  cssNumber,
  CSSSolidColor,
  cssSolidColor,
  defaultLinearGradientBackgroundLayer,
  printCSSNumber,
  cssSolidBackgroundLayer,
} from './css-utils'
import {
  InspectorCallbackContext,
  InspectorCallbackContextData,
  InspectorPropsContext,
  InspectorPropsContextData,
  stylePropPathMappingFn,
  useCallbackFactory,
  useInspectorInfo,
  useInspectorStyleInfo,
} from './property-path-hooks'
import { betterReactMemo } from 'uuiui-deps'

interface RenderTestHookProps<T> {
  value: T
  callback: (newValue: T, transient?: boolean) => void
  transformFunction1: <NT>(newValue: NT, oldValue: T) => T
  transformFunction2?: <NT>(newValue: NT, oldValue: T) => T
}

function useRenderTestHook<T>(props: RenderTestHookProps<T>) {
  /* eslint-disable react-hooks/rules-of-hooks */
  const factory = useCallbackFactory(props.value, props.callback)

  const [onSubmitValue1] = factory(props.transformFunction1)
  const [onSubmitValue2] = props.transformFunction2 ? factory(props.transformFunction2) : [null]

  return {
    submitValue1: onSubmitValue1,
    submitValue2: onSubmitValue2,
  }

  /* eslint-enable react-hooks/rules-of-hooks */
}

describe('useCallbackFactory', () => {
  it('the returned factory memoizes based on inputs', () => {
    const oldValue = 'a value'
    const aCallback = () => {}

    const aTransform = () => 'hello'

    const { result, rerender } = renderHook((props) => useRenderTestHook(props), {
      initialProps: {
        value: oldValue,
        callback: aCallback,
        transformFunction1: aTransform,
      },
    })

    const { submitValue1 } = result.current

    rerender({
      value: oldValue,
      callback: aCallback,
      transformFunction1: aTransform,
    })

    const { submitValue1: submitValue1b } = result.current

    expect(submitValue1).toStrictEqual(submitValue1b)
  })
  it('the returned factory memoizes based on inputs even if the factory is used more than once', () => {
    const oldValue = 'a value'
    const aCallback = () => {}

    const aTransform = () => 'hello'
    const bTransform = () => 'ello'

    const { result, rerender } = renderHook((props) => useRenderTestHook(props), {
      initialProps: {
        value: oldValue,
        callback: aCallback,
        transformFunction1: aTransform,
        transformFunction2: bTransform,
      },
    })

    const { submitValue1, submitValue2 } = result.current

    rerender({
      value: oldValue,
      callback: aCallback,
      transformFunction1: aTransform,
      transformFunction2: bTransform,
    })

    const { submitValue1: submitValue1b, submitValue2: submitValue2b } = result.current

    expect(submitValue1).toStrictEqual(submitValue1b)
    expect(submitValue2).toStrictEqual(submitValue2b!)
  })

  it('the returned factory returns new callbacks if the oldValue or the main callback changes', () => {
    const oldValue = 'a value'
    const newValue = 'b value'
    const aCallback = () => {}

    const aTransform = () => 'hello'
    const bTransform = () => 'ello'

    const { result, rerender } = renderHook((props) => useRenderTestHook(props), {
      initialProps: {
        value: oldValue,
        callback: aCallback,
        transformFunction1: aTransform,
        transformFunction2: bTransform,
      },
    })

    const { submitValue1, submitValue2 } = result.current

    rerender({
      value: newValue,
      callback: aCallback,
      transformFunction1: aTransform,
      transformFunction2: bTransform,
    })

    const { submitValue1: submitValue1b, submitValue2: submitValue2b } = result.current

    expect(submitValue1).not.toStrictEqual(submitValue1b)
    expect(submitValue2).not.toStrictEqual(submitValue2b!)
  })

  it('since this is a hook that returns a hook, it will break if we rerender with different amount of factory calls', () => {
    const oldValue = 'a value'
    const aCallback = () => {}

    const aTransform = () => 'hello'
    const bTransform = () => 'ello'

    const { rerender } = renderHook<
      RenderTestHookProps<any>,
      {
        submitValue1: (newValue: unknown) => void
        submitValue2: ((newValue: unknown) => void) | null
      }
    >((props) => useRenderTestHook(props), {
      initialProps: {
        value: oldValue,
        callback: aCallback,
        transformFunction1: aTransform,
        transformFunction2: bTransform,
      },
    })

    expect(() => {
      console.error('WE EXPECT AN ERROR MESSAGE ON THE CONSOLE - DONT WORRY ITS FOR A TEST')
      rerender({
        value: oldValue,
        callback: aCallback,
        transformFunction1: aTransform,
      })
    }).toThrow()
  })
})

const WellBehavedInspectorSubsection = betterReactMemo('WellBehavedInspectorSubsection', () => {
  const { value, onSubmitValue } = useInspectorStyleInfo('opacity')
  onSubmitValue(cssNumber(0.9))
  return <div onClick={() => onSubmitValue(cssNumber(0.5))}>{printCSSNumber(value)}</div>
})
enableWhyDidYouRenderOnComponent(WellBehavedInspectorSubsection)

const InspectorSectionProvider = (props: {
  propsData: InspectorPropsContextData
  callbackData: InspectorCallbackContextData
}) => {
  return (
    <InspectorCallbackContext.Provider value={props.callbackData}>
      <InspectorPropsContext.Provider value={props.propsData}>
        <WellBehavedInspectorSubsection />
      </InspectorPropsContext.Provider>
    </InspectorCallbackContext.Provider>
  )
}

describe('useInspectorMetadataForPropsObject memoization', () => {
  const callbackData = { onSubmitValue: utils.NO_OP, onUnsetValue: utils.NO_OP }

  it('make sure the tested component is in testing mode', () => {
    expect((WellBehavedInspectorSubsection as any).whyDidYouRender).toBeTruthy()
  })

  it('this hook wont cause rerender if the context is exactly the same', () => {
    const [getUpdateCount] = setupReactWhyDidYouRender()
    const propsWithOpacity: JSXAttributes[] = [
      {
        style: jsxAttributeValue({ opacity: cssNumber(0.9) }),
      },
    ]
    const realValues: Array<{ [key: string]: any }> = [
      {
        style: { opacity: { value: 0.9 } },
      },
    ]
    const computedStyles = [
      {
        opacity: '0.9',
      },
    ]
    const { rerender } = render(
      <InspectorSectionProvider
        propsData={{
          editedMultiSelectedProps: propsWithOpacity,
          targetPath: ['myStyleOuter', 'myStyleInner'],
          realValues: realValues,
          computedStyles: computedStyles,
        }}
        callbackData={callbackData}
      />,
    )
    rerender(
      <InspectorSectionProvider
        propsData={{
          editedMultiSelectedProps: propsWithOpacity,
          targetPath: ['myStyleOuter', 'myStyleInner'],
          realValues: realValues,
          computedStyles: computedStyles,
        }}
        callbackData={callbackData}
      />,
    )
    expect(getUpdateCount()).toEqual(0)
  })
  it('this hook wont cause rerender if the single selected JSXAttributes stays the same', () => {
    const [getUpdateCount] = setupReactWhyDidYouRender()
    const propsWithOpacity: JSXAttributes = {
      style: jsxAttributeValue({ opacity: 0.9 }),
    }
    const realValues: Array<{ [key: string]: any }> = [
      {
        style: { opacity: 0.9 },
      },
    ]
    const computedStyles = [
      {
        opacity: '0.9',
      },
    ]
    const { rerender } = render(
      <InspectorSectionProvider
        propsData={{
          editedMultiSelectedProps: [propsWithOpacity],
          targetPath: ['myStyleOuter', 'myStyleInner'],
          realValues: realValues,
          computedStyles: computedStyles,
        }}
        callbackData={callbackData}
      />,
    )
    rerender(
      <InspectorSectionProvider
        propsData={{
          editedMultiSelectedProps: [propsWithOpacity],
          targetPath: ['myStyleOuter', 'myStyleInner'],
          realValues: realValues,
          computedStyles: computedStyles,
        }}
        callbackData={callbackData}
      />,
    )
    expect(getUpdateCount()).toEqual(0)
  })

  it('if props change, but not the prop we care about, skip rerender', () => {
    const [getUpdateCount] = setupReactWhyDidYouRender()
    const opacityProp = jsxAttributeValue(0.9)
    const propsWithOpacity: JSXAttributes = {
      // FIXME: This nests `jsxAttributeValue` inside a `jsxAttributeValue`.
      style: jsxAttributeValue({ opacity: opacityProp, otherProp: 'dontcare' }),
    }
    const propsChangedOpacitySame: JSXAttributes = {
      // FIXME: This nests `jsxAttributeValue` inside a `jsxAttributeValue`.
      style: jsxAttributeValue({ opacity: opacityProp, otherProp: 'imdifferent' }),
    }
    const realValues: Array<{ [key: string]: any }> = [
      {
        style: { opacity: 0.9, otherProp: 'dontcare' },
      },
    ]
    const realValuesChanged: Array<{ [key: string]: any }> = [
      {
        style: { opacity: 0.9, otherProp: 'imdifferent' },
      },
    ]
    const computedStyles = [
      {
        opacity: '0.9',
        otherProp: 'dontcare',
      },
    ]
    const computedStylesChanged = [
      {
        opacity: '0.9',
        otherProp: 'imdifferent',
      },
    ]
    const { rerender, getByText } = render(
      <InspectorSectionProvider
        propsData={{
          editedMultiSelectedProps: [propsWithOpacity],
          targetPath: ['myStyleOuter', 'myStyleInner'],
          realValues: realValues,
          computedStyles: computedStyles,
        }}
        callbackData={callbackData}
      />,
    )
    rerender(
      <InspectorSectionProvider
        propsData={{
          editedMultiSelectedProps: [propsChangedOpacitySame],
          targetPath: ['myStyleOuter', 'myStyleInner'],
          realValues: realValuesChanged,
          computedStyles: computedStylesChanged,
        }}
        callbackData={callbackData}
      />,
    )
    expect(getUpdateCount()).toEqual(0)
  })

  it('if the relevant prop changes, do rerender', () => {
    const [getUpdateCount] = setupReactWhyDidYouRender()
    const opacityProp = 0.9
    const propsWithOpacity: JSXAttributes = {
      style: jsxAttributeValue({ opacity: opacityProp, otherProp: 'dontcare' }),
    }
    const propsWithOpacityChanged: JSXAttributes = {
      style: jsxAttributeValue({ opacity: 0.5, otherProp: 'imdifferent' }),
    }
    const realValues: Array<{ [key: string]: any }> = [
      {
        style: { opacity: 0.9, otherProp: 'dontcare' },
      },
    ]
    const realValuesChanged: Array<{ [key: string]: any }> = [
      {
        style: { opacity: 0.5, otherProp: 'imdifferent' },
      },
    ]
    const computedStyles = [
      {
        opacity: '0.9',
        otherProp: 'dontcare',
      },
    ]
    const computedStylesChanged = [
      {
        opacity: '0.5',
        otherProp: 'imdifferent',
      },
    ]
    const { rerender, getByText } = render(
      <InspectorSectionProvider
        propsData={{
          editedMultiSelectedProps: [propsWithOpacity],
          targetPath: ['style'],
          realValues: realValues,
          computedStyles: computedStyles,
        }}
        callbackData={callbackData}
      />,
    )
    const renderedElement = getByText('0.9')
    expect(renderedElement).toBeDefined()
    rerender(
      <InspectorSectionProvider
        propsData={{
          editedMultiSelectedProps: [propsWithOpacityChanged],
          targetPath: ['style'],
          realValues: realValuesChanged,
          computedStyles: computedStylesChanged,
        }}
        callbackData={callbackData}
      />,
    )
    const reRenderedElement = getByText('0.5')
    expect(reRenderedElement).toBeDefined()
    expect(getUpdateCount()).toBeGreaterThan(0)
  })
})

function getPropsForStyleProp(
  targetPropExpression: string,
  target: string[],
): JSXAttributes | null {
  // this test starts with real code, and uses the parser
  // the aim here is to capture a vertical understanding from code -> UI

  const targetExprPrefix1 = `${target[0]}={`
  const targetExprPrefix2 = target
    .slice(1)
    .map((t) => `{${t}:`)
    .join('\n')

  const targetExprPostfix = target.map((t) => `}`).join('\n')
  const code = `import * as React from "react";
  import {
    Ellipse,
    Image,
    Rectangle,
    Text,
    View
  } from "utopia-api";
  import { cake } from 'cake'
  export var ${CanvasMetadataName} = {
    scenes: [],
    elementMetadata: {
      aab: {
        name: 'hello',
        aspectRatioLocked: true,
      },
      "111": {
        name: 'hi',
        aspectRatioLocked: true,
      }
    },
  }
  
  export var App = (props) => {
    return (
      <View
        uid={'aaa'} 
        ${targetExprPrefix1}
        ${targetExprPrefix2}
          ${targetPropExpression}
        ${targetExprPostfix}
      />
    )
  }`

  const parseResult = testParseCode(code)
  if (!isRight(parseResult)) {
    fail('expected parseResult to be Right')
    return null
  }
  const appComponent = parseResult.value.topLevelElements[0]
  if (!isUtopiaJSXComponent(appComponent) || appComponent.name !== `App`) {
    fail('expected the second topLevelElement to be the App component')
    return null
  }
  if (!isJSXElement(appComponent.rootElement)) {
    fail(`expected the App component's root element to be a JSXElement`)
    return null
  }

  return appComponent.rootElement.props
}

const makeInspectorHookContextProvider = (
  multiselectAttributes: JSXAttributes[],
  targetPath: string[],
  realValues: Array<{ [key: string]: any }>,
  computedStyles: Array<ComputedStyle>,
) => ({ children }: any) => (
  <InspectorPropsContext.Provider
    value={{
      editedMultiSelectedProps: multiselectAttributes,
      targetPath,
      realValues: realValues,
      computedStyles: computedStyles,
    }}
  >
    {children}
  </InspectorPropsContext.Provider>
)

function getBackgroundColorHookResult(
  backgroundColorExpressions: Array<string>,
  targetPath: string[],
  realInnerValues: Array<any>,
) {
  const propses = backgroundColorExpressions.map(
    (expression) => getPropsForStyleProp(expression, ['myStyleOuter', 'myStyleInner'])!,
  )
  const realValues = realInnerValues.map((realInnerValue) => {
    return targetPath.reduceRight((working, pathPart) => {
      return {
        [pathPart]: working,
      }
    }, realInnerValue)
  })

  const contextProvider = makeInspectorHookContextProvider(propses, targetPath, realValues, []) // FIXME This should be using computed styles

  const { result } = renderHook(
    () =>
      useInspectorInfo(
        backgroundLonghandPaths,
        backgroundImagesAndColorToCSSBackgroundLayerArray,
        cssBackgroundLayerArrayToBackgroundImagesAndColor,
        stylePropPathMappingFn,
      ),
    {
      wrapper: contextProvider,
    },
  )
  return result.current
}

describe('Integration Test: backgroundColor property', () => {
  it('parses a off control status', () => {
    const hookResult = getBackgroundColorHookResult([], ['myStyleOuter', 'myStyleInner'], [])

    const expectedControlStatus: ControlStatus = 'off'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('parses a simple control status', () => {
    const expectedValue: Array<CSSBackgroundLayers> = [
      [cssSolidBackgroundLayer(cssSolidColor({ ...blackHexCSSColor }))],
      [{ ...defaultLinearGradientBackgroundLayer }],
      [
        cssSolidBackgroundLayer(cssSolidColor({ ...blackHexCSSColor })),
        { ...defaultLinearGradientBackgroundLayer },
      ],
    ]

    const hookResults = [
      getBackgroundColorHookResult(
        [`{backgroundColor: "#000"}`],
        ['myStyleOuter', 'myStyleInner'],
        [{ backgroundColor: '#000' }],
      ),
      getBackgroundColorHookResult(
        [`{backgroundImage: "linear-gradient(90deg, #000 0%, #fff 100%)"}`],
        ['myStyleOuter', 'myStyleInner'],
        [{ backgroundImage: 'linear-gradient(90deg, #000 0%, #fff 100%)' }],
      ),
      getBackgroundColorHookResult(
        [
          `{backgroundImage: "linear-gradient(90deg, #000 0%, #fff 100%)", backgroundColor: "#000"}`,
        ],
        ['myStyleOuter', 'myStyleInner'],
        [
          {
            backgroundImage: 'linear-gradient(90deg, #000 0%, #fff 100%)',
            backgroundColor: '#000',
          },
        ],
      ),
    ]
    const expectedControlStatus: ControlStatus = 'simple'

    hookResults.forEach((hookResult, i) => {
      expect(hookResult.value).toEqual(expectedValue[i])
      expect(hookResult.controlStatus).toEqual(expectedControlStatus)
    })
  })

  it('parses an unset control status', () => {
    const hookResults = [
      getBackgroundColorHookResult(['{}'], ['myStyleOuter', 'myStyleInner'], [{}]),
      getBackgroundColorHookResult(
        ['{backgroundColor: undefined}'],
        ['myStyleOuter', 'myStyleInner'],
        [{ backgroundColor: undefined }],
      ),
    ]
    const expectedControlStatus: ControlStatus = 'unset'

    hookResults.forEach((hookResult, i) => {
      expect(hookResult.value).toEqual([])
      expect(hookResult.controlStatus).toEqual(expectedControlStatus)
    })
  })

  it('parses a simple-unknown-css control status', () => {
    const hookResults = [
      getBackgroundColorHookResult(
        [`{backgroundColor: 'a garbage'}`],
        ['myStyleOuter', 'myStyleInner'],
        [{ backgroundColor: 'a garbage' }],
      ),
      getBackgroundColorHookResult(
        [`{backgroundImage: 'gradient(#000 0%, #fff 100%)'}`],
        ['myStyleOuter', 'myStyleInner'],
        [{ backgroundImage: 'gradient(#000 0%, #fff 100%)' }],
      ),
      getBackgroundColorHookResult(
        [`{backgroundColor: 'a garbage', backgroundImage: 'linear-gradient(#000 0%, #fff 100%)'}`],
        ['myStyleOuter', 'myStyleInner'],
        [{ backgroundColor: 'a garbage', backgroundImage: 'linear-gradient(#000 0%, #fff 100%)' }],
      ),
    ]

    const expectedControlStatus: ControlStatus = 'simple-unknown-css'
    hookResults.forEach((hookResult) => {
      expect(hookResult.controlStatus).toEqual(expectedControlStatus)
    })
  })

  it('parses a controlled control status', () => {
    const hookResults = [
      getBackgroundColorHookResult(
        [`{backgroundColor: 5 + 15}`],
        ['myStyleOuter', 'myStyleInner'],
        // Set to something different to prevent the test from failing
        // because this doesn't parse.
        [{ backgroundColor: 'red' }],
      ),
      getBackgroundColorHookResult(
        ['{backgroundImage: `linear-gradient(#fff 0%, #000 100%)`}'],
        ['myStyleOuter', 'myStyleInner'],
        [{ backgroundImage: `linear-gradient(#fff 0%, #000 100%)` }],
      ),
      getBackgroundColorHookResult(
        [
          '{backgroundColor: "purple", backgroundImage: `linear-gradient(${"#fff"} 0%, #000 100%)`}',
        ],
        ['myStyleOuter', 'myStyleInner'],
        [
          {
            backgroundColor: 'purple',
            backgroundImage: `linear-gradient(${'#fff'} 0%, #000 100%)`,
          },
        ],
      ),
    ]

    const expectedControlStatus: ControlStatus = 'controlled'
    hookResults.forEach((hookResult) => {
      expect(hookResult.controlStatus).toEqual(expectedControlStatus)
    })
  })

  xit('parses an unoverwritable control status', () => {
    const hookResult = getBackgroundColorHookResult(
      [`nodeValue1`],
      ['myStyleOuter', 'myStyleInner'],
      [`nodeValue1`],
    )

    const expectedControlStatus: ControlStatus = 'unoverwritable'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('parses a multiselect-identical-simple control status', () => {
    const hookResult = getBackgroundColorHookResult(
      [`{backgroundColor: '#ff00ff'}`, `{backgroundColor: '#ff00ff'}`],
      ['myStyleOuter', 'myStyleInner'],
      [{ backgroundColor: '#ff00ff' }, { backgroundColor: '#ff00ff' }],
    )

    const expectedControlStatus: ControlStatus = 'multiselect-identical-simple'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('parses a multiproperty multiselect-simple-unknown-css control status', () => {
    const hookResults = [
      getBackgroundColorHookResult(
        [
          `{backgroundColor: 'a garbage'}`,
          `{backgroundColor: '#ff00ff'}`,
          `{backgroundImage: '#ff00ff'}`,
        ],
        ['myStyleOuter', 'myStyleInner'],
        [
          { backgroundColor: 'a garbage' },
          { backgroundColor: '#ff00ff' },
          { backgroundImage: '#ff00ff' },
        ],
      ),
      getBackgroundColorHookResult(
        [`{backgroundColor: 'a garbage'}`, `{backgroundColor: 'a garbage'}`],
        ['myStyleOuter', 'myStyleInner'],
        [{ backgroundColor: 'a garbage' }, { backgroundColor: 'a garbage' }],
      ),
      getBackgroundColorHookResult(
        [`{ backgroundImage: 'a garbage' }`, `{ backgroundColor: '#ff00ff' }`],
        ['myStyleOuter', 'myStyleInner'],
        [{ backgroundImage: 'a garbage' }, { backgroundColor: '#ff00ff' }],
      ),
    ]

    const expectedControlStatus: ControlStatus = 'multiselect-simple-unknown-css'
    hookResults.forEach((hookResult) => {
      expect(hookResult.controlStatus).toEqual(expectedControlStatus)
    })
  })

  it('parses a multiselect-identical-unset control status', () => {
    const hookResult = getBackgroundColorHookResult(
      [`{}`, `{}`],
      ['myStyleOuter', 'myStyleInner'],
      [{}, {}],
    )

    const expectedControlStatus: ControlStatus = 'multiselect-identical-unset'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('parses a multiselect-controlled control status', () => {
    const hookResult = getBackgroundColorHookResult(
      [`{backgroundColor: 5 + 15}`, `{backgroundColor: 5 + 15}`],
      ['myStyleOuter', 'myStyleInner'],
      // Set to something different to prevent the test from failing
      // because this doesn't parse.
      [{ backgroundColor: 'red' }, { backgroundColor: 'green' }],
    )

    const expectedControlStatus: ControlStatus = 'multiselect-controlled'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('parses a multiselect-mixed-simple-or-unset control status', () => {
    const hookResult = getBackgroundColorHookResult(
      [`{backgroundColor: "#ff00ff"}`, `{backgroundColor: "#000"}`],
      ['myStyleOuter', 'myStyleInner'],
      [{ backgroundColor: '#ff00ff' }, { backgroundColor: '#000' }],
    )

    const expectedControlStatus: ControlStatus = 'multiselect-mixed-simple-or-unset'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  xit('parses a multiselect-unoverwritable control status', () => {
    const hookResult = getBackgroundColorHookResult(
      [`nodeValue1`, `nodeValue2`],
      ['myStyleOuter', 'myStyleInner'],
      [`nodeValue1`, `nodeValue2`],
    )

    const expectedControlStatus: ControlStatus = 'multiselect-unoverwritable'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('handles the case where the value is a function call', () => {
    const hookResult = getBackgroundColorHookResult(
      [`{backgroundColor: coolBackground()}`],
      ['myStyleOuter', 'myStyleInner'],
      [{ backgroundColor: '#ff00ff' }],
    )

    expect(hookResult.value).toMatchInlineSnapshot(`
      Array [
        Object {
          "color": Object {
            "hex": "#ff00ff",
            "type": "Hex",
          },
          "enabled": true,
          "type": "solid-background-layer",
        },
      ]
    `)
  })
})

describe('Integration Test: opacity property', () => {
  function getOpacityHookResult(
    opacityExpressions: Array<string>,
    realValues: Array<any>,
    computedStyles: Array<ComputedStyle>,
  ) {
    const propses = opacityExpressions.map(
      (expression) => getPropsForStyleProp(expression, ['myStyleOuter', 'myStyleInner'])!,
    )

    const contextProvider = ({ children }: any) => (
      <InspectorPropsContext.Provider
        value={{
          editedMultiSelectedProps: propses,
          targetPath: ['myStyleOuter', 'myStyleInner'],
          realValues: realValues,
          computedStyles: computedStyles,
        }}
      >
        {children}
      </InspectorPropsContext.Provider>
    )

    const { result } = renderHook(() => useInspectorStyleInfo('opacity'), {
      wrapper: contextProvider,
    })
    return result.current
  }

  it('parses a off control status', () => {
    const hookResult = getOpacityHookResult([], [], [])

    const expectedControlStatus: ControlStatus = 'off'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('parses an unset control status', () => {
    const hookResult = getOpacityHookResult([`{}`], [{}], [])
    const expectedControlStatus: ControlStatus = 'unset'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('parses a multiselect-identical-unset control status', () => {
    const hookResult = getOpacityHookResult([`{}`, `{}`], [{}, {}], [])

    const expectedControlStatus: ControlStatus = 'multiselect-identical-unset'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('parses a simple control status', () => {
    const expectedValue = cssNumber(0.9)

    const hookResult = getOpacityHookResult(
      [`{opacity: 0.9}`],
      [{ opacity: 0.9 }],
      [{ opacity: '0.9' }],
    )

    expect(hookResult.value).toEqual(expectedValue)

    const expectedControlStatus: ControlStatus = 'simple'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('parses a simple-unknown-css control status', () => {
    const hookResult = getOpacityHookResult(
      [`{opacity: 'a garbage'}`],
      [{ opacity: 'a garbage' }],
      [{ opacity: 'a garbage' }],
    )

    const expectedControlStatus: ControlStatus = 'simple-unknown-css'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('parses a multiselect-identical-simple control status', () => {
    const expectedValue = cssNumber(0.9)

    const hookResult = getOpacityHookResult(
      [`{opacity: 0.9}`, `{opacity: 0.9}`],
      [{ opacity: 0.9 }, { opacity: 0.9 }],
      [{ opacity: '0.9' }, { opacity: '0.9' }],
    )

    expect(hookResult.value).toEqual(expectedValue)

    const expectedControlStatus: ControlStatus = 'multiselect-identical-simple'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('parses a multiselect-simple-unknown-css control status', () => {
    const hookResults = [
      getOpacityHookResult(
        [`{opacity: 'a garbage'}`, `{opacity: 0.9}`],
        [{ opacity: 'a garbage' }, { opacity: 0.9 }],
        [{ opacity: 'a garbage' }, { opacity: '0.9' }],
      ),
      getOpacityHookResult(
        [`{opacity: 0.9}`, `{opacity: 'a garbage'}`],
        [{ opacity: 0.9 }, { opacity: 'a garbage' }],
        [{ opacity: '0.9' }, { opacity: 'a garbage' }],
      ),
      getOpacityHookResult(
        [`{opacity: 1}`, `{opacity: 0.9}`, `{opacity: 'a garbage'}`],
        [{ opacity: 1 }, { opacity: 0.9 }, { opacity: 'a garbage' }],
        [{ opacity: '1' }, { opacity: '0.9' }, { opacity: 'a garbage' }],
      ),
    ]

    const expectedControlStatus: ControlStatus = 'multiselect-simple-unknown-css'
    hookResults.forEach((hookResult) => {
      expect(hookResult.controlStatus).toEqual(expectedControlStatus)
    })
  })

  it('parses a multiselect-mixed-simple-or-unset control status', () => {
    const expectedValue = cssNumber(0.9)

    const hookResult = getOpacityHookResult(
      [`{opacity: 0.9}`, `{opacity: 0.5}`],
      [{ opacity: 0.9 }, { opacity: 0.5 }],
      [{ opacity: '0.9' }, { opacity: '0.5' }],
    )

    expect(hookResult.value).toEqual(expectedValue)

    const expectedControlStatus: ControlStatus = 'multiselect-mixed-simple-or-unset'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('parses a controlled control status', () => {
    const hookResult = getOpacityHookResult(
      [`{opacity: true ? 1 : 0.1}`],
      [{ opacity: 1 }],
      [{ opacity: '1' }],
    )
    const expectedControlStatus: ControlStatus = 'controlled'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('parses a multiselect-controlled control status', () => {
    const hookResult = getOpacityHookResult(
      [`{opacity: true ? 1 : 0.1}`, `{opacity: true ? 1 : 0.1}`],
      [{ opacity: 1 }, { opacity: 1 }],
      [{ opacity: '1' }, { opacity: '1' }],
    )
    const expectedControlStatus: ControlStatus = 'multiselect-controlled'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  xit('parses an unoverwritable control status', () => {
    const hookResult = getOpacityHookResult([`nodeValue1`], [`nodeValue1`], [])
    const expectedControlStatus: ControlStatus = 'unoverwritable'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  xit('parses a multiselect-unoverwritable control status', () => {
    const hookResult = getOpacityHookResult(
      [`nodeValue1`, `nodeValue1`],
      [`nodeValue1`, `nodeValue1`],
      [],
    )
    const expectedControlStatus: ControlStatus = 'multiselect-unoverwritable'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  xit('parses a multiselect-unoverwritable control status', () => {
    const hookResult = getOpacityHookResult(
      [`nodeValue1`, `nodeValue2`],
      [`nodeValue1`, `nodeValue2`],
      [],
    )
    const expectedControlStatus: ControlStatus = 'multiselect-unoverwritable'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })
})

describe('Integration Test: boxShadow property', () => {
  function getBoxShadowHookResult(
    boxShadowExpressions: Array<string>,
    realValues: Array<any>,
    computedStyles: Array<ComputedStyle>,
  ) {
    const props = boxShadowExpressions.map(
      (boxShadow) => getPropsForStyleProp(boxShadow, ['myStyleOuter', 'myStyleInner'])!,
    )

    const contextProvider = makeInspectorHookContextProvider(
      props,
      ['myStyleOuter', 'myStyleInner'],
      realValues,
      computedStyles,
    )

    const { result } = renderHook(() => useInspectorStyleInfo('boxShadow'), {
      wrapper: contextProvider,
    })
    return result.current
  }

  it('poorly formed shows up as unknown', () => {
    const hookResult = getBoxShadowHookResult(
      [`{ boxShadow: '1px 1px burple' }`],
      [{ boxShadow: '1px 1px burple' }],
      [{ boxShadow: '1px 1px burple' }],
    )
    const expectedControlStatus: ControlStatus = 'simple-unknown-css'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('multiselected poorly formed shows up as unknown', () => {
    const hookResult = getBoxShadowHookResult(
      [
        `{ boxShadow: '1px 1px burple' }`,
        `{ boxShadow: '1px 1px purple' }`,
        `{ boxShadow: '1px 1px beeple' }`,
        `{ boxShadow: '1px 1px boople' }`,
      ],
      [
        { boxShadow: '1px 1px burple' },
        { boxShadow: '1px 1px purple' },
        { boxShadow: '1px 1px beeple' },
        { boxShadow: '1px 1px boople' },
      ],
      [
        { boxShadow: '1px 1px burple' },
        { boxShadow: '1px 1px purple' },
        { boxShadow: '1px 1px beeple' },
        { boxShadow: '1px 1px boople' },
      ],
    )
    const expectedControlStatus: ControlStatus = 'multiselect-simple-unknown-css'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('shows up as simple', () => {
    const hookResult = getBoxShadowHookResult(
      [`{ boxShadow: '0 0 0 1px #ff00ff' }`],
      [{ boxShadow: '0 0 0 1px #ff00ff' }],
      [{ boxShadow: '0 0 0 1px #ff00ff' }],
    )
    const expectedControlStatus: ControlStatus = 'simple'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('with a controlled parameter shows up as controlled', () => {
    const hookResult = getBoxShadowHookResult(
      [`{ boxShadow: 5 + 15 }`],
      [{ boxShadow: '20' }],
      [{ boxShadow: '20' }],
    )
    const expectedControlStatus: ControlStatus = 'controlled'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('multiselect, with a controlled parameter shows up as controlled', () => {
    const hookResult = getBoxShadowHookResult(
      [`{ boxShadow: 5 + 15 }`, `{ boxShadow: 5 + 15 }`],
      [{ boxShadow: '20' }, { boxShadow: '20' }],
      [{ boxShadow: '20' }, { boxShadow: '20' }],
    )
    const expectedControlStatus: ControlStatus = 'multiselect-controlled'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('multiselect with a mixed value, with a controlled parameter shows up as controlled', () => {
    const hookResult = getBoxShadowHookResult(
      [`{ boxShadow: 5 + 15 }`, `{ boxShadow: 5 + 25 }`],
      [{ boxShadow: '20' }, { boxShadow: '30' }],
      [{ boxShadow: '20' }, { boxShadow: '30' }],
    )
    const expectedControlStatus: ControlStatus = 'multiselect-controlled'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })
})
