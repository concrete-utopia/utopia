import { render, renderHook } from '@testing-library/react'
import React from 'react'

jest.mock('../../editor/store/store-hook', () => ({
  useRefEditorState: jest.fn(() => ({ current: jest.fn(() => {}) })),
}))

import { isRight } from '../../../core/shared/either'
import type {
  JSXAttributes,
  ComputedStyle,
  StyleAttributeMetadata,
} from '../../../core/shared/element-template'
import {
  isJSXElement,
  isUtopiaJSXComponent,
  jsExpressionValue,
  jsxAttributesFromMap,
  emptyComments,
} from '../../../core/shared/element-template'
import { CanvasMetadataName } from '../../../core/workers/parser-printer/parser-printer-parsing'
import { testParseCode } from '../../../core/workers/parser-printer/parser-printer.test-utils'
import {
  enableWhyDidYouRenderOnComponent,
  setupReactWhyDidYouRender,
} from '../../../utils/react-memoize.test-utils'
import utils from '../../../utils/utils'
import {
  backgroundImagesAndColorToCSSBackgroundLayerArray,
  cssBackgroundLayerArrayToBackgroundImagesAndColor,
  backgroundLonghandPaths,
} from '../sections/style-section/background-subsection/background-subsection'
import type { ControlStatus } from './control-status'
import type { CSSBackgroundLayers, ParsedCSSPropertiesKeys } from './css-utils'
import {
  blackHexCSSColor,
  cssNumber,
  CSSSolidColor,
  cssSolidColor,
  defaultLinearGradientBackgroundLayer,
  printCSSNumber,
  cssSolidBackgroundLayer,
  ParsedPropertiesKeys,
} from './css-utils'
import type { InspectorCallbackContextData, InspectorPropsContextData } from './property-path-hooks'
import {
  InspectorCallbackContext,
  InspectorPropsContext,
  stylePropPathMappingFn,
  useCallbackFactory,
  useGetOrderedPropertyKeys,
  useInspectorInfo,
  useInspectorStyleInfo,
} from './property-path-hooks'
import { isParseSuccess, ElementPath } from '../../../core/shared/project-file-types'
import {
  getPropsForStyleProp,
  makeInspectorHookContextProvider,
} from './property-path-hooks.test-utils'
import { DispatchContext } from '../../editor/store/dispatch-context'
import { NO_OP } from '../../../core/shared/utils'
import { styleStringInArray } from '../../../utils/common-constants'

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
  const [onSubmitValue2] =
    props.transformFunction2 != null ? factory(props.transformFunction2) : [null]

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
})

const WellBehavedInspectorSubsection = React.memo(() => {
  const { value, onSubmitValue } = useInspectorStyleInfo('opacity')
  onSubmitValue(cssNumber(0.9))
  return <div onClick={() => onSubmitValue(cssNumber(0.5))}>{printCSSNumber(value, null)}</div>
})
enableWhyDidYouRenderOnComponent(WellBehavedInspectorSubsection)

const InspectorSectionProvider = (props: {
  propsData: InspectorPropsContextData
  callbackData: InspectorCallbackContextData
}) => {
  return (
    <DispatchContext.Provider value={NO_OP}>
      <InspectorCallbackContext.Provider value={props.callbackData}>
        <InspectorPropsContext.Provider value={props.propsData}>
          <WellBehavedInspectorSubsection />
        </InspectorPropsContext.Provider>
      </InspectorCallbackContext.Provider>
    </DispatchContext.Provider>
  )
}

describe('useInspectorMetadataForPropsObject memoization', () => {
  const callbackData = {
    onSubmitValue: utils.NO_OP,
    onUnsetValue: utils.NO_OP,
    collectActionsToSubmitValue: () => [],
    collectActionsToUnsetValue: () => [],
    selectedViewsRef: { current: [] },
  }

  it('make sure the tested component is in testing mode', () => {
    expect((WellBehavedInspectorSubsection as any).whyDidYouRender).toBeTruthy()
  })

  it('this hook wont cause rerender if the context is exactly the same', () => {
    const [getUpdateCount] = setupReactWhyDidYouRender()
    const propsWithOpacity: JSXAttributes[] = [
      jsxAttributesFromMap({
        style: jsExpressionValue({ opacity: cssNumber(0.9) }, emptyComments),
      }),
    ]
    const spiedProps: Array<{ [key: string]: any }> = [
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
          selectedViews: [],
          editedMultiSelectedProps: propsWithOpacity,
          targetPath: ['myStyleOuter', 'myStyleInner'],
          spiedProps: spiedProps,
          computedStyles: computedStyles,
          selectedAttributeMetadatas: [],
        }}
        callbackData={callbackData}
      />,
    )
    rerender(
      <InspectorSectionProvider
        propsData={{
          selectedViews: [],
          editedMultiSelectedProps: propsWithOpacity,
          targetPath: ['myStyleOuter', 'myStyleInner'],
          spiedProps: spiedProps,
          computedStyles: computedStyles,
          selectedAttributeMetadatas: [],
        }}
        callbackData={callbackData}
      />,
    )
    expect(getUpdateCount()).toEqual(0)
  })
  it('this hook wont cause rerender if the single selected JSXAttributes stays the same', () => {
    const [getUpdateCount] = setupReactWhyDidYouRender()
    const propsWithOpacity: JSXAttributes = jsxAttributesFromMap({
      style: jsExpressionValue({ opacity: 0.9 }, emptyComments),
    })
    const spiedProps: Array<{ [key: string]: any }> = [
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
          selectedViews: [],
          editedMultiSelectedProps: [propsWithOpacity],
          targetPath: ['myStyleOuter', 'myStyleInner'],
          spiedProps: spiedProps,
          computedStyles: computedStyles,
          selectedAttributeMetadatas: [],
        }}
        callbackData={callbackData}
      />,
    )
    rerender(
      <InspectorSectionProvider
        propsData={{
          selectedViews: [],
          editedMultiSelectedProps: [propsWithOpacity],
          targetPath: ['myStyleOuter', 'myStyleInner'],
          spiedProps: spiedProps,
          computedStyles: computedStyles,
          selectedAttributeMetadatas: [],
        }}
        callbackData={callbackData}
      />,
    )
    expect(getUpdateCount()).toEqual(0)
  })

  it('if props change, but not the prop we care about, skip rerender', () => {
    const [getUpdateCount] = setupReactWhyDidYouRender()
    const opacityProp = jsExpressionValue(0.9, emptyComments)
    const propsWithOpacity: JSXAttributes = jsxAttributesFromMap({
      // FIXME: This nests `jsxAttributeValue` inside a `jsxAttributeValue`.
      style: jsExpressionValue({ opacity: opacityProp, otherProp: 'dontcare' }, emptyComments),
    })
    const propsChangedOpacitySame: JSXAttributes = jsxAttributesFromMap({
      // FIXME: This nests `jsxAttributeValue` inside a `jsxAttributeValue`.
      style: jsExpressionValue({ opacity: opacityProp, otherProp: 'imdifferent' }, emptyComments),
    })
    const spiedProps: Array<{ [key: string]: any }> = [
      {
        style: { opacity: 0.9, otherProp: 'dontcare' },
      },
    ]
    const spiedPropsChanged: Array<{ [key: string]: any }> = [
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
          selectedViews: [],
          editedMultiSelectedProps: [propsWithOpacity],
          targetPath: ['myStyleOuter', 'myStyleInner'],
          spiedProps: spiedProps,
          computedStyles: computedStyles,
          selectedAttributeMetadatas: [],
        }}
        callbackData={callbackData}
      />,
    )
    rerender(
      <InspectorSectionProvider
        propsData={{
          selectedViews: [],
          editedMultiSelectedProps: [propsChangedOpacitySame],
          targetPath: ['myStyleOuter', 'myStyleInner'],
          spiedProps: spiedPropsChanged,
          computedStyles: computedStylesChanged,
          selectedAttributeMetadatas: [],
        }}
        callbackData={callbackData}
      />,
    )
    expect(getUpdateCount()).toEqual(0)
  })

  it('if the relevant prop changes, do rerender', () => {
    const [getUpdateCount] = setupReactWhyDidYouRender()
    const opacityProp = 0.9
    const propsWithOpacity: JSXAttributes = jsxAttributesFromMap({
      style: jsExpressionValue({ opacity: opacityProp, otherProp: 'dontcare' }, emptyComments),
    })
    const propsWithOpacityChanged: JSXAttributes = jsxAttributesFromMap({
      style: jsExpressionValue({ opacity: 0.5, otherProp: 'imdifferent' }, emptyComments),
    })
    const spiedProps: Array<{ [key: string]: any }> = [
      {
        style: { opacity: 0.9, otherProp: 'dontcare' },
      },
    ]
    const spiedPropsChanged: Array<{ [key: string]: any }> = [
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
          selectedViews: [],
          editedMultiSelectedProps: [propsWithOpacity],
          targetPath: styleStringInArray,
          spiedProps: spiedProps,
          computedStyles: computedStyles,
          selectedAttributeMetadatas: [],
        }}
        callbackData={callbackData}
      />,
    )
    const renderedElement = getByText('0.9')
    expect(renderedElement).toBeDefined()
    rerender(
      <InspectorSectionProvider
        propsData={{
          selectedViews: [],
          editedMultiSelectedProps: [propsWithOpacityChanged],
          targetPath: styleStringInArray,
          spiedProps: spiedPropsChanged,
          computedStyles: computedStylesChanged,
          selectedAttributeMetadatas: [],
        }}
        callbackData={callbackData}
      />,
    )
    const reRenderedElement = getByText('0.5')
    expect(reRenderedElement).toBeDefined()
    expect(getUpdateCount()).toBeGreaterThan(0)
  })
})

function getBackgroundColorHookResult(
  backgroundColorExpressions: Array<string>,
  targetPath: string[],
  spiedProps: Array<any>,
) {
  const propses = backgroundColorExpressions.map(
    (expression) => getPropsForStyleProp(expression, ['myStyleOuter', 'myStyleInner'])!,
  )

  const contextProvider = makeInspectorHookContextProvider(
    [],
    propses,
    targetPath,
    spiedProps,
    [],
    [],
  ) // FIXME This should be using computed styles

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

    const expectedControlStatus: ControlStatus = 'unset'
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
    spiedProps: Array<any>,
    computedStyles: Array<ComputedStyle>,
    attributeMetadatas: Array<StyleAttributeMetadata>,
  ) {
    const propses = opacityExpressions.map(
      (expression) => getPropsForStyleProp(expression, ['myStyleOuter', 'myStyleInner'])!,
    )

    const contextProvider = ({ children }: any) => (
      <InspectorPropsContext.Provider
        value={{
          selectedViews: [],
          editedMultiSelectedProps: propses,
          targetPath: ['myStyleOuter', 'myStyleInner'],
          spiedProps: spiedProps,
          computedStyles: computedStyles,
          selectedAttributeMetadatas: attributeMetadatas,
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
    const hookResult = getOpacityHookResult([], [], [], [])

    const expectedControlStatus: ControlStatus = 'unset'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('parses an unset control status', () => {
    const hookResult = getOpacityHookResult([`{}`], [{}], [], [])
    const expectedControlStatus: ControlStatus = 'unset'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('parses a multiselect-identical-unset control status', () => {
    const hookResult = getOpacityHookResult([`{}`, `{}`], [{}, {}], [], [])

    const expectedControlStatus: ControlStatus = 'multiselect-identical-unset'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('parses a simple control status', () => {
    const expectedValue = cssNumber(0.9)

    const hookResult = getOpacityHookResult(
      [`{opacity: 0.9}`],
      [{ opacity: 0.9 }],
      [{ opacity: '0.9' }],
      [],
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
      [],
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
      [],
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
        [],
      ),
      getOpacityHookResult(
        [`{opacity: 0.9}`, `{opacity: 'a garbage'}`],
        [{ opacity: 0.9 }, { opacity: 'a garbage' }],
        [{ opacity: '0.9' }, { opacity: 'a garbage' }],
        [],
      ),
      getOpacityHookResult(
        [`{opacity: 1}`, `{opacity: 0.9}`, `{opacity: 'a garbage'}`],
        [{ opacity: 1 }, { opacity: 0.9 }, { opacity: 'a garbage' }],
        [{ opacity: '1' }, { opacity: '0.9' }, { opacity: 'a garbage' }],
        [],
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
      [],
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
      [],
    )
    const expectedControlStatus: ControlStatus = 'controlled'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('parses a multiselect-controlled control status', () => {
    const hookResult = getOpacityHookResult(
      [`{opacity: true ? 1 : 0.1}`, `{opacity: true ? 1 : 0.1}`],
      [{ opacity: 1 }, { opacity: 1 }],
      [{ opacity: '1' }, { opacity: '1' }],
      [],
    )
    const expectedControlStatus: ControlStatus = 'multiselect-controlled'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  xit('parses an unoverwritable control status', () => {
    const hookResult = getOpacityHookResult([`nodeValue1`], [`nodeValue1`], [], [])
    const expectedControlStatus: ControlStatus = 'unoverwritable'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  xit('parses a multiselect-unoverwritable control status', () => {
    const hookResult = getOpacityHookResult(
      [`nodeValue1`, `nodeValue1`],
      [`nodeValue1`, `nodeValue1`],
      [],
      [],
    )
    const expectedControlStatus: ControlStatus = 'multiselect-unoverwritable'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  xit('parses a multiselect-unoverwritable control status 2', () => {
    const hookResult = getOpacityHookResult(
      [`nodeValue1`, `nodeValue2`],
      [`nodeValue1`, `nodeValue2`],
      [],
      [],
    )
    const expectedControlStatus: ControlStatus = 'multiselect-unoverwritable'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })
})

describe('Integration Test: boxShadow property', () => {
  function getBoxShadowHookResult(
    boxShadowExpressions: Array<string>,
    spiedProps: Array<any>,
    computedStyles: Array<ComputedStyle>,
    attributeMetadatas: Array<StyleAttributeMetadata>,
  ) {
    const props = boxShadowExpressions.map(
      (boxShadow) => getPropsForStyleProp(boxShadow, ['myStyleOuter', 'myStyleInner'])!,
    )

    const contextProvider = makeInspectorHookContextProvider(
      [],
      props,
      ['myStyleOuter', 'myStyleInner'],
      spiedProps,
      computedStyles,
      attributeMetadatas,
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
      [],
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
      [],
    )
    const expectedControlStatus: ControlStatus = 'multiselect-simple-unknown-css'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('shows up as simple', () => {
    const hookResult = getBoxShadowHookResult(
      [`{ boxShadow: '0 0 0 1px #ff00ff' }`],
      [{ boxShadow: '0 0 0 1px #ff00ff' }],
      [{ boxShadow: '0 0 0 1px #ff00ff' }],
      [],
    )
    const expectedControlStatus: ControlStatus = 'simple'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('with a controlled parameter shows up as controlled', () => {
    const hookResult = getBoxShadowHookResult(
      [`{ boxShadow: 5 + 15 }`],
      [{ boxShadow: '20' }],
      [{ boxShadow: '20' }],
      [],
    )
    const expectedControlStatus: ControlStatus = 'controlled'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('multiselect, with a controlled parameter shows up as controlled', () => {
    const hookResult = getBoxShadowHookResult(
      [`{ boxShadow: 5 + 15 }`, `{ boxShadow: 5 + 15 }`],
      [{ boxShadow: '20' }, { boxShadow: '20' }],
      [{ boxShadow: '20' }, { boxShadow: '20' }],
      [],
    )
    const expectedControlStatus: ControlStatus = 'multiselect-controlled'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })

  it('multiselect with a mixed value, with a controlled parameter shows up as controlled', () => {
    const hookResult = getBoxShadowHookResult(
      [`{ boxShadow: 5 + 15 }`, `{ boxShadow: 5 + 25 }`],
      [{ boxShadow: '20' }, { boxShadow: '30' }],
      [{ boxShadow: '20' }, { boxShadow: '30' }],
      [],
    )
    const expectedControlStatus: ControlStatus = 'multiselect-controlled'
    expect(hookResult.controlStatus).toEqual(expectedControlStatus)
  })
})

describe('useGetOrderedPropertyKeys', () => {
  function getPaddingHookResult<P extends ParsedCSSPropertiesKeys>(
    propsKeys: Array<P>,
    styleObjectExpressions: Array<string>,
    spiedProps: Array<any>,
    computedStyles: Array<ComputedStyle>,
    attributeMetadatas: Array<StyleAttributeMetadata>,
  ) {
    const props = styleObjectExpressions.map(
      (styleExpression) => getPropsForStyleProp(styleExpression, styleStringInArray)!,
    )

    const contextProvider = makeInspectorHookContextProvider(
      [],
      props,
      styleStringInArray,
      spiedProps,
      computedStyles,
      attributeMetadatas,
    )

    const { result } = renderHook(
      () => useGetOrderedPropertyKeys<P>(stylePropPathMappingFn, propsKeys),
      {
        wrapper: contextProvider,
      },
    )
    return result.current
  }

  it('does not contain entry for nonexistent prop 1', () => {
    const hookResult = getPaddingHookResult(
      ['paddingLeft', 'padding'],
      [`{ paddingLeft: 5 }`],
      [{ paddingLeft: 5 }],
      [{ paddingTop: '0px', paddingRight: '0px', paddingBottom: '0px', paddingLeft: '15px' }],
      [],
    )
    expect(hookResult).toEqual([['paddingLeft']])
  })

  it('does not contain entry for nonexistent prop 2', () => {
    const hookResult = getPaddingHookResult(
      ['paddingLeft', 'padding'],
      [`{ padding: 15 }`],
      [{ padding: 15 }],
      [{ paddingTop: '15px', paddingRight: '15px', paddingBottom: '15px', paddingLeft: '15px' }],
      [],
    )
    expect(hookResult).toEqual([['padding']])
  })

  it('does contain entry for prop explicitly set to undefined', () => {
    const hookResult = getPaddingHookResult(
      ['paddingLeft', 'padding'],
      [`{ padding: 15, paddingLeft: undefined }`],
      [{ padding: 15, paddingLeft: undefined }],
      [{ paddingTop: '15px', paddingRight: '15px', paddingBottom: '15px', paddingLeft: '15px' }],
      [],
    )
    expect(hookResult).toEqual([['padding', 'paddingLeft']])
  })

  it('keeps the order of props for single select 1', () => {
    const hookResult = getPaddingHookResult(
      ['paddingLeft', 'padding'],
      [`{ paddingLeft: 5, padding: 15 }`],
      [{ paddingLeft: 5, padding: 15 }],
      [{ paddingTop: '15px', paddingRight: '15px', paddingBottom: '15px', paddingLeft: '15px' }],
      [],
    )
    expect(hookResult).toEqual([['paddingLeft', 'padding']])
  })

  it('keeps the order of props for single select 2', () => {
    const hookResult = getPaddingHookResult(
      ['paddingLeft', 'padding'],
      [`{ padding: 15, paddingLeft: 5 }`],
      [{ padding: 15, paddingLeft: 5 }],
      [{ paddingTop: '15px', paddingRight: '15px', paddingBottom: '15px', paddingLeft: '5px' }],
      [],
    )
    expect(hookResult).toEqual([['padding', 'paddingLeft']])
  })

  it('works with controlled longhand', () => {
    const hookResult = getPaddingHookResult(
      ['paddingLeft', 'padding'],
      [`{ paddingLeft: 5 + 5 }`],
      [{ paddingLeft: 10 }],
      [{ paddingLeft: '10px' }],
      [],
    )
    expect(hookResult).toEqual([['paddingLeft']])
  })

  it('keeps the order of props for multi select 1', () => {
    const hookResult = getPaddingHookResult(
      ['paddingLeft', 'padding'],
      [`{ padding: 15 }`, `{ padding: 15 }`],
      [{ padding: 15 }, { padding: 15 }],
      [
        { paddingTop: '15px', paddingRight: '15px', paddingBottom: '15px', paddingLeft: '15px' },
        { paddingTop: '15px', paddingRight: '15px', paddingBottom: '15px', paddingLeft: '15px' },
      ],
      [],
    )
    expect(hookResult).toEqual([['padding'], ['padding']])
  })

  it('keeps the order of props for multi select 2', () => {
    const hookResult = getPaddingHookResult(
      ['paddingLeft', 'padding'],
      [`{ paddingLeft: 5, padding: 15 }`, `{ paddingLeft: 5, padding: 15 }`],
      [
        { paddingLeft: 5, padding: 15 },
        { paddingLeft: 5, padding: 15 },
      ],
      [
        { paddingTop: '15px', paddingRight: '15px', paddingBottom: '15px', paddingLeft: '15px' },
        { paddingTop: '15px', paddingRight: '15px', paddingBottom: '15px', paddingLeft: '15px' },
      ],
      [],
    )

    expect(hookResult).toEqual([
      ['paddingLeft', 'padding'],
      ['paddingLeft', 'padding'],
    ])
  })

  it('multiselect: the paddings are in different order 1', () => {
    const hookResult = getPaddingHookResult(
      ['paddingLeft', 'padding'],
      [`{ paddingLeft: 5, padding: 15 }`, `{ padding: 15 }`],
      [{ paddingLeft: 5, padding: 15 }, { padding: 15 }],
      [
        { paddingTop: '15px', paddingRight: '15px', paddingBottom: '15px', paddingLeft: '15px' },
        { paddingTop: '15px', paddingRight: '15px', paddingBottom: '15px', paddingLeft: '15px' },
      ],
      [],
    )

    expect(hookResult).toEqual([['paddingLeft', 'padding'], ['padding']])
  })

  it('multiselect: the paddings are in different order 2', () => {
    const hookResult = getPaddingHookResult(
      ['paddingLeft', 'padding'],
      [`{ padding: 15, paddingLeft: 5 }`, `{ padding: 15 }`],
      [{ padding: 15, paddingLeft: 5 }, { padding: 15 }],
      [
        { paddingTop: '15px', paddingRight: '15px', paddingBottom: '15px', paddingLeft: '5px' },
        { paddingTop: '15px', paddingRight: '15px', paddingBottom: '15px', paddingLeft: '15px' },
      ],
      [],
    )
    expect(hookResult).toEqual([['padding', 'paddingLeft'], ['padding']])
  })
})
