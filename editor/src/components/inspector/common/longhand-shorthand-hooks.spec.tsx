import * as React from 'react'
import { renderHook } from '@testing-library/react-hooks'
import {
  ComputedStyle,
  jsxAttributeValue,
  StyleAttributeMetadata,
} from '../../../core/shared/element-template'
import { cssNumber, ParsedPropertiesKeys } from './css-utils'
import { useInspectorInfoLonghandShorthand } from './longhand-shorthand-hooks'
import { stylePropPathMappingFn } from './property-path-hooks'
import {
  getPropsForStyleProp,
  makeInspectorHookContextProvider,
} from './property-path-hooks.test-utils'
import { EditorStore } from '../../editor/store/editor-state'
import create from 'zustand'
import { EditorStateContext } from '../../editor/store/store-hook'
import * as TP from '../../../core/shared/template-path'
import * as PP from '../../../core/shared/property-path'
import { setProp_UNSAFE, unsetProperty } from '../../editor/actions/action-creators'
import { emptyComments } from '../../../core/workers/parser-printer/parser-printer-comments'

const TestSelectedComponent = TP.instancePath(['scene1'], ['aaa', 'bbb'])

function getPaddingHookResult<P extends ParsedPropertiesKeys>(
  longhand: P,
  shorthand: P,
  styleObjectExpressions: Array<string>,
  spiedProps: Array<any>,
  computedStyles: Array<ComputedStyle>,
  attributeMetadatas: Array<StyleAttributeMetadata>,
) {
  const props = styleObjectExpressions.map(
    (styleExpression) => getPropsForStyleProp(styleExpression, ['style'])!,
  )

  const mockDispatch = jest.fn()

  // eslint-disable-next-line @typescript-eslint/ban-types
  const contextProvider: React.FunctionComponent<{}> = ({ children }) => {
    const InspectorContextProvider = makeInspectorHookContextProvider(
      [TestSelectedComponent],
      props,
      ['style'],
      spiedProps,
      computedStyles,
      attributeMetadatas,
    )

    const initialEditorStore: EditorStore = {
      editor: null as any,
      derived: null as any,
      history: null as any,
      userState: null as any,
      workers: null as any,
      dispatch: mockDispatch,
    }

    const storeHook = create<EditorStore>(() => initialEditorStore)

    return (
      <EditorStateContext.Provider value={{ api: storeHook, useStore: storeHook }}>
        <InspectorContextProvider>{children}</InspectorContextProvider>
      </EditorStateContext.Provider>
    )
  }

  const { result } = renderHook(
    () =>
      useInspectorInfoLonghandShorthand(
        longhand,
        shorthand,
        stylePropPathMappingFn as any, // ¯\_(ツ)_/¯
      ),
    {
      wrapper: contextProvider,
    },
  )
  return { hookResult: result.current, mockDispatch: mockDispatch }
}

describe('useInspectorInfo: reading padding shorthand and longhands', () => {
  it('paddingLeft', () => {
    const { hookResult } = getPaddingHookResult(
      'paddingLeft',
      'padding',
      [`{ paddingLeft: 5 }`],
      [{ paddingLeft: 5 }],
      [{ paddingTop: '0px', paddingRight: '0px', paddingBottom: '0px', paddingLeft: '5px' }],
      [],
    )
    expect(hookResult.value).toEqual({ unit: null, value: 5 })
    expect(hookResult.orderedPropKeys).toEqual([['paddingLeft']])
  })

  it('paddingLeft, padding', () => {
    const { hookResult } = getPaddingHookResult(
      'paddingLeft',
      'padding',
      [`{ paddingLeft: 5, padding: 15 }`],
      [{ paddingLeft: 5, padding: 15 }],
      [{ paddingTop: '15px', paddingRight: '15px', paddingBottom: '15px', paddingLeft: '15px' }],
      [],
    )
    expect(hookResult.value).toEqual({ unit: 'px', value: 15 })
    expect(hookResult.orderedPropKeys).toEqual([['paddingLeft', 'padding']])
  })

  it('padding, paddingLeft', () => {
    const { hookResult } = getPaddingHookResult(
      'paddingLeft',
      'padding',
      [`{ padding: 15, paddingLeft: 5 }`],
      [{ padding: 15, paddingLeft: 5 }],
      [{ paddingTop: '15px', paddingRight: '15px', paddingBottom: '15px', paddingLeft: '5px' }],
      [],
    )
    expect(hookResult.value).toEqual({ unit: null, value: 5 })
    expect(hookResult.orderedPropKeys).toEqual([['padding', 'paddingLeft']])
  })

  it('paddingLeft, padding, paddingRight', () => {
    const { hookResult } = getPaddingHookResult(
      'paddingLeft',
      'padding',
      [`{ paddingLeft: 5, padding: 15, paddingRight: 20 }`], // paddingRight is irrelevant, we are making sure it doesn't affect anything
      [{ paddingLeft: 5, padding: 15, paddingRight: 20 }],
      [{ paddingTop: '15px', paddingRight: '20px', paddingBottom: '15px', paddingLeft: '15px' }],
      [],
    )
    expect(hookResult.value).toEqual({ unit: 'px', value: 15 })
    expect(hookResult.orderedPropKeys).toEqual([['paddingLeft', 'padding']])
  })

  it('paddingLeft controlled', () => {
    const { hookResult } = getPaddingHookResult(
      'paddingLeft',
      'padding',
      [`{ paddingLeft: 5 + 5 }`],
      [{ paddingLeft: 10 }],
      [{ paddingTop: '0px', paddingRight: '0px', paddingBottom: '0px', paddingLeft: '10px' }],
      [],
    )
    expect(hookResult.value).toEqual({ unit: 'px', value: 10 })
    expect(hookResult.orderedPropKeys).toEqual([['paddingLeft']])
    expect(hookResult.controlStatus).toEqual('controlled')
  })

  it('paddingLeft, padding controlled', () => {
    const { hookResult } = getPaddingHookResult(
      'paddingLeft',
      'padding',
      [`{ paddingLeft: 5, padding: 15 + 5 }`],
      [{ paddingLeft: 5, padding: 20 }],
      [{ paddingTop: '20px', paddingRight: '20px', paddingBottom: '20px', paddingLeft: '20px' }],
      [],
    )
    expect(hookResult.value).toEqual({ unit: 'px', value: 20 })
    expect(hookResult.orderedPropKeys).toEqual([['paddingLeft', 'padding']])
    expect(hookResult.controlStatus).toEqual('controlled')
  })

  it('padding controlled, paddingLeft', () => {
    const { hookResult } = getPaddingHookResult(
      'paddingLeft',
      'padding',
      [`{ padding: 15 + 5, paddingLeft: 5 }`],
      [{ padding: 20, paddingLeft: 5 }],
      [{ paddingTop: '20px', paddingRight: '20px', paddingBottom: '20px', paddingLeft: '5px' }],
      [],
    )
    expect(hookResult.value).toEqual({ unit: null, value: 5 })
    expect(hookResult.orderedPropKeys).toEqual([['padding', 'paddingLeft']])
    expect(hookResult.controlStatus).toEqual('simple')
  })

  it('paddingLeft controlled, padding', () => {
    const { hookResult } = getPaddingHookResult(
      'paddingLeft',
      'padding',
      [`{ paddingLeft: 5 + 5, padding: 15 }`],
      [{ paddingLeft: 10, padding: 15 }],
      [{ paddingTop: '15px', paddingRight: '15px', paddingBottom: '15px', paddingLeft: '15px' }],
      [],
    )
    expect(hookResult.value).toEqual({ unit: 'px', value: 15 })
    expect(hookResult.orderedPropKeys).toEqual([['paddingLeft', 'padding']])
    expect(hookResult.controlStatus).toEqual('simple')
  })

  it('multiselect: [paddingLeft], [paddingLeft] identical', () => {
    const { hookResult } = getPaddingHookResult(
      'paddingLeft',
      'padding',
      [`{ paddingLeft: 5 }`, `{ paddingLeft: 5 }`],
      [{ paddingLeft: 5 }, { paddingLeft: 5 }],
      [
        { paddingTop: '0px', paddingRight: '0px', paddingBottom: '0px', paddingLeft: '5px' },
        { paddingTop: '0px', paddingRight: '0px', paddingBottom: '0px', paddingLeft: '5px' },
      ],
      [],
    )
    expect(hookResult.value).toEqual({ unit: null, value: 5 })
    expect(hookResult.orderedPropKeys).toEqual([['paddingLeft'], ['paddingLeft']])
    expect(hookResult.controlStatus).toEqual('multiselect-identical-simple')
  })

  it('multiselect: [paddingLeft], [padding, paddingLeft] if not identical, return unoverwritable', () => {
    const { hookResult } = getPaddingHookResult(
      'paddingLeft',
      'padding',
      [`{ paddingLeft: 5 }`, `{ padding: 5, paddingLeft: 5 }`],
      [{ paddingLeft: 5 }, { paddingLeft: 5 }],
      [
        { paddingTop: '0px', paddingRight: '0px', paddingBottom: '0px', paddingLeft: '5px' },
        { paddingTop: '0px', paddingRight: '0px', paddingBottom: '0px', paddingLeft: '5px' },
      ],
      [],
    )
    expect(hookResult.value).toEqual(undefined)
    expect(hookResult.orderedPropKeys).toEqual([['paddingLeft'], ['padding', 'paddingLeft']])
    expect(hookResult.controlStatus).toEqual('multiselect-unoverwritable')
  })

  it('multiselect: [paddingLeft], [paddingLeft, padding] if not identical, return unoverwritable', () => {
    const { hookResult } = getPaddingHookResult(
      'paddingLeft',
      'padding',
      [`{ paddingLeft: 5 }`, `{ paddingLeft: 5, padding: 5 }`],
      [{ paddingLeft: 5 }, { paddingLeft: 5 }],
      [
        { paddingTop: '0px', paddingRight: '0px', paddingBottom: '0px', paddingLeft: '5px' },
        { paddingTop: '0px', paddingRight: '0px', paddingBottom: '0px', paddingLeft: '5px' },
      ],
      [],
    )
    expect(hookResult.value).toEqual(undefined)
    expect(hookResult.orderedPropKeys).toEqual([['paddingLeft'], ['paddingLeft', 'padding']])
    expect(hookResult.controlStatus).toEqual('multiselect-unoverwritable')
  })
})

describe('useInspectorInfo: updating padding shorthand and longhands', () => {
  it('updates an existing longhand', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      'paddingLeft',
      'padding',
      [`{ paddingLeft: 5 }`],
      [{ paddingLeft: 5 }],
      [{ paddingTop: '0px', paddingRight: '0px', paddingBottom: '0px', paddingLeft: '5px' }],
      [],
    )
    hookResult.onSubmitValue(cssNumber(100, 'px'))
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          setProp_UNSAFE(
            TestSelectedComponent,
            { propertyElements: ['style', 'paddingLeft'] },
            jsxAttributeValue({ unit: 'px', value: 100 }, emptyComments),
          ),
        ],
      ],
    ])
  })
  it('updates an existing shorthand', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      'paddingLeft',
      'padding',
      [`{ padding: "8px 8px 8px 8px" }`],
      [{ padding: 8 }],
      [{ paddingTop: '8px', paddingRight: '8px', paddingBottom: '8px', paddingLeft: '8px' }],
      [],
    )
    hookResult.onSubmitValue(cssNumber(50, 'px'))
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create(['style', 'padding']),
            jsxAttributeValue(
              {
                paddingTop: { unit: 'px', value: 8 },
                paddingRight: { unit: 'px', value: 8 },
                paddingBottom: { unit: 'px', value: 8 },
                paddingLeft: { unit: 'px', value: 50 },
              },
              emptyComments,
            ),
          ),
        ],
      ],
    ])
  })
  it('updates a controlled padding shorthand by appending a longhand', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      'paddingRight',
      'padding',
      [`{ padding: 10+10 }`],
      [{ padding: 20 }],
      [{ paddingTop: '20px', paddingRight: '20px', paddingBottom: '20px', paddingLeft: '20px' }],
      [],
    )
    hookResult.onSubmitValue(cssNumber(5, 'px'))
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create(['style', 'paddingRight']),
            jsxAttributeValue({ unit: 'px', value: 5 }, emptyComments),
          ),
        ],
      ],
    ])
  })
  it('updates an existing longhand overriding the existing controlled shorthand by changing the longhand', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      'paddingLeft',
      'padding',
      [`{ padding: 10+5, paddingLeft: 6 }`],
      [{ padding: 15, paddingLeft: 6 }],
      [{ paddingTop: '15px', paddingRight: '15px', paddingBottom: '15px', paddingLeft: '6px' }],
      [],
    )
    hookResult.onSubmitValue(cssNumber(8, 'px'))
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create(['style', 'paddingLeft']),
            jsxAttributeValue({ unit: 'px', value: 8 }, emptyComments),
          ),
        ],
      ],
    ])
  })
  it('updates an existing longhand overriding the existing shorthand by changing the longhand', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      'paddingLeft',
      'padding',
      [`{ padding: "10px", paddingLeft: 6 }`],
      [{ padding: 10, paddingLeft: 6 }],
      [{ paddingTop: '10px', paddingRight: '10px', paddingBottom: '10px', paddingLeft: '6px' }],
      [],
    )
    hookResult.onSubmitValue(cssNumber(8, 'px'))
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create(['style', 'paddingLeft']),
            jsxAttributeValue({ unit: 'px', value: 8 }, emptyComments),
          ),
        ],
      ],
    ])
  })
  it('updates a longhand that was shadowed by a expression shorthand, the shadowed longhand is deleted', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      'paddingLeft',
      'padding',
      [`{ paddingLeft: 4, padding: 4 + 6 }`],
      [{ paddingLeft: 4, padding: 10 }],
      [{ paddingTop: '10px', paddingRight: '10px', paddingBottom: '10px', paddingLeft: '4px' }],
      [],
    )
    hookResult.onSubmitValue(cssNumber(16, 'px'))
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          unsetProperty(TestSelectedComponent, { propertyElements: ['style', 'paddingLeft'] }),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create(['style', 'paddingLeft']),
            jsxAttributeValue({ unit: 'px', value: 16 }, emptyComments),
          ),
        ],
      ],
    ])
  })
  it('updates a longhand that was shadowed by a shorthand, the shadowed longhand is deleted', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      'paddingLeft',
      'padding',
      [`{ paddingLeft: 6, padding: "4px 8px 4px 8px" }`],
      [{ paddingLeft: 6, padding: '4px 8px 4px 8px' }],
      [{ paddingTop: '4px', paddingRight: '8px', paddingBottom: '4px', paddingLeft: '8px' }],
      [],
    )
    hookResult.onSubmitValue(cssNumber(18, 'px'))
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          unsetProperty(TestSelectedComponent, { propertyElements: ['style', 'paddingLeft'] }),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create(['style', 'padding']),
            jsxAttributeValue(
              {
                paddingTop: { unit: 'px', value: 4 },
                paddingRight: { unit: 'px', value: 8 },
                paddingBottom: { unit: 'px', value: 4 },
                paddingLeft: { unit: 'px', value: 18 },
              },
              emptyComments,
            ),
          ),
        ],
      ],
    ])
  })
})
