import * as React from 'react'
import { renderHook } from '@testing-library/react-hooks'
import { ComputedStyle, StyleAttributeMetadata } from '../../../core/shared/element-template'
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
      [],
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
    expect(hookResult.value).toEqual({ unit: null, value: 10 })
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
  it('updates paddingLeft', () => {
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
          {
            action: 'SET_PROP',
            propertyPath: { propertyElements: ['what', 'up'] },
            target: {
              element: ['hello', 'eni'],
              scene: { sceneElementPath: [], type: 'scenepath' },
            },
            value: {
              comments: { leadingComments: [], trailingComments: [] },
              type: 'ATTRIBUTE_VALUE',
              value: { unit: 'px', value: 100 },
            },
          },
        ],
      ],
    ])
  })
})
