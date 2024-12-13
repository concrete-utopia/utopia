import React from 'react'
import { renderHook } from '@testing-library/react'
import type { ComputedStyle, StyleAttributeMetadata } from '../../../core/shared/element-template'
import { emptyComments, jsExpressionValue } from '../../../core/shared/element-template'
import type { ParsedPropertiesKeys } from './css-utils'
import { cssNumber } from './css-utils'
import { useInspectorInfoLonghandShorthand } from './longhand-shorthand-hooks'
import { stylePropPathMappingFn } from './property-path-hooks'
import {
  getPropsForStyleProp,
  makeInspectorHookContextProvider,
} from './property-path-hooks.test-utils'
import {
  emptyCollaborativeEditingSupport,
  type EditorStorePatched,
} from '../../editor/store/editor-state'
import create, { GetState, Mutate, SetState, StoreApi } from 'zustand'
import { subscribeWithSelector } from 'zustand/middleware'
import type { UtopiaStoreAPI } from '../../editor/store/store-hook'
import {
  createStoresAndState,
  EditorStateContext,
  OriginalMainEditorStateContext,
} from '../../editor/store/store-hook'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import { setProp_UNSAFE, unsetProperty } from '../../editor/actions/action-creators'
import { DispatchContext } from '../../editor/store/dispatch-context'
import { styleStringInArray } from '../../../utils/common-constants'
import { emptyProjectServerState } from '../../editor/store/project-server-state'
import { InitialOnlineState } from '../../editor/online-status'

const TestSelectedComponent = EP.elementPath([['scene1'], ['aaa', 'bbb']])

function getPaddingHookResult<P extends ParsedPropertiesKeys, S extends ParsedPropertiesKeys>(
  longhands: Array<P>,
  shorthand: S,
  styleObjectExpressions: Array<string>,
  spiedProps: Array<any>,
  computedStyles: Array<ComputedStyle>,
  attributeMetadatas: Array<StyleAttributeMetadata>,
) {
  const props = styleObjectExpressions.map(
    (styleExpression) => getPropsForStyleProp(styleExpression, styleStringInArray)!,
  )

  const mockDispatch = jest.fn()

  // eslint-disable-next-line @typescript-eslint/ban-types
  const contextProvider: React.FunctionComponent<React.PropsWithChildren<{}>> = ({ children }) => {
    const InspectorContextProvider = makeInspectorHookContextProvider(
      [TestSelectedComponent],
      props,
      styleStringInArray,
      spiedProps,
      computedStyles,
      attributeMetadatas,
    )

    const initialEditorStore: EditorStorePatched = {
      editor: null as any,
      derived: null as any,
      strategyState: null as any,
      history: null as any,
      userState: null as any,
      workers: null as any,
      persistence: null as any,
      elementMetadata: null as any,
      postActionInteractionSession: null,
      saveCountThisSession: 0,
      builtInDependencies: [],
      storeName: 'editor-store',
      projectServerState: emptyProjectServerState(),
      collaborativeEditingSupport: emptyCollaborativeEditingSupport(),
      onlineState: InitialOnlineState,
    }

    const storeHook: UtopiaStoreAPI = createStoresAndState(initialEditorStore)

    return (
      <DispatchContext.Provider value={mockDispatch}>
        <OriginalMainEditorStateContext.Provider value={storeHook}>
          <EditorStateContext.Provider value={storeHook}>
            <InspectorContextProvider>{children}</InspectorContextProvider>
          </EditorStateContext.Provider>
        </OriginalMainEditorStateContext.Provider>
      </DispatchContext.Provider>
    )
  }

  const { result } = renderHook(
    () =>
      useInspectorInfoLonghandShorthand(
        longhands,
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
      ['paddingLeft'],
      'padding',
      [`{ paddingLeft: 5 }`],
      [{ paddingLeft: 5 }],
      [{ paddingTop: '0px', paddingRight: '0px', paddingBottom: '0px', paddingLeft: '5px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    expect(paddingLeft.value).toEqual({ unit: null, value: 5 })
    expect(paddingLeft.orderedPropKeys).toEqual([['paddingLeft']])
  })

  it('paddingLeft, padding', () => {
    const { hookResult } = getPaddingHookResult(
      ['paddingLeft'],
      'padding',
      [`{ paddingLeft: 5, padding: 15 }`],
      [{ paddingLeft: 5, padding: 15 }],
      [{ paddingTop: '15px', paddingRight: '15px', paddingBottom: '15px', paddingLeft: '15px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    expect(paddingLeft.value).toEqual({ unit: 'px', value: 15 })
    expect(paddingLeft.orderedPropKeys).toEqual([['paddingLeft', 'padding']])
  })

  it('padding, paddingLeft', () => {
    const { hookResult } = getPaddingHookResult(
      ['paddingLeft'],
      'padding',
      [`{ padding: 15, paddingLeft: 5 }`],
      [{ padding: 15, paddingLeft: 5 }],
      [{ paddingTop: '15px', paddingRight: '15px', paddingBottom: '15px', paddingLeft: '5px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    expect(paddingLeft.value).toEqual({ unit: null, value: 5 })
    expect(paddingLeft.orderedPropKeys).toEqual([['padding', 'paddingLeft']])
  })

  it('padding, undefined paddingLeft', () => {
    const { hookResult } = getPaddingHookResult(
      ['paddingLeft'],
      'padding',
      [`{ padding: 15, paddingLeft: undefined }`],
      [{ padding: 15, paddingLeft: undefined }],
      [{ paddingTop: '15px', paddingRight: '15px', paddingBottom: '15px', paddingLeft: '0px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    expect(paddingLeft.value).toEqual({ unit: 'px', value: 0 })
    expect(paddingLeft.orderedPropKeys).toEqual([['padding', 'paddingLeft']])
  })

  it('paddingLeft, padding, paddingRight', () => {
    const { hookResult } = getPaddingHookResult(
      ['paddingLeft'],
      'padding',
      [`{ paddingLeft: 5, padding: 15, paddingRight: 20 }`], // paddingRight is irrelevant, we are making sure it doesn't affect anything
      [{ paddingLeft: 5, padding: 15, paddingRight: 20 }],
      [{ paddingTop: '15px', paddingRight: '20px', paddingBottom: '15px', paddingLeft: '15px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    expect(paddingLeft.value).toEqual({ unit: 'px', value: 15 })
    expect(paddingLeft.orderedPropKeys).toEqual([['paddingLeft', 'padding']])
  })

  it('paddingLeft controlled', () => {
    const { hookResult } = getPaddingHookResult(
      ['paddingLeft'],
      'padding',
      [`{ paddingLeft: 5 + 5 }`],
      [{ paddingLeft: 10 }],
      [{ paddingTop: '0px', paddingRight: '0px', paddingBottom: '0px', paddingLeft: '10px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    expect(paddingLeft.value).toEqual({ unit: null, value: 10 })
    expect(paddingLeft.orderedPropKeys).toEqual([['paddingLeft']])
    expect(paddingLeft.controlStatus).toEqual('controlled')
  })

  it('paddingLeft, padding controlled', () => {
    const { hookResult } = getPaddingHookResult(
      ['paddingLeft'],
      'padding',
      [`{ paddingLeft: 5, padding: 15 + 5 }`],
      [{ paddingLeft: 5, padding: 20 }],
      [{ paddingTop: '20px', paddingRight: '20px', paddingBottom: '20px', paddingLeft: '20px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    expect(paddingLeft.value).toEqual({ unit: 'px', value: 20 })
    expect(paddingLeft.orderedPropKeys).toEqual([['paddingLeft', 'padding']])
    expect(paddingLeft.controlStatus).toEqual('controlled')
  })

  it('padding controlled, paddingLeft', () => {
    const { hookResult } = getPaddingHookResult(
      ['paddingLeft'],
      'padding',
      [`{ padding: 15 + 5, paddingLeft: 5 }`],
      [{ padding: 20, paddingLeft: 5 }],
      [{ paddingTop: '20px', paddingRight: '20px', paddingBottom: '20px', paddingLeft: '5px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    expect(paddingLeft.value).toEqual({ unit: null, value: 5 })
    expect(paddingLeft.orderedPropKeys).toEqual([['padding', 'paddingLeft']])
    expect(paddingLeft.controlStatus).toEqual('simple')
  })

  it('paddingLeft controlled, padding', () => {
    const { hookResult } = getPaddingHookResult(
      ['paddingLeft'],
      'padding',
      [`{ paddingLeft: 5 + 5, padding: 15 }`],
      [{ paddingLeft: 10, padding: 15 }],
      [{ paddingTop: '15px', paddingRight: '15px', paddingBottom: '15px', paddingLeft: '15px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    expect(paddingLeft.value).toEqual({ unit: 'px', value: 15 })
    expect(paddingLeft.orderedPropKeys).toEqual([['paddingLeft', 'padding']])
    expect(paddingLeft.controlStatus).toEqual('simple')
  })

  it('multiselect: [paddingLeft], [paddingLeft] identical', () => {
    const { hookResult } = getPaddingHookResult(
      ['paddingLeft'],
      'padding',
      [`{ paddingLeft: 5 }`, `{ paddingLeft: 5 }`],
      [{ paddingLeft: 5 }, { paddingLeft: 5 }],
      [
        { paddingTop: '0px', paddingRight: '0px', paddingBottom: '0px', paddingLeft: '5px' },
        { paddingTop: '0px', paddingRight: '0px', paddingBottom: '0px', paddingLeft: '5px' },
      ],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    expect(paddingLeft.value).toEqual({ unit: null, value: 5 })
    expect(paddingLeft.orderedPropKeys).toEqual([['paddingLeft'], ['paddingLeft']])
    expect(paddingLeft.controlStatus).toEqual('multiselect-identical-simple')
  })

  it('multiselect: [paddingLeft], [padding, paddingLeft] if not identical, return unoverwritable', () => {
    const { hookResult } = getPaddingHookResult(
      ['paddingLeft'],
      'padding',
      [`{ paddingLeft: 5 }`, `{ padding: 5, paddingLeft: 5 }`],
      [{ paddingLeft: 5 }, { padding: 5, paddingLeft: 5 }],
      [
        { paddingTop: '0px', paddingRight: '0px', paddingBottom: '0px', paddingLeft: '5px' },
        { paddingTop: '0px', paddingRight: '0px', paddingBottom: '0px', paddingLeft: '5px' },
      ],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    expect(paddingLeft.value).toEqual({ unit: null, value: 5 })
    expect(paddingLeft.orderedPropKeys).toEqual([['paddingLeft'], ['padding', 'paddingLeft']])
    expect(paddingLeft.controlStatus).toEqual('multiselect-unoverwritable')
  })

  it('multiselect: [paddingLeft], [paddingLeft, padding] if not identical, return unoverwritable', () => {
    const { hookResult } = getPaddingHookResult(
      ['paddingLeft'],
      'padding',
      [`{ paddingLeft: 5 }`, `{ paddingLeft: 5, padding: 5 }`],
      [{ paddingLeft: 5 }, { paddingLeft: 5 }],
      [
        { paddingTop: '0px', paddingRight: '0px', paddingBottom: '0px', paddingLeft: '5px' },
        { paddingTop: '0px', paddingRight: '0px', paddingBottom: '0px', paddingLeft: '5px' },
      ],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    expect(paddingLeft.value).toEqual({ unit: null, value: 5 })
    expect(paddingLeft.orderedPropKeys).toEqual([['paddingLeft'], ['paddingLeft', 'padding']])
    expect(paddingLeft.controlStatus).toEqual('multiselect-unoverwritable')
  })
})

describe('useInspectorInfo: updating padding shorthand and longhands', () => {
  it('updates an existing longhand', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      ['paddingLeft'],
      'padding',
      [`{ paddingLeft: 5 }`],
      [{ paddingLeft: 5 }],
      [{ paddingTop: '0px', paddingRight: '0px', paddingBottom: '0px', paddingLeft: '5px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    paddingLeft.onSubmitValue(cssNumber(100, 'px'))
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingLeft'),
            jsExpressionValue(100, emptyComments, expect.stringContaining('')),
          ),
        ],
      ],
    ])
  })
  it('updates an existing shorthand', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      ['paddingLeft'],
      'padding',
      [`{ padding: "8px 8px 8px 8px" }`],
      [{ padding: 8 }],
      [{ paddingTop: '8px', paddingRight: '8px', paddingBottom: '8px', paddingLeft: '8px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    paddingLeft.onSubmitValue(cssNumber(50, 'px'))
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'padding'),
            jsExpressionValue('8px 8px 8px 50px', emptyComments, expect.stringContaining('')),
          ),
        ],
      ],
    ])
  })
  it('updates a controlled padding shorthand by appending a longhand', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      ['paddingRight'],
      'padding',
      [`{ padding: 10+10 }`],
      [{ padding: 20 }],
      [{ paddingTop: '20px', paddingRight: '20px', paddingBottom: '20px', paddingLeft: '20px' }],
      [],
    )
    const paddingRight = hookResult.paddingRight
    paddingRight.onSubmitValue(cssNumber(5, 'px'))
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingRight'),
            jsExpressionValue(5, emptyComments, expect.stringContaining('')),
          ),
        ],
      ],
    ])
  })
  it('updates an existing longhand overriding the existing controlled shorthand by changing the longhand', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      ['paddingLeft'],
      'padding',
      [`{ padding: 10+5, paddingLeft: 6 }`],
      [{ padding: 15, paddingLeft: 6 }],
      [{ paddingTop: '15px', paddingRight: '15px', paddingBottom: '15px', paddingLeft: '6px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    paddingLeft.onSubmitValue(cssNumber(8, 'px'))
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingLeft'),
            jsExpressionValue(8, emptyComments, expect.stringContaining('')),
          ),
        ],
      ],
    ])
  })
  it('updates an existing longhand overriding the existing shorthand by changing the longhand', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      ['paddingLeft'],
      'padding',
      [`{ padding: "10px", paddingLeft: 6 }`],
      [{ padding: 10, paddingLeft: 6 }],
      [{ paddingTop: '10px', paddingRight: '10px', paddingBottom: '10px', paddingLeft: '6px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    paddingLeft.onSubmitValue(cssNumber(8, 'px'))
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingLeft'),
            jsExpressionValue(8, emptyComments, expect.stringContaining('')),
          ),
        ],
      ],
    ])
  })
  it('updates a longhand that was shadowed by a expression shorthand, the shadowed longhand is deleted', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      ['paddingLeft'],
      'padding',
      [`{ paddingLeft: 4, padding: 4 + 6 }`],
      [{ paddingLeft: 4, padding: 10 }],
      [{ paddingTop: '10px', paddingRight: '10px', paddingBottom: '10px', paddingLeft: '4px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    paddingLeft.onSubmitValue(cssNumber(16, 'px'))
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          unsetProperty(TestSelectedComponent, { propertyElements: ['style', 'paddingLeft'] }),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingLeft'),
            jsExpressionValue(16, emptyComments, expect.stringContaining('')),
          ),
        ],
      ],
    ])
  })
  it('updates a longhand that was shadowed by a shorthand, the shadowed longhand is deleted', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      ['paddingLeft'],
      'padding',
      [`{ paddingLeft: 6, padding: "4px 8px 4px 8px" }`],
      [{ paddingLeft: 6, padding: '4px 8px 4px 8px' }],
      [{ paddingTop: '4px', paddingRight: '8px', paddingBottom: '4px', paddingLeft: '8px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    paddingLeft.onSubmitValue(cssNumber(18, 'px'))
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          unsetProperty(TestSelectedComponent, { propertyElements: ['style', 'paddingLeft'] }),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'padding'),
            jsExpressionValue('4px 8px 4px 18px', emptyComments, expect.stringContaining('')),
          ),
        ],
      ],
    ])
  })
})

describe('useInspectorInfo: onUnsetValues', () => {
  it('unsets a simple longhand', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      ['paddingTop', 'paddingRight', 'paddingBottom', 'paddingLeft'],
      'padding',
      [`{ paddingLeft: 6 }`],
      [{ paddingLeft: 6 }],
      [{ paddingLeft: '6px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    paddingLeft.onUnsetValues()
    expect(mockDispatch.mock.calls).toEqual([
      [[unsetProperty(TestSelectedComponent, PP.create('style', 'paddingLeft'))]],
    ])
  })

  it('unsetting a simple shorthand results in the longhand being broken up', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      ['paddingTop', 'paddingRight', 'paddingBottom', 'paddingLeft'],
      'padding',
      [`{ padding: 10 }`],
      [{ padding: 10 }],
      [{ paddingTop: '10px', paddingRight: '10px', paddingBottom: '10px', paddingLeft: '10px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    paddingLeft.onUnsetValues()
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          unsetProperty(TestSelectedComponent, PP.create('style', 'paddingLeft')),
          unsetProperty(TestSelectedComponent, PP.create('style', 'padding')),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingTop'),
            jsExpressionValue(10, emptyComments, expect.stringContaining('')),
          ),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingRight'),
            jsExpressionValue(10, emptyComments, expect.stringContaining('')),
          ),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingBottom'),
            jsExpressionValue(10, emptyComments, expect.stringContaining('')),
          ),
        ],
      ],
    ])
  })

  it('unsetting a controlled shorthand means overriding it with an undefined', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      ['paddingTop', 'paddingRight', 'paddingBottom', 'paddingLeft'],
      'padding',
      [`{ padding: 5 + 5 }`],
      [{ padding: 10 }],
      [{ paddingTop: '10px', paddingRight: '10px', paddingBottom: '10px', paddingLeft: '10px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    paddingLeft.onUnsetValues()
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          unsetProperty(TestSelectedComponent, PP.create('style', 'paddingLeft')),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingLeft'),
            jsExpressionValue(undefined, emptyComments, expect.stringContaining('')),
          ),
        ],
      ],
    ])
  })

  it('longhand overriding a shorthand', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      ['paddingTop', 'paddingRight', 'paddingBottom', 'paddingLeft'],
      'padding',
      [`{ padding: 10, paddingLeft: 5 }`],
      [{ padding: 10, paddingLeft: 5 }],
      [{ paddingTop: '10px', paddingRight: '10px', paddingBottom: '10px', paddingLeft: '5px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    paddingLeft.onUnsetValues()
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          unsetProperty(TestSelectedComponent, PP.create('style', 'paddingLeft')),
          unsetProperty(TestSelectedComponent, PP.create('style', 'padding')),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingTop'),
            jsExpressionValue(10, emptyComments, expect.stringContaining('')),
          ),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingRight'),
            jsExpressionValue(10, emptyComments, expect.stringContaining('')),
          ),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingBottom'),
            jsExpressionValue(10, emptyComments, expect.stringContaining('')),
          ),
        ],
      ],
    ])
  })

  it('shorthand shadowing a longhand', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      ['paddingTop', 'paddingRight', 'paddingBottom', 'paddingLeft'],
      'padding',
      [`{ paddingLeft: 5, padding: 10 }`],
      [{ paddingLeft: 5, padding: 10 }],
      [{ paddingTop: '10px', paddingRight: '10px', paddingBottom: '10px', paddingLeft: '10px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    paddingLeft.onUnsetValues()
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          unsetProperty(TestSelectedComponent, PP.create('style', 'paddingLeft')),
          unsetProperty(TestSelectedComponent, PP.create('style', 'padding')),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingTop'),
            jsExpressionValue(10, emptyComments, expect.stringContaining('')),
          ),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingRight'),
            jsExpressionValue(10, emptyComments, expect.stringContaining('')),
          ),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingBottom'),
            jsExpressionValue(10, emptyComments, expect.stringContaining('')),
          ),
        ],
      ],
    ])
  })

  it('longhand overriding a controlled shorthand', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      ['paddingTop', 'paddingRight', 'paddingBottom', 'paddingLeft'],
      'padding',
      [`{ padding: 5 + 5, paddingLeft: 5 }`],
      [{ padding: 10, paddingLeft: 5 }],
      [{ paddingTop: '10px', paddingRight: '10px', paddingBottom: '10px', paddingLeft: '5px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    paddingLeft.onUnsetValues()
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          unsetProperty(TestSelectedComponent, PP.create('style', 'paddingLeft')),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingLeft'),
            jsExpressionValue(undefined, emptyComments, expect.stringContaining('')),
          ),
        ],
      ],
    ])
  })

  it('controlled shorthand shadowing a longhand', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      ['paddingTop', 'paddingRight', 'paddingBottom', 'paddingLeft'],
      'padding',
      [`{ paddingLeft: 5, padding: 5 + 5 }`],
      [{ paddingLeft: 5, padding: 10 }],
      [{ paddingTop: '10px', paddingRight: '10px', paddingBottom: '10px', paddingLeft: '10px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    paddingLeft.onUnsetValues()
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          unsetProperty(TestSelectedComponent, PP.create('style', 'paddingLeft')),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingLeft'),
            jsExpressionValue(undefined, emptyComments, expect.stringContaining('')),
          ),
        ],
      ],
    ])
  })

  it('an unrelated longhand shadowing the shorthand must be kept alive', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      ['paddingTop', 'paddingRight', 'paddingBottom', 'paddingLeft'],
      'padding',
      [`{ paddingLeft: 5, padding: 10, paddingRight: 25 }`],
      [{ paddingLeft: 5, padding: 10 }],
      [{ paddingTop: '10px', paddingRight: '25px', paddingBottom: '10px', paddingLeft: '10px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    paddingLeft.onUnsetValues()
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          unsetProperty(TestSelectedComponent, PP.create('style', 'paddingLeft')),
          unsetProperty(TestSelectedComponent, PP.create('style', 'padding')),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingTop'),
            jsExpressionValue(10, emptyComments, expect.stringContaining('')),
          ),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingRight'),
            jsExpressionValue(25, emptyComments, expect.stringContaining('')),
          ),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingBottom'),
            jsExpressionValue(10, emptyComments, expect.stringContaining('')),
          ),
        ],
      ],
    ])
  })

  it('an unrelated longhand shadowed by the shorthand has to be overwritten', () => {
    const { hookResult, mockDispatch } = getPaddingHookResult(
      ['paddingTop', 'paddingRight', 'paddingBottom', 'paddingLeft'],
      'padding',
      [`{ paddingLeft: 5, paddingRight: 25, padding: 10 }`],
      [{ paddingLeft: 5, padding: 10 }],
      [{ paddingTop: '10px', paddingRight: '10px', paddingBottom: '10px', paddingLeft: '10px' }],
      [],
    )
    const paddingLeft = hookResult.paddingLeft
    paddingLeft.onUnsetValues()
    expect(mockDispatch.mock.calls).toEqual([
      [
        [
          unsetProperty(TestSelectedComponent, PP.create('style', 'paddingLeft')),
          unsetProperty(TestSelectedComponent, PP.create('style', 'padding')),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingTop'),
            jsExpressionValue(10, emptyComments, expect.stringContaining('')),
          ),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingRight'),
            jsExpressionValue(10, emptyComments, expect.stringContaining('')),
          ),
          setProp_UNSAFE(
            TestSelectedComponent,
            PP.create('style', 'paddingBottom'),
            jsExpressionValue(10, emptyComments, expect.stringContaining('')),
          ),
        ],
      ],
    ])
  })
})
