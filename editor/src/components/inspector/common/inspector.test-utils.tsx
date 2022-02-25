import { produce } from 'immer'
import React from 'react'
import create from 'zustand'
import { emptyComments, jsxAttributeValue, JSXElement } from '../../../core/shared/element-template'
import { setJSXValueAtPath } from '../../../core/shared/jsx-attributes'
import { isRight } from '../../../core/shared/either'
import type {
  PropertyPath,
  StaticElementPath,
  ElementPath,
} from '../../../core/shared/project-file-types'
import { createEditorStates } from '../../../utils/utils.test-utils'
import utils from '../../../utils/utils'
import { EditorDispatch } from '../../editor/action-types'
import {
  modifyOpenJsxElementAtStaticPath,
  defaultUserState,
  StoryboardFilePath,
  EditorStoreFull,
} from '../../editor/store/editor-state'
import { EditorStateContext, EditorStateContextData } from '../../editor/store/store-hook'
import * as EP from '../../../core/shared/element-path'
import { InspectorContextProvider } from '../inspector'
import { getControlStyles, PropertyStatus } from './control-status'
import { InspectorInfo } from './property-path-hooks'
import { ScenePathForTestUiJsFile } from '../../../core/model/test-ui-js-file.test-utils'
import { Frame } from 'utopia-api/core'
import { PinsInfo } from './layout-property-path-hooks'
import { cssNumber, CSSNumber } from './css-utils'
import { mapValues } from '../../../core/shared/object-utils'
import { LayoutPinnedProp } from '../../../core/layout/layout-helpers-new'
import { LocalRectangle, localRectangle } from '../../../core/shared/math-utils'
import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { NO_OP } from '../../../core/shared/utils'

type UpdateFunctionHelpers = {
  updateStoreWithImmer: (fn: (store: EditorStoreFull) => void) => void
  updateStore: (fn: (store: EditorStoreFull) => EditorStoreFull) => void
}

export function getStoreHook(
  mockDispatch: EditorDispatch,
): EditorStateContextData & UpdateFunctionHelpers {
  const editor = createEditorStates([
    EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'bbb']),
  ])
  const defaultState: EditorStoreFull = {
    unpatchedEditor: editor.editor,
    patchedEditor: editor.editor,
    unpatchedDerived: editor.derivedState,
    patchedDerived: editor.derivedState,
    strategyState: editor.strategyState,
    history: {
      previous: [],
      next: [],
      current: {} as any,
    },
    userState: defaultUserState,
    workers: null as any,
    persistence: null as any,
    dispatch: mockDispatch,
    alreadySaved: false,
    builtInDependencies: createBuiltInDependenciesList(null),
  }

  const storeHook = create<EditorStoreFull & UpdateFunctionHelpers>((set) => ({
    ...defaultState,
    updateStoreWithImmer: (fn: (store: EditorStoreFull) => void) => set(produce(fn)),
    updateStore: (fn: (store: EditorStoreFull) => EditorStoreFull) => set(fn),
  }))
  return {
    api: storeHook,
    useStore: storeHook,
    updateStoreWithImmer: storeHook.getState().updateStoreWithImmer,
    updateStore: storeHook.getState().updateStore,
  }
}

export const TestInspectorContextProvider: React.FunctionComponent<{
  selectedViews: Array<ElementPath>
  editorStoreData: EditorStateContextData
}> = (props) => {
  return (
    <EditorStateContext.Provider value={props.editorStoreData}>
      <InspectorContextProvider selectedViews={props.selectedViews} targetPath={['style']}>
        {props.children}
      </InspectorContextProvider>
    </EditorStateContext.Provider>
  )
}

export function editPropOfSelectedView(
  store: EditorStoreFull,
  path: PropertyPath,
  newValue: number | string,
): EditorStoreFull {
  const updatedEditor = modifyOpenJsxElementAtStaticPath(
    store.patchedEditor.selectedViews[0] as StaticElementPath,
    (element): JSXElement => {
      const updatedAttributes = setJSXValueAtPath(
        element.props,
        path,
        jsxAttributeValue(newValue, emptyComments),
      )
      if (isRight(updatedAttributes)) {
        return {
          ...element,
          props: updatedAttributes.value,
        }
      } else {
        throw new Error(`Couldn't set property in test`)
      }
    },
    store.patchedEditor,
  )
  return {
    ...store,
    unpatchedEditor: updatedEditor,
    patchedEditor: updatedEditor,
  }
}

const dummyPropertyStatus: PropertyStatus = {
  controlled: false,
  set: true,
  overwritable: true,
  selectionLength: 1,
  identical: true,
  detected: false,
  fromCssStyleSheet: false,
  trivialDefault: false,
}

const simpleControlStyles = getControlStyles('simple')

export function testInspectorInfo<T>(value: T): InspectorInfo<T> {
  return {
    value: value,
    controlStatus: 'simple',
    propertyStatus: dummyPropertyStatus,
    controlStyles: simpleControlStyles,
    onUnsetValues: utils.NO_OP,
    onSubmitValue: utils.NO_OP,
    onTransientSubmitValue: utils.NO_OP,
    useSubmitValueFactory: (transform: (newValue: never, oldValue: T) => T) => [
      utils.NO_OP,
      utils.NO_OP,
    ],
  }
}

export const SimpleRect: LocalRectangle = localRectangle({
  x: 10,
  y: 10,
  width: 100,
  height: 100,
})

export type SimplePinsInfo = { [key in LayoutPinnedProp]: CSSNumber | undefined }

export function pinsInfoForPins(pins: SimplePinsInfo): PinsInfo {
  return mapValues((pin) => testInspectorInfo(pin), pins) as PinsInfo
}

function getDimensionCenter(
  start: CSSNumber | undefined,
  end: CSSNumber | undefined,
  gap: CSSNumber | undefined,
): CSSNumber | undefined {
  if (start == null) {
    if (end == null) {
      return undefined
    } else {
      if (gap == null) {
        return undefined
      } else {
        return cssNumber(end.value - gap.value / 2)
      }
    }
  } else {
    if (end == null) {
      if (gap == null) {
        return undefined
      } else {
        return cssNumber(start.value + gap.value / 2)
      }
    } else {
      if (gap == null) {
        return cssNumber(start.value + (end.value - start.value) / 2)
      } else {
        return cssNumber(start.value + gap.value / 2)
      }
    }
  }
}

export function frameForPins(pins: SimplePinsInfo): Frame {
  return {
    left: pins.left?.value,
    centerX: getDimensionCenter(pins.left, pins.right, pins.width)?.value,
    right: pins.right?.value,
    width: pins.width?.value,
    top: pins.top?.value,
    centerY: getDimensionCenter(pins.top, pins.bottom, pins.height)?.value,
    bottom: pins.bottom?.value,
    height: pins.height?.value,
  }
}

export const TLWHSimplePins: SimplePinsInfo = {
  left: {
    value: SimpleRect.x,
    unit: null,
  },
  width: { value: SimpleRect.width, unit: null },
  top: { value: SimpleRect.y, unit: null },
  height: { value: SimpleRect.height, unit: null },
  bottom: undefined,
  right: undefined,
}

export const TLBRSimplePins: SimplePinsInfo = {
  left: { value: SimpleRect.x, unit: null },
  width: undefined,
  top: { value: SimpleRect.y, unit: null },
  height: undefined,
  bottom: { value: SimpleRect.y + SimpleRect.height, unit: null },
  right: { value: SimpleRect.x + SimpleRect.width, unit: null },
}
