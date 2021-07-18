import { produce } from 'immer'
import * as React from 'react'
import create from 'zustand'
import { jsxAttributeValue, JSXElement } from '../../../core/shared/element-template'
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
  EditorStore,
  modifyOpenJsxElementAtStaticPath,
  defaultUserState,
  StoryboardFilePath,
  regularMode,
} from '../../editor/store/editor-state'
import { EditorStateContext, EditorStateContextData } from '../../editor/store/store-hook'
import * as EP from '../../../core/shared/element-path'
import { InspectorContextProvider } from '../inspector'
import { getControlStyles, PropertyStatus } from './control-status'
import { InspectorInfo } from './property-path-hooks'
import { ScenePathForTestUiJsFile } from '../../../core/model/test-ui-js-file.test-utils'
import { emptyComments } from '../../../core/workers/parser-printer/parser-printer-comments'
import { Frame } from 'utopia-api'
import { PinsInfo } from './layout-property-path-hooks'
import { CSSNumber } from './css-utils'
import { mapValues } from '../../../core/shared/object-utils'
import { LayoutPinnedProp } from '../../../core/layout/layout-helpers-new'
import { LocalRectangle, localRectangle } from '../../../core/shared/math-utils'

type UpdateFunctionHelpers = {
  updateStoreWithImmer: (fn: (store: EditorStore) => void) => void
  updateStore: (fn: (store: EditorStore) => EditorStore) => void
}

export function getStoreHook(
  mockDispatch: EditorDispatch,
): EditorStateContextData & UpdateFunctionHelpers {
  const editor = createEditorStates([
    EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'bbb']),
  ])
  const defaultState: EditorStore = {
    editor: editor.editor,
    derived: editor.derivedState,
    history: {
      previous: [],
      next: [],
      current: {} as any,
    },
    userState: defaultUserState,
    workers: null as any,
    dispatch: mockDispatch,
    alreadySaved: false,
    editorMode: regularMode,
  }

  const storeHook = create<EditorStore & UpdateFunctionHelpers>((set) => ({
    ...defaultState,
    updateStoreWithImmer: (fn: (store: EditorStore) => void) => set(produce(fn)),
    updateStore: (fn: (store: EditorStore) => EditorStore) => set(fn),
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
  store: EditorStore,
  path: PropertyPath,
  newValue: number | string,
): EditorStore {
  return {
    ...store,
    editor: modifyOpenJsxElementAtStaticPath(
      store.editor.selectedViews[0] as StaticElementPath,
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
      store.editor,
    ),
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

export function frameForPins(pins: SimplePinsInfo): Frame {
  return {
    left: pins.PinnedLeft?.value,
    centerX: pins.PinnedCenterX?.value,
    right: pins.PinnedRight?.value,
    width: pins.Width?.value,
    top: pins.PinnedTop?.value,
    centerY: pins.PinnedCenterY?.value,
    bottom: pins.PinnedBottom?.value,
    height: pins.Height?.value,
  }
}

export const TLWHSimplePins: SimplePinsInfo = {
  PinnedLeft: {
    value: SimpleRect.x,
    unit: null,
  },
  Width: { value: SimpleRect.width, unit: null },
  PinnedTop: { value: SimpleRect.y, unit: null },
  Height: { value: SimpleRect.height, unit: null },
  PinnedBottom: undefined,
  PinnedRight: undefined,
  PinnedCenterX: undefined,
  PinnedCenterY: undefined,
}

export const TLBRSimplePins: SimplePinsInfo = {
  PinnedLeft: { value: SimpleRect.x, unit: null },
  Width: undefined,
  PinnedTop: { value: SimpleRect.y, unit: null },
  Height: undefined,
  PinnedBottom: { value: SimpleRect.y + SimpleRect.height, unit: null },
  PinnedRight: { value: SimpleRect.x + SimpleRect.width, unit: null },
  PinnedCenterX: undefined,
  PinnedCenterY: undefined,
}

export const CxCyWHSimplePins: SimplePinsInfo = {
  PinnedLeft: undefined,
  Width: { value: SimpleRect.width, unit: null },
  PinnedTop: undefined,
  Height: { value: SimpleRect.height, unit: null },
  PinnedBottom: undefined,
  PinnedRight: undefined,
  PinnedCenterX: { value: SimpleRect.x, unit: null }, // Offset by 10 since both parent and element frames are the same width
  PinnedCenterY: { value: SimpleRect.y, unit: null }, // Offset by 10 since both parent and element frames are the same height
}
