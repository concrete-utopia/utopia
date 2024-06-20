import { produce } from 'immer'
import React from 'react'
import create from 'zustand'
import { subscribeWithSelector } from 'zustand/middleware'
import type { JSXElement } from '../../../core/shared/element-template'
import { emptyComments, jsExpressionValue } from '../../../core/shared/element-template'
import { setJSXValueAtPath } from '../../../core/shared/jsx-attribute-utils'
import { isRight } from '../../../core/shared/either'
import type {
  PropertyPath,
  StaticElementPath,
  ElementPath,
} from '../../../core/shared/project-file-types'
import { createEditorStates } from '../../../utils/utils.test-utils'
import utils from '../../../utils/utils'
import { EditorDispatch } from '../../editor/action-types'
import type { EditorStorePatched } from '../../editor/store/editor-state'
import {
  defaultUserState,
  StoryboardFilePath,
  modifyUnderlyingElementForOpenFile,
  emptyCollaborativeEditingSupport,
} from '../../editor/store/editor-state'
import type { UtopiaStoreAPI } from '../../editor/store/store-hook'
import {
  createStoresAndState,
  EditorStateContext,
  OriginalMainEditorStateContext,
} from '../../editor/store/store-hook'
import * as EP from '../../../core/shared/element-path'
import { InspectorContextProvider } from '../inspector'
import type { PropertyStatus } from './control-status'
import { getControlStyles } from './control-styles'
import type { InspectorInfo } from './property-path-hooks'
import { ScenePathForTestUiJsFile } from '../../../core/model/test-ui-js-file.test-utils'
import type { Frame } from 'utopia-api/core'
import type { PinsInfo } from './layout-property-path-hooks'
import type { CSSNumber } from './css-utils'
import { cssNumber } from './css-utils'
import { mapValues } from '../../../core/shared/object-utils'
import type { LayoutPinnedProp } from '../../../core/layout/layout-helpers-new'
import type { LocalRectangle } from '../../../core/shared/math-utils'
import { localRectangle } from '../../../core/shared/math-utils'
import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { DispatchContext } from '../../editor/store/dispatch-context'
import { NO_OP } from '../../../core/shared/utils'
import { styleStringInArray } from '../../../utils/common-constants'
import { fireEvent } from '@testing-library/react'
import type { EditorRenderResult } from '../../canvas/ui-jsx.test-utils'
import { emptyProjectServerState } from '../../editor/store/project-server-state'
import { InitialOnlineState } from '../../editor/online-status'

type UpdateFunctionHelpers = {
  updateStoreWithImmer: (fn: (store: EditorStorePatched) => void) => void
  updateStore: (fn: (store: EditorStorePatched) => EditorStorePatched) => void
}

export function getStoreHook(): UtopiaStoreAPI & UpdateFunctionHelpers {
  const editor = createEditorStates([
    EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'mmm', 'bbb']),
  ])
  const defaultState: EditorStorePatched = {
    storeName: 'editor-store',
    editor: editor.editor,
    derived: editor.derivedState,
    strategyState: editor.strategyState,
    history: {
      previous: [],
      next: [],
      current: {} as any,
    },
    userState: defaultUserState,
    workers: null as any,
    persistence: null as any,
    saveCountThisSession: 0,
    postActionInteractionSession: null,
    builtInDependencies: createBuiltInDependenciesList(null),
    projectServerState: emptyProjectServerState(),
    collaborativeEditingSupport: emptyCollaborativeEditingSupport(),
    onlineState: InitialOnlineState,
  }

  const storeHook: UtopiaStoreAPI = createStoresAndState(defaultState)
  const updateStoreWithImmer = (fn: (store: EditorStorePatched) => void) =>
    storeHook.setState(produce(fn)(storeHook.getState()))
  const updateStore = (fn: (store: EditorStorePatched) => EditorStorePatched) =>
    storeHook.setState(fn(storeHook.getState()))

  return {
    ...storeHook,
    updateStoreWithImmer: updateStoreWithImmer,
    updateStore: updateStore,
  }
}

export const TestInspectorContextProvider: React.FunctionComponent<
  React.PropsWithChildren<{
    selectedViews: Array<ElementPath>
    editorStoreData: UtopiaStoreAPI
  }>
> = (props) => {
  return (
    <DispatchContext.Provider value={NO_OP}>
      <OriginalMainEditorStateContext.Provider value={props.editorStoreData}>
        <EditorStateContext.Provider value={props.editorStoreData}>
          <InspectorContextProvider
            selectedViews={props.selectedViews}
            targetPath={styleStringInArray}
          >
            {props.children}
          </InspectorContextProvider>
        </EditorStateContext.Provider>
      </OriginalMainEditorStateContext.Provider>
    </DispatchContext.Provider>
  )
}

export function editPropOfSelectedView(
  store: EditorStorePatched,
  path: PropertyPath,
  newValue: number | string,
): EditorStorePatched {
  return {
    ...store,
    editor: modifyUnderlyingElementForOpenFile(
      store.editor.selectedViews[0] as StaticElementPath,
      store.editor,
      (element): JSXElement => {
        const updatedAttributes = setJSXValueAtPath(
          element.props,
          path,
          jsExpressionValue(newValue, emptyComments),
          'include-in-printing',
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

export async function changeInspectorNumberControl(
  editor: EditorRenderResult,
  testId: string,
  newValue: string,
): Promise<void> {
  const numberInput = editor.renderedDOM.getByTestId(testId) as HTMLInputElement

  numberInput.focus()

  fireEvent.change(numberInput, {
    target: { value: newValue },
  })

  numberInput.blur()
}
