import { produce } from 'immer'
import * as React from 'react'
import create from 'zustand'
import { jsxAttributeValue, JSXElement } from '../../../core/shared/element-template'
import { setJSXValueAtPath } from '../../../core/shared/jsx-attributes'
import { isRight } from '../../../core/shared/either'
import { PropertyPath, StaticInstancePath } from '../../../core/shared/project-file-types'
import { createEditorStates } from '../../../utils/test-utils'
import utils from '../../../utils/utils'
import { EditorDispatch } from '../../editor/action-types'
import { EditorStore, modifyOpenJsxElementAtStaticPath } from '../../editor/store/editor-state'
import { EditorStateContext, EditorStateContextData } from '../../editor/store/store-hook'
import * as TP from '../../../core/shared/template-path'
import { InspectorContextProvider } from '../inspector'
import { getControlStyles, PropertyStatus } from './control-status'
import { InspectorInfo } from './property-path-hooks'
import { ScenePathForTestUiJsFile } from '../../../core/model/test-ui-js-file'

type UpdateFunctionHelpers = {
  updateStoreWithImmer: (fn: (store: EditorStore) => void) => void
  updateStore: (fn: (store: EditorStore) => EditorStore) => void
}

export function getStoreHook(
  mockDispatch: EditorDispatch,
): EditorStateContextData & UpdateFunctionHelpers {
  const editor = createEditorStates('/src/app.ui.js', [
    TP.instancePath(ScenePathForTestUiJsFile.sceneElementPath, ['aaa', 'bbb']),
  ])
  const defaultState: EditorStore = {
    editor: editor.editor,
    derived: editor.derivedState,
    history: {
      previous: [],
      next: [],
      current: {} as any,
    },
    loginState: { type: 'NOT_LOGGED_IN' },
    workers: null as any,
    dispatch: mockDispatch,
  }

  const [storeHook, api] = create<EditorStore & UpdateFunctionHelpers>((set) => ({
    ...defaultState,
    updateStoreWithImmer: (fn: (store: EditorStore) => void) => set(produce(fn)),
    updateStore: (fn: (store: EditorStore) => EditorStore) => set(fn),
  }))
  return {
    api: api,
    useStore: storeHook,
    updateStoreWithImmer: api.getState().updateStoreWithImmer,
    updateStore: api.getState().updateStore,
  }
}

export const TestInspectorContextProvider: React.FunctionComponent<{
  editorStoreData: EditorStateContextData
}> = (props) => {
  return (
    <EditorStateContext.Provider value={props.editorStoreData}>
      <InspectorContextProvider targetPath={['style']}>{props.children}</InspectorContextProvider>
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
      store.editor.selectedViews[0] as StaticInstancePath,
      (element): JSXElement => {
        const updatedAttributes = setJSXValueAtPath(
          element.props,
          path,
          jsxAttributeValue(newValue),
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
