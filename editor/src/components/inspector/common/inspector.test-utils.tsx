import { produce } from 'immer'
import * as React from 'react'
import create from 'zustand'
import { jsxAttributeValue, JSXElement } from '../../../core/shared/element-template'
import { setJSXValueAtPath } from '../../../core/shared/jsx-attributes'
import { isRight } from '../../../core/shared/either'
import type {
  PropertyPath,
  StaticInstancePath,
  TemplatePath,
} from '../../../core/shared/project-file-types'
import { createEditorStates } from '../../../utils/test-utils'
import utils from '../../../utils/utils'
import { EditorDispatch } from '../../editor/action-types'
import {
  EditorStore,
  modifyOpenJsxElementAtStaticPath,
  defaultUserState,
} from '../../editor/store/editor-state'
import { EditorStateContext, EditorStateContextData } from '../../editor/store/store-hook'
import * as TP from '../../../core/shared/template-path'
import { InspectorContextProvider } from '../inspector'
import { getControlStyles, PropertyStatus } from './control-status'
import { InspectorInfo } from './property-path-hooks'
import { ScenePathForTestUiJsFile } from '../../../core/model/test-ui-js-file'
import { emptyComments } from '../../../core/workers/parser-printer/parser-printer-comments'

type UpdateFunctionHelpers = {
  updateStoreWithImmer: (fn: (store: EditorStore) => void) => void
  updateStore: (fn: (store: EditorStore) => EditorStore) => void
}

export function getStoreHook(
  mockDispatch: EditorDispatch,
): EditorStateContextData & UpdateFunctionHelpers {
  const editor = createEditorStates('/src/app.js', [
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
    userState: defaultUserState,
    workers: null as any,
    dispatch: mockDispatch,
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
  selectedViews: Array<TemplatePath>
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
      store.editor.selectedViews[0] as StaticInstancePath,
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
