import React from 'react'
import { DefaultThirdPartyControlDefinitions } from '../../core/third-party/third-party-controls'
import type { PropertyControlsInfo } from '../custom-code/code-file'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { useDispatch } from '../editor/store/dispatch-context'
import { updatePropertyControlsInfo } from '../editor/actions/action-creators'

export const PropertyControlsExportedForTestInspection: { current: PropertyControlsInfo } = {
  current: {},
}

let listeners: Array<() => void> = []

export const propControlsStore = {
  setPropertyControls(propertyControls: PropertyControlsInfo) {
    PropertyControlsExportedForTestInspection.current = propertyControls
    emitChange()
  },
  subscribe(listener: () => void) {
    listeners = [...listeners, listener]
    return () => {
      listeners = listeners.filter((l) => l !== listener)
    }
  },
  getSnapshot(): PropertyControlsInfo {
    return PropertyControlsExportedForTestInspection.current
  },
}

function emitChange() {
  for (let listener of listeners) {
    listener()
  }
}

export function usePropertyControlsInfo(): PropertyControlsInfo {
  // TODO don't forget about DefaultThirdPartyControlDefinitions
  return React.useSyncExternalStore(propControlsStore.subscribe, propControlsStore.getSnapshot)
}

export function useDispatchWhenPropertyControlsInfoChanges() {
  const dispatch = useDispatch()

  const previousPropControls = React.useRef<PropertyControlsInfo | null>(null)
  const propControls = usePropertyControlsInfo()
  if (previousPropControls.current !== propControls) {
    dispatch([updatePropertyControlsInfo(propControls)])
  }

  previousPropControls.current = propControls
}
