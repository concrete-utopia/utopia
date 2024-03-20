import { DefaultThirdPartyControlDefinitions } from '../../core/third-party/third-party-controls'
import type { PropertyControlsInfo } from '../custom-code/code-file'
import { Substores, useEditorState } from '../editor/store/store-hook'

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
  const override = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.propertyControlsInfoOverride,
    'usePropertyControlsInfo override',
  )

  // TODO don't forget about DefaultThirdPartyControlDefinitions
  const defaultControls = DefaultThirdPartyControlDefinitions

  return override ?? (null as any)
}
