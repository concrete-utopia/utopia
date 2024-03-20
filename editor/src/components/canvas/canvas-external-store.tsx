import { DefaultThirdPartyControlDefinitions } from '../../core/third-party/third-party-controls'
import type { PropertyControlsInfo } from '../custom-code/code-file'
import { Substores, useEditorState } from '../editor/store/store-hook'

export const PropertyControlsExportedForTestInspection: { current: PropertyControlsInfo } = {
  current: {},
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
