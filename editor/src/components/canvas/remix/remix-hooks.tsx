import { useAtom } from 'jotai'
import React from 'react'
import { RemixNavigationAtom } from './utopia-remix-root-component'
import { AlwaysFalse, usePubSubAtomReadOnly } from '../../../core/shared/atom-with-pub-sub'
import { UiJsxCanvasCtxAtom } from '../ui-jsx-canvas'

export function useResetRemixApps() {
  const [_, setNavigationData] = useAtom(RemixNavigationAtom)
  let uiJsxCanvasContext = usePubSubAtomReadOnly(UiJsxCanvasCtxAtom, AlwaysFalse)
  return React.useCallback(() => {
    setNavigationData({})
    uiJsxCanvasContext.current.spyValues.metadata = {}
  }, [setNavigationData, uiJsxCanvasContext])
}
