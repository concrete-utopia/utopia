import { useAtom } from 'jotai'
import React from 'react'
import { RemixNavigationAtom } from './utopia-remix-root-component'
import { AlwaysFalse, usePubSubAtomReadOnly } from '../../../core/shared/atom-with-pub-sub'
import { UiJsxCanvasCtxAtom } from '../ui-jsx-canvas'

export function useResetRemixApps() {
  const [navigationData] = useAtom(RemixNavigationAtom)
  let uiJsxCanvasContext = usePubSubAtomReadOnly(UiJsxCanvasCtxAtom, AlwaysFalse)
  return React.useCallback(() => {
    uiJsxCanvasContext.current.spyValues.metadata = {}
    Object.values(navigationData).forEach((navData) => navData.home())
  }, [navigationData, uiJsxCanvasContext])
}
