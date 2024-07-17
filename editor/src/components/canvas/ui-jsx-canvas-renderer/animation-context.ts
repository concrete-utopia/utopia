import type {
  ElementOrSelector,
  DOMKeyframesDefinition,
  DynamicAnimationOptions,
  AnimationPlaybackControls,
  AnimationScope,
} from 'framer-motion'
import React, { useContext } from 'react'
import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getUtopiaID } from '../../../core/shared/uid-utils'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { mapDropNulls } from '../../../core/shared/array-utils'

export type AnimationCtx = {
  scope: AnimationScope | null
  animate:
    | ((
        value: ElementOrSelector,
        keyframes: DOMKeyframesDefinition,
        options?: DynamicAnimationOptions | undefined,
      ) => AnimationPlaybackControls)
    | null
}

export const AnimationContext = React.createContext<AnimationCtx>({
  scope: null,
  animate: null,
})

export function useCanvasAnimation(paths: ElementPath[]) {
  const ctx = useContext(AnimationContext)

  const uids = useEditorState(
    Substores.metadata,
    (store) => {
      return mapDropNulls((path) => {
        const element = MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path)
        if (element == null) {
          return null
        }
        return getUtopiaID(element)
      }, paths)
    },
    'useCanvasAnimation uids',
  )

  const selector = React.useMemo(() => {
    return uids.map((uid) => `[data-uid='${uid}']`).join(',')
  }, [uids])

  const elements = React.useMemo(
    () => (selector === '' ? [] : document.querySelectorAll(selector)),
    [selector],
  )

  return React.useCallback(
    (keyframes: DOMKeyframesDefinition, options?: DynamicAnimationOptions) => {
      if (ctx.animate == null || elements.length === 0) {
        return
      }
      void ctx.animate(elements, keyframes, options)
    },
    [ctx, elements],
  )
}
