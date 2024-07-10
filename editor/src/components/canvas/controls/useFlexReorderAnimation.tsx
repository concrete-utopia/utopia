import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { nullIfInfinity, rectanglesEqual } from '../../../core/shared/math-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { Substores, useEditorState } from '../../editor/store/store-hook'

export function useFlexReorderAnimation(parentPath: ElementPath, childPath: ElementPath) {
  const childrenPaths = useEditorState(
    Substores.metadata,
    (store) =>
      MetadataUtils.getChildrenPathsOrdered(
        store.editor.jsxMetadata,
        store.editor.elementPathTree,
        parentPath,
      ),
    '',
  )

  const childrenPathsStrings = childrenPaths.map((p) => EP.toString(p)).join('')

  const [lastChildrenPaths, setLastChildrenPaths] = React.useState(childrenPaths)

  const currentFrame = useEditorState(
    Substores.metadata,
    (store) =>
      nullIfInfinity(
        MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, childPath)?.globalFrame,
      ),
    '',
  )

  const [lastFrame, setLastFrame] = React.useState(currentFrame)

  const animate = useCanvasAnimations(childrenPaths)

  React.useEffect(() => {
    if (
      lastFrame == null ||
      currentFrame == null ||
      rectanglesEqual(lastFrame, currentFrame) ||
      childrenPaths === lastChildrenPaths
    ) {
      return
    }

    void animate(
      {
        x: [lastFrame.x - currentFrame.x, 0],
        y: [lastFrame.y - currentFrame.y, 0],
      },
      { duration: 0.15 },
    )

    setLastFrame(currentFrame)
    setLastChildrenPaths(childrenPaths)
  }, [animate, childrenPaths, currentFrame, lastChildrenPaths, lastFrame])
}
