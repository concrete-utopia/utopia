import React from 'react'
import { MultiplayerWrapper } from '../../utils/multiplayer-wrapper'
import { getThreadLocationOnCanvas, useScenes } from './comment-hooks'
import { useEditThreadMetadata, useThreads } from '../../../liveblocks.config'
import { getIdOfScene } from '../../components/canvas/controls/comment-mode/comment-mode-hooks'
import * as EP from '../shared/element-path'
import { isNotNullFiniteRectangle } from '../shared/math-utils'

export const CommentMaintainer = React.memo(() => {
  return (
    <MultiplayerWrapper errorFallback={null} suspenseFallback={null}>
      <CommentMaintainerInner />
    </MultiplayerWrapper>
  )
})
CommentMaintainer.displayName = 'CommentMaintainer'

const CommentMaintainerInner = React.memo(() => {
  useMaintainComments()

  return null
})
CommentMaintainerInner.displayName = 'CommentMaintainerInner'

function useMaintainComments() {
  const { threads } = useThreads()
  const scenes = useScenes()
  const editThreadMetadata = useEditThreadMetadata()

  threads.forEach(async (t): Promise<void> => {
    const sceneId = t.metadata.sceneId
    if (sceneId == null) {
      return
    }

    const scene = scenes.find(
      (s) => getIdOfScene(s) === sceneId || EP.toUid(s.elementPath) === sceneId,
    )

    const globalFrame = scene?.globalFrame ?? null
    if (!isNotNullFiniteRectangle(globalFrame)) {
      return
    }

    const p = getThreadLocationOnCanvas(t, globalFrame)
    if (p.x === t.metadata.x && p.y === t.metadata.y) {
      return
    }

    editThreadMetadata({
      threadId: t.id,
      metadata: {
        x: p.x,
        y: p.y,
      },
    })
  })
}
