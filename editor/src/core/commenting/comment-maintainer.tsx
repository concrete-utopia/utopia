import React from 'react'
import { MultiplayerWrapper } from '../../utils/multiplayer-wrapper'
import { getThreadLocationOnCanvas, useCanComment, useScenes } from './comment-hooks'
import { useEditThreadMetadata, useThreads } from '../../../liveblocks.config'
import { getIdOfScene } from '../../components/canvas/controls/comment-mode/comment-mode-hooks'
import * as EP from '../shared/element-path'
import { isNotNullFiniteRectangle } from '../shared/math-utils'
import { isCanvasThreadMetadata, liveblocksThreadMetadataToUtopia } from './comment-types'
import { Substores, useEditorState } from '../../components/editor/store/store-hook'

export const CommentMaintainer = React.memo(() => {
  const canComment = useCanComment()

  if (!canComment) {
    return null
  }

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

  const isInteraction = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.interactionSession != null,
    'useMaintainComments isInteraction',
  )

  React.useEffect(() => {
    if (isInteraction) {
      return
    }

    threads.forEach(async (t): Promise<void> => {
      const metadata = liveblocksThreadMetadataToUtopia(t.metadata)
      if (isCanvasThreadMetadata(metadata)) {
        return
      }
      const { sceneId } = metadata

      const scene = scenes.find(
        (s) => getIdOfScene(s) === sceneId || EP.toUid(s.elementPath) === sceneId,
      )

      const globalFrame = scene?.globalFrame ?? null
      if (!isNotNullFiniteRectangle(globalFrame)) {
        return
      }

      const p = getThreadLocationOnCanvas(t, globalFrame)
      if (p.x === metadata.position.x && p.y === metadata.position.y) {
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
  }, [editThreadMetadata, isInteraction, scenes, threads])
}
