import React from 'react'
import type { MouseCallbacks } from '../select-mode/select-mode-hooks'
import { NO_OP } from '../../../../core/shared/utils'
import { useKeepShallowReferenceEquality } from '../../../../utils/react-performance'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { switchEditorMode } from '../../../editor/actions/action-creators'
import type { CommentId } from '../../../editor/editor-modes'
import {
  EditorModes,
  canvasCommentLocation,
  newComment,
  sceneCommentLocation,
} from '../../../editor/editor-modes'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import {
  canvasPoint,
  isNotNullFiniteRectangle,
  offsetPoint,
  pointDifference,
  rectContainsPoint,
  windowPoint,
} from '../../../../core/shared/math-utils'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { isLeft } from '../../../../core/shared/either'
import type { ElementInstanceMetadata } from '../../../../core/shared/element-template'
import { isJSXElement } from '../../../../core/shared/element-template'
import {
  getJSXAttributesAtPath,
  jsxSimpleAttributeToValue,
} from '../../../../core/shared/jsx-attributes'
import { create } from '../../../../core/shared/property-path'
import { optionalMap } from '../../../../core/shared/optional-utils'
import { useScenesWithId } from '../../../../core/commenting/comment-hooks'

export function useCommentModeSelectAndHover(comment: CommentId | null): MouseCallbacks {
  const dispatch = useDispatch()

  const scenes = useScenesWithId()

  const storeRef = useRefEditorState((store) => {
    return {
      scale: store.editor.canvas.scale,
      canvasOffset: store.editor.canvas.roundedCanvasOffset,
    }
  })

  const onMouseUp = React.useCallback(
    (event: React.MouseEvent) => {
      if (comment == null) {
        const loc = windowToCanvasCoordinates(
          storeRef.current.scale,
          storeRef.current.canvasOffset,
          windowPoint({ x: event.clientX, y: event.clientY }),
        )

        const scenesUnderTheMouse = scenes.filter((scene) => {
          const sceneId = getIdOfScene(scene)
          return (
            sceneId != null &&
            isNotNullFiniteRectangle(scene.globalFrame) &&
            rectContainsPoint(scene.globalFrame, loc.canvasPositionRaw)
          )
        })
        const scene = scenesUnderTheMouse[0]
        const sceneId = getIdOfScene(scene)
        const offset =
          sceneId != null && isNotNullFiniteRectangle(scene.globalFrame)
            ? pointDifference(scene.globalFrame, loc.canvasPositionRounded)
            : null

        if (scene == null || sceneId == null || offset == null) {
          dispatch([
            switchEditorMode(
              EditorModes.commentMode(
                newComment(canvasCommentLocation(loc.canvasPositionRounded)),
                'not-dragging',
              ),
            ),
          ])
          return
        }

        dispatch([
          switchEditorMode(
            EditorModes.commentMode(
              newComment(sceneCommentLocation(sceneId, offset)),
              'not-dragging',
            ),
          ),
        ])
      } else {
        dispatch([switchEditorMode(EditorModes.selectMode(null, false, 'none'))])
      }
    },
    [dispatch, comment, storeRef, scenes],
  )

  return useKeepShallowReferenceEquality({
    onMouseMove: NO_OP,
    onMouseDown: NO_OP,
    onMouseUp: onMouseUp,
  })
}

export function getIdOfScene(scene: ElementInstanceMetadata): string | null {
  const sceneElement = scene.element
  if (isLeft(sceneElement) || !isJSXElement(sceneElement.value)) {
    return null
  }

  const idProperty = getJSXAttributesAtPath(sceneElement.value.props, create('id'))
  const currentValue = optionalMap(jsxSimpleAttributeToValue, idProperty?.attribute)
  if (currentValue == null || isLeft(currentValue) || typeof currentValue.value !== 'string') {
    return null
  }
  return currentValue.value
}
