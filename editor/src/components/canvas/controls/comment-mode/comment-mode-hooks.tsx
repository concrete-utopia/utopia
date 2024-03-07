import React from 'react'
import type { MouseCallbacks } from '../select-mode/select-mode-hooks'
import { NO_OP } from '../../../../core/shared/utils'
import { useKeepShallowReferenceEquality } from '../../../../utils/react-performance'
import { useDispatch } from '../../../editor/store/dispatch-context'
import {
  clearHighlightedViews,
  setHighlightedView,
  switchEditorMode,
} from '../../../editor/actions/action-creators'
import type { CommentId } from '../../../editor/editor-modes'
import {
  EditorModes,
  canvasCommentLocation,
  newComment,
  sceneCommentLocation,
} from '../../../editor/editor-modes'
import { useRefEditorState } from '../../../editor/store/store-hook'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import type { CanvasPoint } from '../../../../core/shared/math-utils'
import {
  getLocalPointInNewParentContext,
  isNotNullFiniteRectangle,
  rectContainsPoint,
  windowPoint,
} from '../../../../core/shared/math-utils'
import { isLeft } from '../../../../core/shared/either'
import type { ElementInstanceMetadata } from '../../../../core/shared/element-template'
import { isJSXElement } from '../../../../core/shared/element-template'
import {
  getJSXAttributesAtPath,
  jsxSimpleAttributeToValue,
} from '../../../../core/shared/jsx-attribute-utils'
import { create } from '../../../../core/shared/property-path'
import { optionalMap } from '../../../../core/shared/optional-utils'
import { useScenes } from '../../../../core/commenting/comment-hooks'
import { safeIndex } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import { SceneCommentIdPropName } from '../../../../core/model/scene-id-utils'

export function useCommentModeSelectAndHover(comment: CommentId | null): MouseCallbacks {
  const dispatch = useDispatch()

  const scenes = useScenes()

  const storeRef = useRefEditorState((store) => {
    return {
      scale: store.editor.canvas.scale,
      canvasOffset: store.editor.canvas.roundedCanvasOffset,
    }
  })

  const onMouseMove = React.useCallback(
    (event: React.MouseEvent) => {
      if (comment == null) {
        const loc = windowToCanvasCoordinates(
          storeRef.current.scale,
          storeRef.current.canvasOffset,
          windowPoint({ x: event.clientX, y: event.clientY }),
        )
        const scene = getSceneUnderPoint(loc.canvasPositionRaw, scenes)

        if (scene == null) {
          dispatch([clearHighlightedViews()])
        } else {
          dispatch([setHighlightedView(scene.elementPath)])
        }
      }
    },
    [dispatch, comment, storeRef, scenes],
  )

  const onMouseUp = React.useCallback(
    (event: React.MouseEvent) => {
      if (comment == null) {
        const loc = windowToCanvasCoordinates(
          storeRef.current.scale,
          storeRef.current.canvasOffset,
          windowPoint({ x: event.clientX, y: event.clientY }),
        )
        const scene = getSceneUnderPoint(loc.canvasPositionRaw, scenes)
        const sceneId = optionalMap(getIdOfScene, scene)

        const offset =
          scene != null && isNotNullFiniteRectangle(scene.globalFrame)
            ? getLocalPointInNewParentContext(scene.globalFrame, loc.canvasPositionRounded)
            : null

        if (scene == null || offset == null) {
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

        const sceneIdToUse = sceneId ?? EP.toUid(scene.elementPath)

        dispatch([
          setHighlightedView(scene.elementPath),
          switchEditorMode(
            EditorModes.commentMode(
              newComment(sceneCommentLocation(sceneIdToUse, offset, loc.canvasPositionRounded)),
              'not-dragging',
            ),
          ),
        ])
      } else {
        dispatch([switchEditorMode(EditorModes.commentMode(null, 'not-dragging'))])
      }
    },
    [dispatch, comment, storeRef, scenes],
  )

  return useKeepShallowReferenceEquality({
    onMouseMove: onMouseMove,
    onMouseDown: NO_OP,
    onMouseUp: onMouseUp,
  })
}

export function getSceneUnderPoint(
  location: CanvasPoint,
  scenes: ElementInstanceMetadata[],
): ElementInstanceMetadata | null {
  const scenesUnderTheMouse = scenes.filter((scene) => {
    return (
      isNotNullFiniteRectangle(scene.globalFrame) && rectContainsPoint(scene.globalFrame, location)
    )
  })
  return safeIndex(scenesUnderTheMouse, 0) ?? null // TODO: choose the topmost one in z-order
}

export function getIdOfScene(scene: ElementInstanceMetadata): string | null {
  const sceneElement = scene.element
  if (isLeft(sceneElement) || !isJSXElement(sceneElement.value)) {
    return null
  }

  const idProperty = getJSXAttributesAtPath(
    sceneElement.value.props,
    create(SceneCommentIdPropName),
  )
  const currentValue = optionalMap(jsxSimpleAttributeToValue, idProperty?.attribute)
  if (currentValue == null || isLeft(currentValue) || typeof currentValue.value !== 'string') {
    return null
  }
  return currentValue.value
}
