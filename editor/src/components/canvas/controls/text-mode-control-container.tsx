import * as React from 'react'
import { InstancePath } from '../../../core/shared/project-file-types'
import { scenePath, staticInstancePath } from '../../../core/shared/template-path'
import { selectComponents } from '../../editor/actions/actions'
import { useEditorState } from '../../editor/store/store-hook'

export function recursivelyGetTemplatePathForDomNode(
  node: HTMLElement,
  workingTemplatePath: Array<string> = [],
): InstancePath {
  const uid = node.dataset.uid
  if (uid != null) {
    const newWorkingTemplatePath = [uid, ...workingTemplatePath]
    const parentElement = node.parentElement
    if (parentElement != null) {
      return recursivelyGetTemplatePathForDomNode(parentElement, newWorkingTemplatePath)
    } else {
      if (workingTemplatePath.length === 0) {
        throw new Error('Could not find parentElement for node')
      } else {
        throw new Error('Could not find scene for node')
      }
    }
  } else {
    const utopiaSceneId = node.dataset.utopiaSceneId
    if (utopiaSceneId != null) {
      return staticInstancePath(scenePath(utopiaSceneId.split('/')), workingTemplatePath)
    } else {
      throw new Error(`Couldn't find a scene for the element`)
    }
  }
}

export const TextModeControlContainer: React.FunctionComponent = () => {
  const dispatch = useEditorState((store) => store.dispatch, 'Text Mode Controls')
  const onSelectionChange = React.useCallback(
    (e: Event) => {
      const selection = document.getSelection()
      if (
        selection != null &&
        selection.focusNode != null &&
        selection.focusNode.nodeType === document.TEXT_NODE
      ) {
        const focusNode = selection.focusNode as CharacterData
        const parentElement = focusNode.parentElement
        if (parentElement != null) {
          dispatch([selectComponents([recursivelyGetTemplatePathForDomNode(parentElement)], false)])
        }
      }
    },
    [dispatch],
  )

  React.useEffect(() => {
    document.addEventListener('selectionchange', onSelectionChange)
  }, [onSelectionChange])
  return null
}
