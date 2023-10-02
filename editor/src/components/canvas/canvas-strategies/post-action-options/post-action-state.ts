import type { KeyCharacter } from '../../../../utils/keyboard'
import type { Modifiers } from '../../../../utils/modifiers'
import type { PostActionMenuData } from '../../../editor/store/editor-state'

export function updatePostActionSessionStateViaKeyboard(
  postActionMenuData: PostActionMenuData,
  keyPressed: KeyCharacter,
  modifiers: Modifiers,
): PostActionMenuData {
  if (postActionMenuData.type !== 'KEYBOARD_REORDER') {
    return postActionMenuData
  }

  return {
    ...postActionMenuData,
    keyStates: [
      ...postActionMenuData.keyStates,
      { keysPressed: new Set([keyPressed]), modifiers: modifiers },
    ],
  }
}
