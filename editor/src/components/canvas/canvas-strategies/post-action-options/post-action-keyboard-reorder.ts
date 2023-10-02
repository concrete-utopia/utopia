import type { KeyboardReorderPostActionMenuData } from '../../../editor/store/editor-state'
import {
  doKeyboardReorder,
  isKeyboardReorderApplicable,
} from '../strategies/keyboard-reorder-strategy'
import { accumulatePresses } from '../strategies/shared-keyboard-strategy-helpers'
import type { PostActionChoice } from './post-action-options'

export function KeyboardReorderPostActionChoice(
  data: KeyboardReorderPostActionMenuData,
): PostActionChoice | null {
  const keyboardReorderData = isKeyboardReorderApplicable(
    data.selectedElements,
    data.startingMetadata,
    data.startingElementPathTree,
  )
  if (keyboardReorderData == null) {
    return null
  }

  return {
    name: 'Keyboard reorder',
    id: 'keyboard-reorder',
    run: () => {
      const accumulatedPresses = accumulatePresses(data.keyStates)
      return doKeyboardReorder(accumulatedPresses, keyboardReorderData).commands
    },
  }
}
