import type { MouseCallbacks } from '../select-mode/select-mode-hooks'
import { NO_OP } from '../../../../core/shared/utils'

const noop: MouseCallbacks = {
  onMouseMove: NO_OP,
  onMouseDown: NO_OP,
  onMouseUp: NO_OP,
}

export function usePanelsModeSelectAndHover(): MouseCallbacks {
  return noop
}
