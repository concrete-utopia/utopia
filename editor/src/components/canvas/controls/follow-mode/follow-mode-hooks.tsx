import type { MouseCallbacks } from '../select-mode/select-mode-hooks'
import { NO_OP } from '../../../../core/shared/utils'
import { useKeepShallowReferenceEquality } from '../../../../utils/react-performance'

export function useFollowModeSelectAndHover(): MouseCallbacks {
  return useKeepShallowReferenceEquality({
    onMouseMove: NO_OP,
    onMouseDown: NO_OP,
    onMouseUp: NO_OP,
  })
}
