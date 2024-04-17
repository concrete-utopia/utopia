import type { ElementPath } from '../../../core/shared/project-file-types'
import { LeftMenuTab } from '../../editor/store/editor-state'

export function nextSelectedTab(
  currentLeftMenuTab: LeftMenuTab,
  newSelectedPaths: ElementPath[],
): LeftMenuTab {
  if (newSelectedPaths.length === 0) {
    return currentLeftMenuTab
  }

  if (currentLeftMenuTab === LeftMenuTab.Pages) {
    return LeftMenuTab.Pages
  }

  return LeftMenuTab.Navigator
}
