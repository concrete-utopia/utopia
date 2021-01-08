import * as React from 'react'
import { NO_OP } from '../../../../core/shared/utils'
import { useKeepShallowReferenceEquality } from '../../../../utils/react-performance'
import { useHighlightCallbacks } from './select-mode-hooks'

/**
 * const allPaths = MetadataUtils.getAllPaths(this.props.componentMetadata)
    const insertTargets = allPaths.filter((path) => {
      if (TP.isScenePath(path)) {
        // TODO Scene Implementation
        return false
      } else {
        return (
          (insertionSubjectIsJSXElement(this.props.mode.subject) ||
            insertionSubjectIsDragAndDrop(this.props.mode.subject)) &&
          MetadataUtils.targetSupportsChildren(
            this.props.imports,
            this.props.componentMetadata,
            path,
          )
        )
      }
    })
 */

export function useInsertModeSelectAndHover(): {
  onMouseOver: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
  onMouseOut: () => void
  onMouseDown: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
} {
  const { onMouseOver, onMouseOut } = useHighlightCallbacks(true)

  return useKeepShallowReferenceEquality({
    onMouseOver: onMouseOver,
    onMouseOut: onMouseOut,
    onMouseDown: NO_OP,
  })
}
