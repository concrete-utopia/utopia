import React from 'react'
import type { Notice } from '../../components/common/notice'
import { notice } from '../../components/common/notice'
import { stripNulls } from '../shared/array-utils'
import type { PropertyPath } from '../shared/project-file-types'
import * as PP from '../shared/property-path'

export function createStylePostActionToast(
  name: string,
  originalPropertyPaths: PropertyPath[],
  updatedPropertyPaths: PropertyPath[],
): Array<Notice> {
  const removedProps = originalPropertyPaths.filter((x) => !updatedPropertyPaths.includes(x)) // R.difference(originalPropertyPaths, updatedPropertyPaths)
  const addedProps = updatedPropertyPaths.filter((x) => !originalPropertyPaths.includes(x)) // R.difference(updatedPropertyPaths, originalPropertyPaths)

  if (removedProps.length > 0 || addedProps.length > 0) {
    const added =
      addedProps.length === 0 ? null : `Added: ${addedProps.map(PP.toString).join(', ')}`
    const removed =
      removedProps.length === 0 ? null : `Removed: ${removedProps.map(PP.toString).join(', ')}`

    const addedRemovedProps = stripNulls([added, removed]).join('\n')

    return [
      notice(
        // this is a createElement because we have a dependency-cruiser rule that the actions cannot import .tsx files.
        // this is a valid exception here
        React.createElement(
          'div',
          { style: { whiteSpace: 'pre' } },
          `${name.trim()} has changed props:\n${addedRemovedProps}`,
        ),
        'PRIMARY',
        false,
      ),
    ]
  } else {
    return []
  }
}
