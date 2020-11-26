import * as React from 'react'
import { UpdateInfo } from '@welldone-software/why-did-you-render'
import * as WDYRTypes from '@welldone-software/why-did-you-render'
// another sad case where the typescript typing is erroneous.
// unfortunately it claims that whyDidYouRender is a default export whereas it's a module export
export const whyDidYouRender = require('@welldone-software/why-did-you-render') as typeof WDYRTypes.default

/**
 * sets up why-did-you-render and
 * @returns [getUpdateCount, getUpdateInfos]
 */
export function setupReactWhyDidYouRender(verbose: boolean = false) {
  const updateInfos: UpdateInfo[] = []
  whyDidYouRender(React, {
    notifier: (updateInfo: UpdateInfo) => {
      updateInfos.push(updateInfo)
      if (verbose) {
        console.info(
          `${updateInfo.displayName} – reason: ${JSON.stringify(updateInfo, null, 2)} \n`,
        )
      }
    },
    logOnDifferentValues: true,
  })

  return [
    () => {
      return updateInfos.length
    },
    () => {
      return updateInfos
    },
  ]
}

export function enableWhyDidYouRenderOnComponent<T extends React.ComponentType<any>>(
  component: T,
): T {
  ;(component as any).whyDidYouRender = true
  return component
}
