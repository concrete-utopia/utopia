import React from 'react'
import {
  ActiveRemixSceneAtom,
  UtopiaRemixRootComponent,
} from '../remix/utopia-remix-root-component'
import { UtopiaStyles, useColorTheme } from '../../../uuiui'
import { UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'
import * as EP from '../../../core/shared/element-path'
import { useSetAtom } from 'jotai'
import type { AppLoadContext } from '@remix-run/server-runtime'

export const REMIX_SCENE_TESTID = 'remix-scene'

export interface RemixSceneProps {
  style?: React.CSSProperties
  [UTOPIA_PATH_KEY]?: string
  getLoadContext?: (request: Request) => Promise<AppLoadContext> | AppLoadContext
  startingRoute?: string
}

export const RemixSceneComponent = React.memo((props: React.PropsWithChildren<RemixSceneProps>) => {
  const colorTheme = useColorTheme()

  const { style, getLoadContext, startingRoute, ...remainingProps } = props

  const sceneStyle: React.CSSProperties = {
    position: 'relative',
    backgroundColor: colorTheme.emphasizedBackground.value,
    boxShadow: UtopiaStyles.shadowStyles.grounded.boxShadow,
    ...UtopiaStyles.backgrounds.checkerboardBackground,
    ...style,
  }

  const pathString = props[UTOPIA_PATH_KEY]
  if (pathString == null) {
    throw new Error('Cannot render `UtopiaRemixRootComponent` without a path prop')
  }

  const setActiveRemixScene = useSetAtom(ActiveRemixSceneAtom)

  const path = React.useMemo(() => EP.fromString(pathString), [pathString])

  const onMouseDown = React.useCallback(() => {
    setActiveRemixScene(path)
  }, [path, setActiveRemixScene])

  return (
    <div
      data-testid={REMIX_SCENE_TESTID}
      {...remainingProps}
      style={sceneStyle}
      onMouseDown={onMouseDown}
    >
      <UtopiaRemixRootComponent
        data-path={path}
        getLoadContext={getLoadContext}
        startingRoute={startingRoute}
      />
    </div>
  )
})
RemixSceneComponent.displayName = 'RemixSceneComponent'
