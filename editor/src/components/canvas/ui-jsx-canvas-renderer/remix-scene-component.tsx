import React from 'react'
import {
  ActiveRemixSceneAtom,
  UtopiaRemixRootComponent,
} from '../remix/utopia-remix-root-component'
import { UtopiaStyles, useColorTheme } from '../../../uuiui'
import { UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'
import type { ElementPath } from '../../../core/shared/project-file-types'
import invariant from '../../../third-party/remix/invariant'
import * as EP from '../../../core/shared/element-path'
import { useSetAtom } from 'jotai'

export const REMIX_SCENE_TESTID = 'remix-scene'

export interface RemixSceneProps {
  style?: React.CSSProperties
  [UTOPIA_PATH_KEY]?: string
}

export const RemixSceneComponent = React.memo((props: React.PropsWithChildren<RemixSceneProps>) => {
  const colorTheme = useColorTheme()
  const canvasIsLive = false

  const { style, ...remainingProps } = props

  const sceneStyle: React.CSSProperties = {
    position: 'relative',
    backgroundColor: colorTheme.emphasizedBackground.value,
    boxShadow: canvasIsLive
      ? UtopiaStyles.scene.live.boxShadow
      : UtopiaStyles.scene.editing.boxShadow,
    ...UtopiaStyles.backgrounds.checkerboardBackground,
    ...style,
  }

  const pathString = props[UTOPIA_PATH_KEY]
  invariant(pathString, `${UTOPIA_PATH_KEY} prop is missing`)

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
      <UtopiaRemixRootComponent data-path={path} />
    </div>
  )
})
