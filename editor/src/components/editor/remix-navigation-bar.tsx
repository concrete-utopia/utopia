/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import { useAtom } from 'jotai'
import React from 'react'
import {
  ActiveRemixSceneAtom,
  RemixNavigationAtom,
} from '../canvas/remix/utopia-remix-root-component'
import { Substores, useEditorState } from './store/store-hook'
import { FlexRow, Tooltip, colorTheme } from '../../uuiui'
import { stopPropagation } from '../inspector/common/inspector-utils'
import * as EP from '../../core/shared/element-path'
import { getRemixLocationLabel, getRemixUrlFromLocation } from '../canvas/remix/remix-utils'

export const RemixNavigationBarPathTestId = 'remix-navigation-bar-path'

export type RemixSceneLabelButtonType = 'back' | 'forward' | 'home'

export const RemixNavigationBarButtonTestId = (button: RemixSceneLabelButtonType): string =>
  `remix-navigation-bar-button-${button}`

export const RemixNavigationBar = React.memo(() => {
  const [navigationControls] = useAtom(RemixNavigationAtom)
  const [activeRemixScene] = useAtom(ActiveRemixSceneAtom)

  const isSelectOrLiveMode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode.type === 'select' || store.editor.mode.type === 'live',
    'RemixNavigationBar isLiveMode',
  )

  const forward = React.useCallback(
    () => void navigationControls[EP.toString(activeRemixScene)]?.forward(),
    [activeRemixScene, navigationControls],
  )
  const back = React.useCallback(
    () => void navigationControls[EP.toString(activeRemixScene)]?.back(),
    [activeRemixScene, navigationControls],
  )
  const home = React.useCallback(
    () => void navigationControls[EP.toString(activeRemixScene)]?.home(),
    [activeRemixScene, navigationControls],
  )

  const url = getRemixUrlFromLocation(navigationControls[EP.toString(activeRemixScene)]?.location)

  const label = getRemixLocationLabel(url)

  if (!isSelectOrLiveMode || navigationControls == null || url == null) {
    return null
  }

  return (
    <FlexRow
      style={{
        gap: 10,
        alignSelf: 'stretch',
        alignItems: 'center',
        justifyContent: 'center',
        pointerEvents: 'initial',
        userSelect: 'none',
        padding: '0 8px',
      }}
      onMouseDown={stopPropagation}
      onClick={stopPropagation}
    >
      <Tooltip title={'Back'} placement='bottom'>
        <span
          data-testid={RemixNavigationBarButtonTestId('back')}
          style={{ cursor: 'pointer', fontSize: 12 }}
          css={{
            '&:hover': {
              color: colorTheme.dynamicBlue.value,
            },
          }}
          onClick={back}
        >
          〱
        </span>
      </Tooltip>
      <Tooltip title={'Forward'} placement='bottom'>
        <span
          data-testid={RemixNavigationBarButtonTestId('forward')}
          style={{ cursor: 'pointer', fontSize: 12, transform: 'scale(-1, 1)' }}
          css={{
            '&:hover': {
              color: colorTheme.dynamicBlue.value,
            },
          }}
          onClick={forward}
        >
          〱
        </span>
      </Tooltip>
      <Tooltip title={'Home'} placement='bottom'>
        <span
          data-testid={RemixNavigationBarButtonTestId('home')}
          style={{ cursor: 'pointer', fontSize: 16 }}
          css={{
            '&:hover': {
              color: colorTheme.dynamicBlue.value,
            },
          }}
          onClick={home}
        >
          ⛫
        </span>
      </Tooltip>
      <div
        data-testid={RemixNavigationBarPathTestId}
        style={{
          backgroundColor: colorTheme.bg3.value,
          borderRadius: 20,
          padding: '2px 10px',
          fontSize: 11,
          minWidth: 150,
        }}
      >
        {label}
      </div>
    </FlexRow>
  )
})
