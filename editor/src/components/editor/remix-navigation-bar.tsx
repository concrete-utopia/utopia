import { useAtom } from 'jotai'
import React from 'react'
import {
  ActiveRemixSceneAtom,
  RemixNavigationAtom,
} from '../canvas/remix/utopia-remix-root-component'
import { Substores, useEditorState } from './store/store-hook'
import { FlexRow, Tooltip, UtopiaTheme, useColorTheme } from '../../uuiui'
import { stopPropagation } from '../inspector/common/inspector-utils'
import * as EP from '../../core/shared/element-path'

export const RemixNavigationBarPathTestId = 'remix-navigation-bar-path'

export const RemixNavigationBarHomeLabel = '(home)'

export type RemixSceneLabelButtonType = 'back' | 'forward' | 'home'

export const RemixNavigationBarButtonTestId = (button: RemixSceneLabelButtonType): string =>
  `remix-navigation-bar-button-${button}`

export const RemixNavigationBar = React.memo(() => {
  const [navigationControls] = useAtom(RemixNavigationAtom)
  const [activeRemixScene] = useAtom(ActiveRemixSceneAtom)

  const isLiveMode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode.type === 'live',
    'RemixNavigationBar isLiveMode',
  )
  const theme = useColorTheme()

  const forward = React.useCallback(
    () => navigationControls[EP.toString(activeRemixScene)]?.forward(),
    [activeRemixScene, navigationControls],
  )
  const back = React.useCallback(
    () => navigationControls[EP.toString(activeRemixScene)]?.back(),
    [activeRemixScene, navigationControls],
  )
  const home = React.useCallback(
    () => navigationControls[EP.toString(activeRemixScene)]?.home(),
    [activeRemixScene, navigationControls],
  )

  const pathname = navigationControls[EP.toString(activeRemixScene)]?.location?.pathname
  const isIndexRoute = pathname === '/'

  const label = isIndexRoute ? RemixNavigationBarHomeLabel : pathname

  if (!isLiveMode || navigationControls == null || pathname == null) {
    return null
  }

  return (
    <FlexRow
      style={{
        gap: 12,
        alignItems: 'center',
        backgroundColor: theme.inspectorBackground.value,
        borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
        boxShadow: `3px 4px 10px 0px ${UtopiaTheme.panelStyles.shadows.medium}`,
        pointerEvents: 'initial',
        userSelect: 'none',
        padding: 4,
        color: theme.fg4.value,
      }}
      onMouseDown={stopPropagation}
      onClick={stopPropagation}
    >
      <Tooltip title={'Back'}>
        <span
          data-testid={RemixNavigationBarButtonTestId('back')}
          style={{ cursor: 'pointer', fontSize: 16 }}
          onMouseDown={back}
        >
          〱
        </span>
      </Tooltip>
      <Tooltip title={'Forward'}>
        <span
          data-testid={RemixNavigationBarButtonTestId('forward')}
          style={{ cursor: 'pointer', fontSize: 16, transform: 'scale(-1, 1)' }}
          onMouseDown={forward}
        >
          〱
        </span>
      </Tooltip>
      <Tooltip title={'Home'}>
        <span
          data-testid={RemixNavigationBarButtonTestId('home')}
          style={{ cursor: 'pointer', fontSize: 16 }}
          onMouseDown={home}
        >
          ／
        </span>
      </Tooltip>
      <div
        data-testid={RemixNavigationBarPathTestId}
        style={{
          backgroundColor: '#f2f3f4',
          borderRadius: 10,
          padding: `${4}px ${12}px`,
          fontSize: 14,
        }}
      >
        {label}
      </div>
    </FlexRow>
  )
})
