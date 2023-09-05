import { useAtom } from 'jotai'
import React from 'react'
import {
  ActiveRemixSceneAtom,
  RemixNavigationAtom,
} from '../canvas/remix/utopia-remix-root-component'
import { Substores, useEditorState } from './store/store-hook'
import { FlexColumn, FlexRow, Icn, Tooltip, UtopiaTheme, useColorTheme } from '../../uuiui'
import { stopPropagation } from '../inspector/common/inspector-utils'
import * as EP from '../../core/shared/element-path'

export const RemixNavigationBar = React.memo(() => {
  const [navigationControls] = useAtom(RemixNavigationAtom)
  const [activeRemixScene] = useAtom(ActiveRemixSceneAtom)

  const isLiveMode = useEditorState(
    Substores.restOfEditor,
    (_) => _.editor.mode.type === 'live',
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
        boxShadow: `3px 4px 10px 0px ${UtopiaTheme.panelStyles.panelShadowColor}`,
        pointerEvents: 'initial',
        userSelect: 'none',
        padding: 10,
      }}
      onMouseDown={stopPropagation}
      onClick={stopPropagation}
    >
      <Tooltip title={'Back'}>
        <Icn
          type='icon-semantic-back'
          color='main'
          width={18}
          height={18}
          onMouseDown={back}
          style={{ cursor: 'pointer' }}
        />
      </Tooltip>
      <Tooltip title={'Forward'}>
        <Icn
          type='icon-semantic-fwd'
          color='main'
          width={18}
          height={18}
          onMouseDown={forward}
          style={{ cursor: 'pointer' }}
        />
      </Tooltip>
      <Tooltip title={'Home'}>
        <span style={{ fontSize: 22, cursor: 'pointer' }} onClick={home}>
          🏠
        </span>
      </Tooltip>
      <div
        style={{ backgroundColor: '#f2f3f4', borderRadius: 10, padding: '4px 12px', minWidth: 20 }}
      >
        {pathname}
      </div>
    </FlexRow>
  )
})
