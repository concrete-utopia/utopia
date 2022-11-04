/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'
import React from 'react'
import { createSelector } from 'reselect'
import { FLOATING_PREVIEW_BASE_URL } from '../../common/env-vars'
import { getGithubFileChangesCount, githubFileChangesSelector } from '../../core/shared/github'
import { shareURLForProject } from '../../core/shared/utils'
import { FlexColumn, IcnProps, MenuIcons, Tooltip, useColorTheme, UtopiaTheme } from '../../uuiui'
import { EditorAction } from '../editor/action-types'
import { setLeftMenuTab } from '../editor/actions/action-creators'
import { LeftMenuTab } from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'

const MenuTileBadge = ({ text }: { text: string }) => {
  const colorTheme = useColorTheme()
  return (
    <div
      style={{
        position: 'absolute',
        top: 0,
        right: 0,
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        backgroundColor: colorTheme.contextMenuHighlightBackground.value,
        minWidth: 13,
        height: 13,
        paddingLeft: 3,
        paddingRight: 3,
        borderRadius: '10px',
        fontSize: 7,
        fontWeight: 800,
        color: '#fff',
      }}
    >
      {text}
    </div>
  )
}

interface TileProps {
  size: keyof typeof UtopiaTheme.layout.rowHeight
}

const GITHUB_FILE_CHANGES_BADGE_LIMIT = 99

const Tile = styled.div<TileProps>((props) => ({
  display: 'flex',
  flexDirection: 'column',
  justifyContent: 'center',
  alignItems: 'center',
  width: props.size,
}))

export interface MenuTileProps extends React.HTMLAttributes<HTMLDivElement>, TileProps {
  selected: boolean
  icon: React.ReactElement<IcnProps>
  size: keyof typeof UtopiaTheme.layout.rowHeight
  badge?: string
}

export const MenuTile: React.FunctionComponent<React.PropsWithChildren<MenuTileProps>> = (
  props,
) => {
  const colorTheme = useColorTheme()

  return (
    <Tile
      size={props.size}
      css={{
        height: 44,
        transition: 'all .1s ease-in-out',
        borderLeft: props.selected ? `2px solid ${colorTheme.primary}` : '2px solid transparent',

        cursor: 'pointer',
        '& > *': {
          opacity: props.selected ? 1 : 0.5,
          transform: props.selected ? 'translateX(1px)' : 'inherit',
        },
        '&:hover > *': {
          opacity: 1,
          transform: props.selected ? 'translateX(1px)' : 'inherit',
        },
        '&:active > *': {
          transform: 'translateX(1px)',
          opacity: 1,
        },
      }}
      onClick={props.onClick}
    >
      <div
        style={{
          ...props.style,
          borderRadius: 1,
          width: 28,
          height: 28,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          position: 'relative',
        }}
      >
        {React.cloneElement(props.icon, {
          color: props.selected ? 'primary' : 'secondary',
        })}
        {props.badge && <MenuTileBadge text={props.badge} />}
      </div>
    </Tile>
  )
}
