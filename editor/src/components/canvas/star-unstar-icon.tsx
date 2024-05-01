/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import * as React from 'react'
import { useDispatch } from '../editor/store/dispatch-context'
import { addNewFeaturedRoute, removeFeaturedRoute } from '../editor/actions/action-creators'
import { Icn, Tooltip, colorTheme } from '../../uuiui'

export interface StarUnstarIconProps {
  url: string
  addedToFavorites: boolean
  selected: boolean
  testId: string
}

export const StarUnstarIcon = React.memo(
  ({ url, addedToFavorites, selected, testId }: StarUnstarIconProps) => {
    const dispatch = useDispatch()

    const onClickAddOrRemoveFavorite = React.useCallback(
      (e: React.MouseEvent) => {
        if (!addedToFavorites) {
          dispatch([addNewFeaturedRoute(url)])
        } else {
          dispatch([removeFeaturedRoute(url)])
        }
        e.stopPropagation()
      },
      [dispatch, url, addedToFavorites],
    )

    const [mouseOver, setMouseOver] = React.useState(false)
    const onMouseOver = React.useCallback(() => {
      setMouseOver(true)
    }, [])
    const onMouseLeave = React.useCallback(() => {
      setMouseOver(false)
    }, [])

    const type: 'star' | 'starfilled' = (() => {
      if (addedToFavorites) {
        return mouseOver ? 'star' : 'starfilled'
      } else {
        return mouseOver ? 'starfilled' : 'star'
      }
    })()

    return (
      <Tooltip title={addedToFavorites ? 'Remove from Favorites' : 'Add to Favorites'}>
        <Icn
          testId={testId}
          onMouseOver={onMouseOver}
          onMouseLeave={onMouseLeave}
          onClick={onClickAddOrRemoveFavorite}
          category='navigator-element'
          type={type}
          color={'main'}
          width={12}
          height={12}
          style={{
            flexShrink: 0,
            opacity: selected ? 1 : undefined,
            color: colorTheme.subduedForeground.value,
            marginLeft: 6,
            marginRight: 6,
            whiteSpace: 'nowrap',
            overflow: 'hidden',
            textOverflow: 'ellipsis',
            cursor: 'pointer',
          }}
          css={{
            opacity: 0,
            '*:hover > &': {
              opacity: 1,
            },
          }}
        />
      </Tooltip>
    )
  },
)
