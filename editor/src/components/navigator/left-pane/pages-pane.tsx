import { useAtom } from 'jotai'
import React from 'react'
import { matchRoutes, type DataRouteObject } from 'react-router'
import * as EP from '../../../core/shared/element-path'
import { FlexColumn, FlexRow, UtopiaTheme, colorTheme } from '../../../uuiui'
import { RemixIndexPathLabel } from '../../canvas/remix/remix-utils'
import {
  ActiveRemixSceneAtom,
  RemixNavigationAtom,
} from '../../canvas/remix/utopia-remix-root-component'
import { Substores, useEditorState } from '../../editor/store/store-hook'

const sortByFilename = (a: DataRouteObject, b: DataRouteObject) =>
  a.path?.localeCompare(b.path ?? '') ?? 0

export const PagesPane = React.memo((props) => {
  const remixRoutes: Array<DataRouteObject> = useEditorState(
    Substores.derived,
    (store) =>
      (store.derived.remixData?.routes ?? [])
        .flatMap((route) => route.children ?? [])
        .sort(sortByFilename),
    'PagesPane remixRoutes',
  )

  const [navigationControls] = useAtom(RemixNavigationAtom)
  const [activeRemixScene] = useAtom(ActiveRemixSceneAtom)

  const pathname = navigationControls[EP.toString(activeRemixScene)]?.location?.pathname ?? ''

  const matchResult = matchRoutes(remixRoutes, pathname)

  return (
    <FlexColumn style={{ height: '100%', overflowY: 'scroll' }}>
      {remixRoutes.map((route: DataRouteObject, index) => {
        const { path } = route
        const pathToDisplay = path ?? RemixIndexPathLabel
        return (
          <PageRouteEntry
            key={path}
            filepath={pathToDisplay}
            active={matchResult?.[0].route.path === path}
          />
        )
      })}
    </FlexColumn>
  )
})

interface PageRouteEntryProps {
  filepath: string
  active: boolean
}
const PageRouteEntry = React.memo<PageRouteEntryProps>((props) => {
  return (
    <FlexRow
      style={{
        flexShrink: 0,
        color: colorTheme.neutralForeground.value,
        backgroundColor: props.active ? colorTheme.subtleBackground.value : 'transparent',
        marginLeft: 8,
        marginRight: 8,
        paddingLeft: 3,
        paddingTop: 3,
        paddingBottom: 3,
        height: UtopiaTheme.layout.rowHeight.smaller,
        alignItems: 'center',
        justifyContent: 'space-between',
        borderRadius: 2,
        position: 'relative',
      }}
    >
      {/* TODO if we want renaming, cannibalize it from FileBrowserItem */}
      <span
        style={{
          flex: 1,
          marginLeft: 6,
          display: 'inline-block',
          whiteSpace: 'nowrap',
          overflow: 'hidden',
          textOverflow: 'ellipsis',
        }}
      >
        {props.filepath === '' ? '(index)' : props.filepath}
      </span>
    </FlexRow>
  )
})
