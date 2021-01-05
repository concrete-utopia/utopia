import * as React from 'react'
import { MenuIcons, SimpleFlexRow, Tooltip } from '../../uuiui'
import { RightMenuTile } from '../canvas/right-menu'
import { useEditorState } from './store/store-hook'
import * as EditorActions from '../editor/actions/action-creators'
import { betterReactMemo } from '../../uuiui-deps'
import { FormulaBar } from '../canvas/controls/formula-bar'

export const TopMenu = betterReactMemo('TopMenu', () => {
  const dispatch = useEditorState((store) => store.dispatch, 'TopMenu dispatch')
  const navigatorPosition = useEditorState(
    (store) => store.editor.navigator.position,
    'TopMenu navigatorPosition',
  )

  const onClickNavigateTab = React.useCallback(() => {
    dispatch([EditorActions.togglePanel('navigatorPane')])
  }, [dispatch])

  return (
    <SimpleFlexRow>
      <Tooltip title={'Navigator'} placement={'bottom'}>
        <span>
          <RightMenuTile
            selected={navigatorPosition !== 'hidden'}
            highlightSelected={false}
            icon={<MenuIcons.Project />}
            onClick={onClickNavigateTab}
          />
        </span>
      </Tooltip>
      <FormulaBar />
    </SimpleFlexRow>
  )
})
