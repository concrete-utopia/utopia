import * as React from 'react'
import { AlwaysTrue, usePubSubAtomReadOnly } from '../../core/shared/atom-with-pub-sub'
import {
  FlexColumn,
  FlexRow,
  Icn,
  IcnSpacer,
  LargerIcons,
  SquareButton,
  useColorTheme,
  UtopiaStyles,
  UtopiaTheme,
} from '../../uuiui'
import {
  useEnterDrawToInsertForButton,
  useEnterDrawToInsertForDiv,
  useEnterDrawToInsertForImage,
  useEnterDrawToInsertForSpan,
} from './insert-callbacks'
import { NavigatorWidthAtom } from './store/editor-state'
import { useEditorState } from './store/store-hook'

export const CanvasToolbar = React.memo(() => {
  const theme = useColorTheme()

  const navigatorWidth = usePubSubAtomReadOnly(NavigatorWidthAtom, AlwaysTrue)
  const navigatorVisible = useEditorState(
    (store) => !store.editor.navigator.minimised,
    'CanvasToolbar navigatorVisible',
  )
  const effectiveNavigatorWidth = navigatorVisible ? navigatorWidth : 0

  const topMenuHeight = UtopiaTheme.layout.rowHeight.normal

  const insertDivCallback = useEnterDrawToInsertForDiv()
  const insertSpanCallback = useEnterDrawToInsertForSpan()
  const insertImgCallback = useEnterDrawToInsertForImage()
  const insertButtonCallback = useEnterDrawToInsertForButton()

  return (
    <FlexColumn
      style={{
        position: 'absolute',
        top: topMenuHeight + 8,
        left: effectiveNavigatorWidth + 8,
        alignItems: 'stretch',
        width: 64,
        padding: 4,
        gap: 4,
        borderRadius: 4,
        backgroundColor: theme.bg0.value,
        boxShadow: UtopiaStyles.popup.boxShadow,
      }}
    >
      <FlexColumn>
        <header style={{ paddingLeft: 4 }}>Insert</header>
        <FlexRow style={{ flexWrap: 'wrap', gap: 4, padding: 4 }}>
          <SquareButton highlight onClick={insertDivCallback}>
            <Icn category='element' type='view' color='main' width={18} height={18} />
          </SquareButton>
          <SquareButton highlight onClick={insertImgCallback}>
            <Icn category='element' type='image' color='main' width={18} height={18} />
          </SquareButton>
          <SquareButton highlight onClick={insertSpanCallback}>
            <Icn category='element' type='text' color='main' width={18} height={18} />
          </SquareButton>
          <SquareButton highlight onClick={insertButtonCallback}>
            <Icn category='element' type='button' color='main' width={18} height={18} />
          </SquareButton>
          <IcnSpacer height={0} width={'100%'} />
          <SquareButton highlight>
            <Icn category='element' type='componentinstance' color='main' width={18} height={18} />
          </SquareButton>
          <SquareButton highlight>…</SquareButton>
        </FlexRow>
      </FlexColumn>
    </FlexColumn>
  )
})
