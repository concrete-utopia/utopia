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
  useCheckInsertModeForElementType,
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

  const divInsertion = useCheckInsertModeForElementType('div')
  const insertDivCallback = useEnterDrawToInsertForDiv()
  const imgInsertion = useCheckInsertModeForElementType('img')
  const insertImgCallback = useEnterDrawToInsertForImage()
  const spanInsertion = useCheckInsertModeForElementType('span')
  const insertSpanCallback = useEnterDrawToInsertForSpan()
  const buttonInsertion = useCheckInsertModeForElementType('button')
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
          <InsertModeButton iconName='view' primary={divInsertion} onClick={insertDivCallback} />
          <InsertModeButton iconName='image' primary={imgInsertion} onClick={insertImgCallback} />
          <InsertModeButton iconName='text' primary={spanInsertion} onClick={insertSpanCallback} />
          <InsertModeButton
            iconName='button'
            primary={buttonInsertion}
            onClick={insertButtonCallback}
          />
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

interface InsertModeButtonProps {
  iconName: string
  primary: boolean
  onClick: (event: React.MouseEvent<Element>) => void
}
const InsertModeButton = React.memo((props: InsertModeButtonProps) => {
  return (
    <SquareButton primary={props.primary} highlight onClick={props.onClick}>
      <Icn
        category='element'
        type={props.iconName}
        color={props.primary ? 'on-highlight-main' : 'main'}
        width={18}
        height={18}
      />
    </SquareButton>
  )
})
