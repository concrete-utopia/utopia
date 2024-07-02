/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { CheckboxInput, colorTheme } from '../../../uuiui'
import { FlexRow } from 'utopia-api'
import { Substores, useEditorState } from '../../../components/editor/store/store-hook'
import { togglePanel } from '../../../components/editor/actions/action-creators'
import { useDispatch } from '../../../components/editor/store/dispatch-context'

export const PanelsPicker = React.memo(() => {
  const dispatch = useDispatch()

  const { codeEditorVisible, navigatorVisible, inspectorVisible } = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return {
        codeEditorVisible: store.editor.interfaceDesigner.codePaneVisible,
        navigatorVisible: store.editor.leftMenu.visible,
        inspectorVisible: store.editor.rightMenu.visible,
      }
    },
    'storedLayoutToResolvedPanels panel visibility',
  )

  const toggleNavigator = React.useCallback(() => {
    dispatch([togglePanel('leftmenu')])
  }, [dispatch])

  const toggleCodeEditor = React.useCallback(() => {
    dispatch([togglePanel('codeEditor')])
  }, [dispatch])

  const toggleInspector = React.useCallback(() => {
    dispatch([togglePanel('rightmenu')])
  }, [dispatch])

  return (
    <div
      style={{
        display: 'flex',
        flexDirection: 'column',
        gap: 0,
        width: '100%',
        height: '100%',
        borderRadius: 10,
        padding: '8px 0',
      }}
    >
      <CheckboxRow label='Navigator' checked={navigatorVisible} onChange={toggleNavigator} />
      <CheckboxRow label='Code Editor' checked={codeEditorVisible} onChange={toggleCodeEditor} />
      <CheckboxRow label='Inspector' checked={inspectorVisible} onChange={toggleInspector} />
    </div>
  )
})

interface CheckboxRowProps {
  checked: boolean
  label: string
  onChange: () => void
}

const CheckboxRow = React.memo((props: CheckboxRowProps) => {
  const { checked, label, onChange } = props

  const id = `input-${label}`

  return (
    <FlexRow
      css={{}}
      style={{
        alignItems: 'center',
        cursor: 'pointer',
        borderRadius: 4,
        paddingLeft: 8,
        color: '#EEE',
      }}
    >
      <FlexRow css={{ gap: 10, height: 28, alignItems: 'center' }}>
        <CheckboxInput id={id} checked={checked} onChange={onChange} />
        <label htmlFor={id} style={{ color: colorTheme.textColor.value }}>
          {label}
        </label>
      </FlexRow>
    </FlexRow>
  )
})
