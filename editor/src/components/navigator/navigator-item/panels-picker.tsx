/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { CheckboxInput } from '../../../uuiui'
import { FlexRow } from 'utopia-api'

export const PanelsPicker = React.memo(() => {
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
      // onKeyDown={onKeyDown}
      // ref={menuRef}
      // data-testid={ComponentPickerTestId}
    >
      <CheckboxRow label="Navigator" />
      <CheckboxRow label="Code Editor" />
      <CheckboxRow label="Inspector" />
    </div>
  )
})

interface CheckboxRowProps {
  label: string
}

const CheckboxRow = React.memo((props: CheckboxRowProps) => {
  const { label } = props

  return (
    <FlexRow
      css={{}}
      style={{
        alignItems: 'center',
        cursor: 'pointer',
        borderRadius: 4,
        // indentation!
        paddingLeft: 8,
        color: '#EEE',
      }}
      // onClick={onItemClick(component.value)}
      // onMouseOver={onItemHover(component.value)}
    >
      <FlexRow css={{ gap: 10, height: 28, alignItems: 'center' }}>
        <CheckboxInput
          style={{ marginRight: 8 }}
          id='showCodeEditorLabel'
          checked={false}
          // onChange={() => {}}
        />
        <span>{label}</span>
      </FlexRow>
    </FlexRow>
  )
})