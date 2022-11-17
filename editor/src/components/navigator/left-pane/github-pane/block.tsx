/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { colorTheme, FlexColumn, FlexRow, UtopiaTheme } from '../../../../uuiui'
import { UIGridRow } from '../../../inspector/widgets/ui-grid-row'

export const IndicatorLight = React.memo((props: { status: BlockStatus }) => (
  <div
    style={{
      zIndex: 99999,
      width: 10,
      height: 10,
      border: '1px solid black',
      borderRadius: 20,
      background: getIndicatorColor(props.status),
    }}
  />
))

function getIndicatorColor(status: BlockStatus): string {
  switch (status) {
    case 'incomplete':
      return '#FFFFFF00'
    case 'successful':
      return '#1FCCB7'
    case 'failed':
      return '#FF7759'
    case 'pending':
      return 'conic-gradient(from 180deg at 50% 50%, #2D2E33 0deg, #FFFFFF 181.87deg, #FFFFFF 360deg)'
    default:
      const _exhaustiveCheck: never = status
      throw new Error(`invalid state ${status}`)
  }
}

export type BlockStatus = 'incomplete' | 'successful' | 'failed' | 'pending'

export type BlockProps = {
  title: string
  subtitle?: string | JSX.Element
  status: BlockStatus
  first?: boolean
  expanded: boolean
  last?: boolean
  children: any
  onClick?: (e: React.MouseEvent) => void
}

export const Block = React.memo((props: BlockProps) => {
  const preventExpand = React.useCallback((e: React.MouseEvent) => {
    e.stopPropagation()
  }, [])
  return (
    <UIGridRow
      variant='<-auto-><----------1fr--------->'
      onClick={props.onClick}
      style={{
        minHeight: UtopiaTheme.layout.rowHeight.normal,
        gridColumnGap: 0,
        borderRadius: 3,
      }}
      css={{
        '&:hover': {
          background: colorTheme.bg2.value,
        },
        '&:active': {
          background: colorTheme.bg4.value,
        },
      }}
      padded={false}
    >
      <div
        style={{
          padding: '0 8px',
          justifyContent: 'flex-start',
          height: '100%',
          display: 'grid',
          gridTemplateRows: '12px 10px 1fr',
          alignItems: 'center',
          justifyItems: 'center',
        }}
      >
        <div
          style={{
            width: 1,
            height: '100%',
            background: 'black',
            opacity: props.first ? 0 : 1,
          }}
        />
        <IndicatorLight status={props.status} />
        <div
          style={{
            width: 1,
            height: '100%',
            background: 'black',
            opacity: props.last ? 0 : 1,
          }}
        />
      </div>
      <FlexColumn
        style={{
          padding: '2px 10px 2px 0px',
        }}
      >
        <FlexRow
          style={{
            height: UtopiaTheme.layout.rowHeight.normal,
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'space-between',
          }}
        >
          <b>{props.title}</b>
          <div>{props.subtitle}</div>
        </FlexRow>

        {props.expanded ? (
          <div>
            <FlexColumn
              style={{
                gap: UtopiaTheme.layout.rowHorizontalPadding,
                alignItems: 'flex-start',
              }}
              onClick={preventExpand}
            >
              {props.children}
            </FlexColumn>
          </div>
        ) : null}
      </FlexColumn>
    </UIGridRow>
  )
})
