/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { when } from '../../../../utils/react-conditionals'
import { colorTheme, FlexColumn, FlexRow, UtopiaTheme } from '../../../../uuiui'
import { UIGridRow } from '../../../inspector/widgets/ui-grid-row'

export const IndicatorLight = React.memo((props: { status: BlockStatus }) => (
  <div
    style={{
      zIndex: 99999,
      width: 11,
      height: 11,
      border: `1px solid ${colorTheme.gitubIndicatorConnectorLine.value}`,
      borderRadius: 20,
      background: getIndicatorColor(props.status),
    }}
  />
))

function getIndicatorColor(status: BlockStatus): string {
  switch (status) {
    case 'incomplete':
      return colorTheme.githubIndicatorIncomplete.value
    case 'successful':
      return colorTheme.githubIndicatorSuccessful.value
    case 'failed':
      return colorTheme.githubIndicatorFailed.value
    case 'pending':
      return 'transparent' // TODO 'conic-gradient(from 180deg at 50% 50%, #2D2E33 0deg, #FFFFFF 181.87deg, #FFFFFF 360deg)'
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
    <>
      <UIGridRow
        variant='<-auto-><----------1fr--------->'
        onClick={props.onClick}
        style={{
          minHeight: UtopiaTheme.layout.rowHeight.normal,
          gridColumnGap: 0,
          borderRadius: 3,
          color: colorTheme.fg1.value,
        }}
        css={{
          '&:hover': {
            background: props.onClick != null ? colorTheme.bg2.value : 'none',
            cursor: props.onClick != null ? 'pointer' : 'default',
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
            gridTemplateRows: '13px 10px 1fr',
            alignItems: 'center',
            justifyItems: 'center',
          }}
        >
          <div
            style={{
              width: 1,
              height: '100%',
              background: colorTheme.gitubIndicatorConnectorLine.value,
              opacity: props.first ? 0 : 1,
            }}
          />
          <IndicatorLight status={props.status} />
          <div
            style={{
              width: 1,
              height: '100%',
              background: colorTheme.gitubIndicatorConnectorLine.value,
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
              minHeight: UtopiaTheme.layout.rowHeight.normal,
              display: 'flex',
              alignItems: 'baseline',
              justifyContent: 'space-between',
              padding: '8px 0',
              gap: 12,
            }}
          >
            <div style={{ fontWeight: 700, color: colorTheme.fg0.value }}>{props.title}</div>
            <div
              style={{
                textAlign: 'right',
                overflowWrap: 'break-word',
                whiteSpace: 'pre-wrap',
              }}
            >
              {props.subtitle}
            </div>
          </FlexRow>
        </FlexColumn>
      </UIGridRow>
      {when(
        props.expanded,
        <UIGridRow
          variant='<-auto-><----------1fr--------->'
          onClick={props.onClick}
          style={{
            minHeight: UtopiaTheme.layout.rowHeight.normal,
            gridColumnGap: 0,
            borderRadius: 3,
            color: colorTheme.fg1.value,
          }}
          padded={false}
        >
          <div
            style={{
              height: '100%',
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
              width: 27,
            }}
          >
            <div
              style={{
                width: 1,
                height: '100%',
                background: colorTheme.gitubIndicatorConnectorLine.value,
                opacity: props.last ? 0 : 1,
              }}
            />
          </div>
          <FlexColumn
            style={{
              padding: '0px 10px 0px 0px',
            }}
          >
            <FlexColumn
              style={{
                gap: UtopiaTheme.layout.rowHorizontalPadding,
                alignItems: 'flex-start',
                paddingBottom: '10px',
              }}
              onClick={preventExpand}
            >
              {props.children}
            </FlexColumn>
          </FlexColumn>
        </UIGridRow>,
      )}
    </>
  )
})
