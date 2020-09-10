/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as React from 'react'
import styled from '@emotion/styled'
import { FlexRow } from '../layout/flex-row'
import { FlexColumn } from '../layout/flex-column'
import { ExpandableIndicator } from '../../../components/navigator/navigator-item/expandable-indicator'
import { UtopiaTheme, colorTheme } from '../../styles/theme'
import { SquareButton } from '../../button'

export const Section = styled.div({
  display: 'flex',
  flexDirection: 'column',
})
interface SectionTitleRowProps {
  minimised: boolean
  toggleMinimised?: () => void
}

export const SectionTitleRow: React.FunctionComponent<SectionTitleRowProps> = (props) => {
  const toggleMinimised = props.toggleMinimised
  const handleClick = React.useCallback(
    (e: React.MouseEvent) => {
      if (typeof toggleMinimised !== 'undefined') {
        toggleMinimised()
      }
      e.stopPropagation()
    },
    [toggleMinimised],
  )

  return (
    <FlexRow
      style={{
        transition: 'all .2s ease-in-out',
        paddingLeft: 12,
        paddingRight: 8,
        minHeight: UtopiaTheme.layout.rowHeight.large,
        cursor: 'pointer',
      }}
      css={{
        '&:hover': {
          background: UtopiaTheme.color.neutralBackground.value,
        },
      }}
      onClick={handleClick}
    >
      {props.children}
      <SquareButton highlight={true} onClick={handleClick}>
        <ExpandableIndicator collapsed={props.minimised} selected={false} visible={true} />
      </SquareButton>
    </FlexRow>
  )
}

export const SectionBodyArea = styled(FlexColumn)<{ minimised: boolean }>(
  (props: { minimised: boolean }) => ({
    transition: 'all .2s ease-in-out',
    paddingBottom: props.minimised ? 0 : 16,
    overflow: 'hidden',
    opacity: props.minimised ? 0 : 1,
    height: props.minimised ? 0 : undefined,
    display: props.minimised ? 'hidden' : undefined,
  }),
)

export const SectionActionSheet = styled(FlexRow)({
  '& > div': { marginLeft: UtopiaTheme.layout.rowButtonSpacing },
})
