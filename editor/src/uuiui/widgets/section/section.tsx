/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import styled from '@emotion/styled'
import { FlexRow } from '../layout/flex-row'
import { FlexColumn } from '../layout/flex-column'
import { ExpandableIndicator } from '../../../components/navigator/navigator-item/expandable-indicator'
import { UtopiaTheme, useColorTheme } from '../../styles/theme'
import { SquareButton } from '../../button'
import { unless } from '../../../utils/react-conditionals'

export const Section = styled.div({
  display: 'flex',
  flexDirection: 'column',
})
interface SectionTitleRowProps {
  minimised: boolean
  toggleMinimised?: () => void
  hideButton?: boolean
}

export const SectionTitleRow: React.FunctionComponent<
  React.PropsWithChildren<SectionTitleRowProps>
> = (props) => {
  const colorTheme = useColorTheme()
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
        paddingLeft: 8,
        paddingRight: 8,
        minHeight: UtopiaTheme.layout.rowHeight.normal,
        color: colorTheme.fg0.value,
        cursor: 'pointer',
      }}
      css={{
        '&:hover': {
          background: colorTheme.neutralBackground.value,
        },
      }}
      onClick={handleClick}
    >
      {props.children}
      {unless(
        props.hideButton === true,
        <SquareButton highlight onClick={handleClick}>
          <ExpandableIndicator collapsed={props.minimised} selected={false} visible={true} />
        </SquareButton>,
      )}
    </FlexRow>
  )
}

export const SectionBodyArea = styled(FlexColumn)<{ minimised: boolean }>(
  (props: { minimised: boolean }) => ({
    transition: 'all .2s ease-in-out',
    overflow: 'hidden',
    opacity: props.minimised ? 0 : 1,
    height: props.minimised ? 0 : undefined,
    display: props.minimised ? 'hidden' : undefined,
  }),
)

export const SectionActionSheet = styled(FlexRow)({
  '& > div': { marginLeft: UtopiaTheme.layout.rowButtonSpacing },
})
