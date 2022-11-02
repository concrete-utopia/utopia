/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { css, jsx } from '@emotion/react'

import { UtopiaTheme } from '../../../uuiui'
import { getControlStyles } from '../common/control-status'

/**
 * the Grid Templates avaliable to use in GridRow.
 * Please try to keep their number low, it makes future refactors easier.
 * If you add a new template, please consult with the team.
 */
const gridTemplates = {
  '<---1fr--->|------172px-------|': {
    gridTemplateColumns: '1fr 172px ',
  },
  '|--67px--||16px||--67px--||16px|': {
    gridTemplateColumns: 'repeat(2, 67px 16px)',
    gridColumnGap: 2,
  },
  '<-auto-><----------1fr--------->': {
    gridColumnGap: 8,
    gridTemplateColumns: 'auto 1fr',
  },
  '<-------1fr------>|----80px----|': {
    gridTemplateColumns: '1fr 80px',
    gridColumnGap: 4,
  },
  '<--------auto-------->|54px||22px|': {
    gridColumnGap: 4,
    gridTemplateColumns: 'auto 54px 22px',
  },
  '<--------auto-------->|--45px--|': {
    gridColumnGap: 4,
    gridTemplateColumns: 'auto 45px',
  },
  '|--32px--|<--------auto-------->': {
    gridColumnGap: 4,
    gridTemplateColumns: '32px auto',
  },
  '<-------auto------->|---60px---|': {
    gridColumnGap: 4,
    gridTemplateColumns: 'auto 60px',
  },
  '<--------auto-------->||22px|': {
    gridColumnGap: 4,
    gridTemplateColumns: 'auto 22px',
  },
  '<--1fr--><--1fr-->': {
    gridColumnGap: 4,
    gridTemplateColumns: '1fr 1fr',
  },
  '<-------------1fr------------->': {
    gridColumnGap: 4,
    gridTemplateColumns: '1fr',
  },
  '<----------1fr---------><-auto->': {
    gridColumnGap: 8,
    gridTemplateColumns: '1fr auto',
  },
  '|--16px--|<--------auto-------->': {
    gridColumnGap: 4,
    gridTemplateColumns: '16px auto',
  },
} as const

export interface GridRowProps extends React.InputHTMLAttributes<HTMLDivElement> {
  /**
   * add a standard horizontal padding
   */
  padded: boolean
  /**
   * pick the row height from our standard themed values
   */
  tall?: boolean
  /**
   * the 'type' of the GridRow is the key which lets you pick from the Grid Templates.
   * Please try to find the template you need before adding a new template to the list.
   */
  variant: keyof typeof gridTemplates
  /**
   * alignItems: default value is 'center'
   */
  alignItems?: 'start' | 'center'
}

export const UIGridRow: React.FunctionComponent<React.PropsWithChildren<GridRowProps>> = ({
  tall,
  variant,
  alignItems,
  style,
  padded,
  children,
  ...props
}) => (
  <div
    {...props}
    css={{
      padding: padded ? `0px ${UtopiaTheme.layout.rowHorizontalPadding}px` : undefined,
      display: 'grid',
      minHeight: tall ? UtopiaTheme.layout.rowHeight.max : UtopiaTheme.layout.rowHeight.normal,
      whiteSpace: 'normal',
      gridColumnGap: 10,
      overflow: 'hidden',
      alignItems: alignItems ?? 'center',
      ...gridTemplates[variant],
      ...(style as any), // TODO Emotion and React 18 types don't like each other
    }}
  >
    {children}
  </div>
)
UIGridRow.displayName = 'UIGridRow'
