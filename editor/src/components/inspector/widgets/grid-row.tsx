import * as React from 'react'
import { UtopiaTheme } from '../../../uuiui'

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
  '<------50%------><------50%------>': {
    gridColumnGap: 4,
    gridTemplateColumns: 'repeat(2, calc(50% - 2px))',
  },
} as const

interface GridRowProps {
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
  type: keyof typeof gridTemplates
  /**
   * alignItems: default value is 'center'
   */
  alignItems?: 'start' | 'center'
  style?: React.CSSProperties
}

export const GridRow: React.FunctionComponent<GridRowProps> = (props) => (
  <div
    style={{
      padding: props.padded ? `0px ${UtopiaTheme.layout.rowHorizontalPadding}px` : undefined,
      display: 'grid',
      height: props.tall
        ? UtopiaTheme.layout.gridRowHeight.tall
        : UtopiaTheme.layout.gridRowHeight.normal,
      gridColumnGap: 10,
      overflow: 'hidden',
      alignItems: props.alignItems ?? 'center',
      ...gridTemplates[props.type],
      ...(props.style ?? {}),
    }}
  >
    {props.children}
  </div>
)
GridRow.displayName = 'GridRow'
