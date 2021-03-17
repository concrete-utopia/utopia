import * as React from 'react'
import { ExpandableIndicator } from '../../../navigator/navigator-item/expandable-indicator'
import * as TP from '../../../../core/shared/template-path'
import { JSXElementName, jsxElementName } from '../../../../core/shared/element-template'
import { useToggle, SeeMoreButton, SeeMoreHOC } from '../../widgets/see-more'
import { GridRow } from '../../widgets/grid-row'
import {
  LayoutWrapperRow,
  LayoutWrapperCoreProps,
  LayoutWrapperRowProps,
} from './layout-wrapper-section'
import { allHtmlElements } from '../../../../utils/html-elements'
import { UtopiaTheme, UtopiaStyles, colorTheme, Section, H2, FlexRow } from '../../../../uuiui'
import { betterReactMemo } from '../../../../uuiui-deps'
import { Imports } from '../../../../core/shared/project-file-types'

export interface HeaderSectionCoreProps {
  onElementTypeChange: (value: JSXElementName, importsToAdd: Imports) => void
  style?: React.CSSProperties
  className?: string
}
export interface HeaderSectionProps
  extends HeaderSectionCoreProps,
    LayoutWrapperCoreProps,
    LayoutWrapperRowProps {}

const LargeRowWithMarginStyle = {
  marginLeft: '8px',
  marginRight: '8px',
  minHeight: UtopiaTheme.layout.rowHeight.large,
}

export const HeaderSection = betterReactMemo('HeaderSection', (props: HeaderSectionProps) => {
  const [seeMoreVisible, toggleSeeMoreVisible] = useToggle(false)

  const utopiaComponents = ['View', 'Rectangle', 'Oval', 'Image']
  const utopiaThings = [...utopiaComponents, ...allHtmlElements]

  return (
    <Section>
      <GridRow padded={true} type='<--------auto-------->||22px|'>
        <SeeMoreButton
          seeMoreVisible={seeMoreVisible}
          toggleSeeMoreVisible={toggleSeeMoreVisible}
        />
      </GridRow>
      <SeeMoreHOC visible={seeMoreVisible}>
        <LayoutWrapperRow onWrap={props.onWrap} onUnwrap={props.onUnwrap} value={props.value} />
      </SeeMoreHOC>
    </Section>
  )
})
