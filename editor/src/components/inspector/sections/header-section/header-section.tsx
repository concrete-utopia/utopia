import * as React from 'react'
import { colorTheme, UtopiaTheme, Section, H2, Icons, UtopiaStyles } from 'uuiui'
import { FlexRow } from 'uuiui'
import { ExpandableIndicator } from '../../../navigator/navigator-item/expandable-indicator'
import * as TP from '../../../../core/shared/template-path'
import { betterReactMemo } from 'uuiui-deps'
import { JSXElementName, jsxElementName } from '../../../../core/shared/element-template'
import { useToggle, SeeMoreButton, SeeMoreHOC } from '../../widgets/see-more'
import { GridRow } from '../../widgets/grid-row'
import { NameRow, NameRowInnerProps } from './name-row'
import { ElementPathButtons, ElementPathProps } from './element-path'
import {
  LayoutWrapperRow,
  LayoutWrapperCoreProps,
  LayoutWrapperRowProps,
} from './layout-wrapper-section'
import { allHtmlElements } from '../../../../utils/html-elements'

export interface HeaderSectionCoreProps extends ElementPathProps {
  onElementTypeChange: (value: JSXElementName) => void
  style?: React.CSSProperties
  className?: string
}
export interface HeaderSectionProps
  extends HeaderSectionCoreProps,
    NameRowInnerProps,
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

  const isNativeHtmlOrBuiltInElement = props.type !== null && utopiaThings.includes(props.type)

  const renderedTitleStyle = isNativeHtmlOrBuiltInElement
    ? {}
    : { ...UtopiaStyles.textBackgroundStyles.primary }

  const renderedTitle = (
    <span>
      <span style={{ ...renderedTitleStyle }}>{props.label}&nbsp;</span>
      <span
        style={{
          marginLeft: 2,
          background: isNativeHtmlOrBuiltInElement
            ? 'rgb(240,240,240)'
            : UtopiaStyles.backgrounds.paleblue,
          color: isNativeHtmlOrBuiltInElement ? 'rgb(140,140,140)' : colorTheme.primary.value,
          borderRadius: 2,
          textTransform: 'uppercase',
          fontSize: 9,
          fontWeight: 600,
          letterSpacing: '0,05em',
          padding: '1px 2px',
        }}
      >
        {props.type}
      </span>
    </span>
  )

  return (
    <Section>
      <GridRow padded={true} type='<--------auto-------->||22px|'>
        <H2>{renderedTitle}</H2>
        <SeeMoreButton
          seeMoreVisible={seeMoreVisible}
          toggleSeeMoreVisible={toggleSeeMoreVisible}
        />
      </GridRow>
      <SeeMoreHOC visible={seeMoreVisible}>
        <FlexRow style={LargeRowWithMarginStyle}>
          <ElementPathButtons elementPath={props.elementPath} onSelect={props.onSelect} />
        </FlexRow>
        <NameRow
          label={props.label}
          type={props.type}
          onElementTypeChange={props.onElementTypeChange}
        />
        <LayoutWrapperRow onWrap={props.onWrap} onUnwrap={props.onUnwrap} value={props.value} />
      </SeeMoreHOC>
    </Section>
  )
})
