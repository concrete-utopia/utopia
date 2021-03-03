import * as React from 'react'
import { JSXElementName } from '../../../core/shared/element-template'
import { Imports, TemplatePath } from '../../../core/shared/project-file-types'
import { IcnProps, Icn } from '../../../uuiui'
import { getIconTypeForElement } from '../../inspector/common/name-and-icon-hook'

interface ItemPreviewProps {
  isAutosizingView: boolean
  isFlexLayoutedContainer: boolean
  yogaDirection: 'row' | 'row-reverse' | 'column' | 'column-reverse'
  yogaWrap: 'wrap' | 'wrap-reverse' | 'nowrap'
  staticElementName: JSXElementName | null
  componentInstance: boolean
  path: TemplatePath
  collapsed: boolean
  selected: boolean
  color: IcnProps['color']
  imports: Imports
}

export const ItemPreview: React.FunctionComponent<ItemPreviewProps> = (props) => {
  const {
    isAutosizingView,
    isFlexLayoutedContainer,
    yogaDirection,
    yogaWrap,
    staticElementName,
    imports,
  } = props

  // preview depends on three things:
  // 1 - role
  // 2 - if it's empty or not
  // 3 - if it's a component or not
  // 4 - if it's a placeholder or not
  // 5 if it's generated or not

  // state
  const openStatus: 'closed' | 'open' = props.collapsed ? 'closed' : 'open'

  const type = getIconTypeForElement(
    imports,
    staticElementName,
    isFlexLayoutedContainer,
    yogaDirection,
    yogaWrap,
    openStatus,
    props.componentInstance,
    isAutosizingView,
  )
  const color = props.color

  return (
    <div className='w20 h20 flex justify-center items-center relative'>
      <Icn category='element' type={type} color={color} width={18} height={18} />
    </div>
  )
}
