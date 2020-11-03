import * as React from 'react'
import { Icn, IcnProps } from 'uuiui'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  ElementInstanceMetadata,
  isJSXElement,
  JSXElementName,
} from '../../../core/shared/element-template'
import {
  isHTMLComponent,
  isViewAgainstImports,
  isEllipseAgainstImports,
  isRectangleAgainstImports,
  isImg,
  isTextAgainstImports,
  isAnimatedElementAgainstImports,
} from '../../../core/model/project-file-utils'
import { Imports, TemplatePath } from '../../../core/shared/project-file-types'
import { isLeft } from '../../../core/shared/either'

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

export const ItemPreview: React.StatelessComponent<ItemPreviewProps> = (props) => {
  const isGroup = props.isAutosizingView
  const { isFlexLayoutedContainer, yogaDirection, yogaWrap, staticElementName, imports } = props

  // preview depends on three things:
  // 1 - role
  // 2 - if it's empty or not
  // 3 - if it's a component or not
  // 4 - if it's a placeholder or not
  // 5 if it's generated or not

  // state
  const openStatus: string = props.collapsed ? 'closed' : 'open'

  // role
  let role: string = 'default'
  const originalFlexDirection = yogaDirection
  const flexDirection: 'column' | 'row' =
    originalFlexDirection === 'column' || originalFlexDirection === 'column-reverse'
      ? 'column'
      : 'row'
  const flexWrap = yogaWrap
  const flexWrapped: boolean = flexWrap === 'wrap' || flexWrap === 'wrap-reverse'

  if (staticElementName == null) {
    role = 'scene'
  } else {
    if (isViewAgainstImports(staticElementName, imports)) {
      if (isGroup) {
        role = 'group'
      } else {
        role = 'view'
      }
    } else if (isEllipseAgainstImports(staticElementName, imports)) {
      role = 'ellipse'
    } else if (isRectangleAgainstImports(staticElementName, imports)) {
      role = 'rectangle'
    } else if (isImg(staticElementName)) {
      role = 'image'
    } else if (isTextAgainstImports(staticElementName, imports)) {
      role = 'text'
    } else if (isAnimatedElementAgainstImports(staticElementName, imports)) {
      role = 'animated'
    } else if (props.componentInstance) {
      role = 'componentinstance'
    } else if (isHTMLComponent(staticElementName, imports)) {
      role = 'div'
    }
  }

  let specifierPath: string
  if (isFlexLayoutedContainer && role === 'view') {
    specifierPath = `-flex-${flexWrapped ? 'wrap' : 'nowrap'}-${flexDirection}`
  } else {
    if (role === 'group') {
      specifierPath = `-${openStatus}`
    } else {
      specifierPath = ''
    }
  }

  const color = props.color

  return (
    <div className='w20 h20 flex justify-center items-center relative'>
      <Icn
        category='element'
        type={`${role}${specifierPath}`}
        color={color}
        width={18}
        height={18}
      />
    </div>
  )
}
