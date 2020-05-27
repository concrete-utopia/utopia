import * as React from 'react'
import { Icn, IcnProps } from 'uuiui'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadata, isJSXElement } from '../../../core/shared/element-template'
import { isHTMLComponent } from '../../../core/model/project-file-utils'
import { Imports, TemplatePath } from '../../../core/shared/project-file-types'
import { isLeft } from '../../../core/shared/either'

interface ItemPreviewProps {
  isAutosizingView: boolean
  element: ElementInstanceMetadata | null
  path: TemplatePath
  collapsed: boolean
  selected: boolean
  color: IcnProps['color']
  imports: Imports
}

export const ItemPreview: React.StatelessComponent<ItemPreviewProps> = (props) => {
  const isGroup = props.isAutosizingView
  const element = props.element

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
  const isFlex = MetadataUtils.isFlexLayoutedContainer(element)
  const originalFlexDirection = MetadataUtils.getYogaDirection(element)
  const flexDirection: 'column' | 'row' =
    originalFlexDirection === 'column' || originalFlexDirection === 'column-reverse'
      ? 'column'
      : 'row'
  const flexWrap = MetadataUtils.getYogaWrap(element)
  const flexWrapped: boolean = flexWrap === 'wrap' || flexWrap === 'wrap-reverse'

  if (element == null) {
    role = 'scene'
  } else {
    if (MetadataUtils.isViewAgainstImports(props.imports, element)) {
      if (isGroup) {
        role = 'group'
      } else {
        role = 'view'
      }
    } else if (MetadataUtils.isEllipseAgainstImports(props.imports, element)) {
      role = 'ellipse'
    } else if (MetadataUtils.isRectangleAgainstImports(props.imports, element)) {
      role = 'rectangle'
    } else if (MetadataUtils.isImg(element)) {
      role = 'image'
    } else if (MetadataUtils.isTextAgainstImports(props.imports, element)) {
      role = 'text'
    } else if (MetadataUtils.isAnimatedElementAgainstImports(props.imports, element)) {
      role = 'animated'
    } else if (element.componentInstance) {
      role = 'componentinstance'
    } else if (isLeft(element.element)) {
      role = 'ghost'
    } else if (isJSXElement(element.element.value)) {
      if (isHTMLComponent(element.element.value.name, props.imports)) {
        role = 'div'
      }
    }
  }

  let specifierPath: string
  if (isFlex && role === 'view') {
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
