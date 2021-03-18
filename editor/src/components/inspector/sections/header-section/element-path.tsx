import * as React from 'react'
import { TemplatePath } from '../../../../core/shared/project-file-types'
import * as TP from '../../../../core/shared/template-path'
import { FlexRow, Icons } from '../../../../uuiui'
import { betterReactMemo } from '../../../../uuiui-deps'

export interface ElementPathElement {
  name?: string
  path: TemplatePath
}

export interface ElementPathProps {
  elementPath: Array<ElementPathElement>
  onSelect: (path: TemplatePath) => void
}

export const ElementPathButtons = betterReactMemo(
  'ElementPathButtons',
  (props: ElementPathProps) => {
    const lastElemIndex = props.elementPath.length - 1
    const elements = props.elementPath.map((elem, index) => {
      const isSelected = index === lastElemIndex
      return (
        <React.Fragment key={`elem-path-${TP.toComponentId(elem.path)}`}>
          <div
            onMouseDown={() => props.onSelect(elem.path)}
            style={{ fontWeight: isSelected ? 500 : 400 }}
          >
            {elem.name}
          </div>
          {isSelected ? null : <span style={{ paddingLeft: 4, paddingRight: 4 }}>&gt;</span>}
        </React.Fragment>
      )
    })

    return (
      <FlexRow>
        <Icons.Component />
        {elements}
      </FlexRow>
    )
  },
)
ElementPathButtons.displayName = 'ElementPathButtons'
