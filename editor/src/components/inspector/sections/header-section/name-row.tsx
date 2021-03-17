import * as React from 'react'
import { getControlStyles } from '../../common/control-status'
import { SelectOption } from '../../controls/select-control'
import { JSXElementName } from '../../../../core/shared/element-template'
import { GridRow } from '../../widgets/grid-row'
import { PopupList } from '../../../../uuiui'
import { betterReactMemo } from '../../../../uuiui-deps'
import { Imports } from '../../../../core/shared/project-file-types'
import {
  getComponentGroups,
  getComponentGroupsAsSelectOptions,
  InsertableComponent,
} from '../../../shared/project-components'
import { useEditorState } from '../../../editor/store/store-hook'
import { usePossiblyResolvedPackageDependencies } from '../../../editor/npm-dependency/npm-dependency'

export interface NameRowProps {
  onElementTypeChange: (value: JSXElementName, importsToAdd: Imports) => void
}

export interface NameRowInnerProps extends NameRowProps {
  label: string
  type: null | string
}

export const NameRow = betterReactMemo('NameRow', (props: NameRowInnerProps) => {
  const onSelect = React.useCallback(
    (selectOption: SelectOption) => {
      const value: InsertableComponent = selectOption.value
      props.onElementTypeChange(value.element.name, value.importsToAdd)
    },
    [props],
  )

  const dependencies = usePossiblyResolvedPackageDependencies()

  const { packageStatus, propertyControlsInfo, projectContents, fullPath } = useEditorState(
    (store) => {
      return {
        packageStatus: store.editor.nodeModules.packageStatus,
        propertyControlsInfo: store.editor.propertyControlsInfo,
        projectContents: store.editor.projectContents,
        fullPath: store.editor.canvas.openFile?.filename ?? null,
      }
    },
    'Name Row Values',
  )

  const insertableComponents = React.useMemo(() => {
    if (fullPath == null) {
      return []
    } else {
      return getComponentGroupsAsSelectOptions(
        packageStatus,
        propertyControlsInfo,
        projectContents,
        dependencies,
        fullPath,
      )
    }
  }, [packageStatus, propertyControlsInfo, projectContents, dependencies, fullPath])

  return (
    <GridRow padded={true} type='<---1fr--->|------172px-------|'>
      <span
        style={{
          whiteSpace: 'nowrap',
          overflow: 'hidden',
          textOverflow: 'ellipsis',
        }}
      >
        Render as
      </span>
      {props.type == null ? null : (
        <PopupList
          disabled={!controlStyles.interactive}
          value={{ value: props.type, label: props.type }}
          onSubmitValue={onSelect}
          options={insertableComponents}
          containerMode='default'
        />
      )}
    </GridRow>
  )
})

const constrolStatus = 'simple'
const controlStyles = getControlStyles(constrolStatus)
