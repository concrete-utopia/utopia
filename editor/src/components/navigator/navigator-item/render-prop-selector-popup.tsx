import React from 'react'
import { useContextMenu, Menu } from 'react-contexify'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  getJSXElementNameAsString,
  jsxAttributesFromMap,
  jsxElement,
} from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { setProp_UNSAFE } from '../../editor/actions/action-creators'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import { ComponentPicker, type ElementToInsert } from './component-picker'
import type { PreferredChildComponentDescriptor } from '../../custom-code/internal-property-controls'
import { generateConsistentUID } from '../../../core/shared/uid-utils'
import { getAllUniqueUids } from '../../../core/model/get-unique-ids'
import { unless, when } from '../../../utils/react-conditionals'
import { elementFromInsertMenuItem } from '../../editor/insert-callbacks'
import { MomentumContextMenu } from '../../context-menu-wrapper'
import { NO_OP } from '../../../core/shared/utils'
import { type ContextMenuItem } from '../../context-menu-items'
import { FlexRow, Icn } from '../../../uuiui'

const usePreferredChildrenForTargetProp = (
  target: ElementPath,
  prop: string,
): Array<PreferredChildComponentDescriptor> => {
  const selectedJSXElement = useEditorState(
    Substores.metadata,
    (store) => MetadataUtils.getJSXElementFromMetadata(store.editor.jsxMetadata, target),
    'usePreferredChildrenForSelectedElement selectedJSXElement',
  )

  const preferredChildrenForTargetProp = useEditorState(
    Substores.restOfEditor,
    (store) => {
      if (selectedJSXElement == null) {
        return null
      }

      const targetName = getJSXElementNameAsString(selectedJSXElement.name)
      // TODO: we don't deal with components registered with the same name in multiple files
      for (const file of Object.values(store.editor.propertyControlsInfo)) {
        for (const [name, value] of Object.entries(file)) {
          if (name === targetName) {
            for (const [registeredPropName, registeredPropValue] of Object.entries(
              value.properties,
            )) {
              if (
                registeredPropName === prop &&
                registeredPropValue.control === 'jsx' &&
                registeredPropValue.preferredChildComponents != null
              ) {
                return registeredPropValue.preferredChildComponents
              }
            }
          }
        }
      }

      return null
    },
    'usePreferredChildrenForSelectedElement propertyControlsInfo',
  )

  if (selectedJSXElement == null || preferredChildrenForTargetProp == null) {
    return []
  }

  return preferredChildrenForTargetProp
}

export const useShowRenderPropPicker = (id: string) => {
  const { show, hideAll } = useContextMenu({ id })
  const onClick = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      show(event)
    },
    [show],
  )

  return { showRenderPropPicker: onClick, hideRenderPropPicker: hideAll }
}

interface RenderPropPickerProps {
  target: ElementPath
  prop: string
  key: string
  id: string
  showFullPicker: boolean
}

export const RenderPropPicker = React.memo<RenderPropPickerProps>(
  ({ key, id, target, prop, showFullPicker }) => {
    const { showRenderPropPicker, hideRenderPropPicker } = useShowRenderPropPicker(`${id}-full`)

    const preferredChildrenForTargetProp = usePreferredChildrenForTargetProp(
      EP.parentPath(target),
      prop,
    )

    const dispatch = useDispatch()

    const projectContentsRef = useRefEditorState((state) => state.editor.projectContents)

    const onItemClickInner = React.useCallback(
      (preferredChildToInsert: ElementToInsert) => {
        const uid = generateConsistentUID(
          'prop',
          new Set(getAllUniqueUids(projectContentsRef.current).uniqueIDs),
        )

        const element = preferredChildToInsert.elementToInsert(uid)
        if (element.type !== 'JSX_ELEMENT') {
          throw new Error('only JSX elements are supported as preferred components')
        }

        dispatch([
          setProp_UNSAFE(
            EP.parentPath(target),
            PP.create(prop),
            element,
            preferredChildToInsert.additionalImports ?? undefined,
          ),
        ])
      },
      [dispatch, projectContentsRef, prop, target],
    )

    const onItemClick = React.useCallback(
      (preferredChildToInsert: ElementToInsert) => (e: React.MouseEvent) => {
        e.stopPropagation()
        e.preventDefault()

        onItemClickInner(preferredChildToInsert)
      },
      [onItemClickInner],
    )

    const squashEvents = React.useCallback((e: React.MouseEvent<unknown>) => {
      e.stopPropagation()
    }, [])

    if (preferredChildrenForTargetProp == null) {
      return null
    }

    const noIcon = (
      <div
        style={{
          width: 18,
          height: 18,
          display: 'flex',
          justifyItems: 'center',
          alignItems: 'center',
          position: 'relative',
        }}
      ></div>
    )

    const simpleContextMenuItems: Array<ContextMenuItem<unknown>> = preferredChildrenForTargetProp
      .flatMap<ContextMenuItem<unknown>>((data) => {
        const submenuLabel = (
          <FlexRow>
            <Icn category='component' type='default' width={18} height={18} />
            {data.name}
          </FlexRow>
        )

        const emptyVariant = {
          name: '(empty)',
          submenuName: submenuLabel,
          enabled: true,
          action: () =>
            onItemClickInner({
              elementToInsert: (uid: string) =>
                jsxElement(data.name, uid, jsxAttributesFromMap({}), []),
              additionalImports: data.imports,
            }),
        }

        if (data.variants == null) {
          return [emptyVariant]
        }
        return [
          emptyVariant,
          ...data.variants.flatMap((variant) => {
            return [
              {
                name: variant.insertMenuLabel,
                submenuName: submenuLabel,
                enabled: true,
                action: () =>
                  onItemClickInner({
                    elementToInsert: (uid: string) =>
                      elementFromInsertMenuItem(variant.elementToInsert(), uid),
                    additionalImports: variant.importsToAdd,
                  }),
              },
            ]
          }),
        ]
      })
      .concat([
        {
          name: <div key='separator' className='react-contexify__separator' />,
          enabled: false,
          isSeparator: true,
          action: () => null,
        },
        {
          name: <FlexRow>{noIcon} More...</FlexRow>,
          enabled: true,
          action: (_data, _dispatch, _rightClickCoordinate, e) => {
            showRenderPropPicker(e as React.MouseEvent<any>)
          },
          hideOnAction: false,
        },
      ])

    return showFullPicker ? (
      <Menu
        key={key}
        id={id}
        animation={false}
        style={showFullPicker ? { width: 457 } : {}}
        onClick={squashEvents}
      >
        <ComponentPicker
          insertionTargetName={prop}
          preferredComponents={preferredChildrenForTargetProp}
          allComponents={preferredChildrenForTargetProp}
          onItemClick={onItemClick}
          onClickCloseButton={hideRenderPropPicker}
        />
      </Menu>
    ) : (
      <MomentumContextMenu id={id} items={simpleContextMenuItems} getData={NO_OP} />
    )
  },
)

interface SimpleComponentPickerProps {
  preferredComponents: PreferredChildComponentDescriptor[]
  onItemClick: (preferredChildToInsert: ElementToInsert) => React.MouseEventHandler
  onClickMore: () => void
}

interface PreferredComponentOption {
  label: string
  value: ElementToInsert
}

const SimpleComponentPicker = React.memo((props: SimpleComponentPickerProps) => {
  const { onItemClick, preferredComponents, onClickMore } = props

  const options: Array<PreferredComponentOption> = preferredComponents.flatMap((data) => {
    if (data.variants == null) {
      return [
        {
          label: data.name,
          value: {
            elementToInsert: (uid: string) =>
              jsxElement(data.name, uid, jsxAttributesFromMap({}), []),
            additionalImports: data.imports,
          },
        },
      ]
    }
    return data.variants.flatMap((variant) => {
      return [
        {
          label: variant.insertMenuLabel,
          value: {
            elementToInsert: (uid) => elementFromInsertMenuItem(variant.elementToInsert(), uid),
            additionalImports: variant.importsToAdd,
          },
        },
      ]
    })
  })

  return (
    <>
      {options.map((option, idx) => (
        <SimpleComponentPickerOption key={idx} option={option} onItemClick={onItemClick} />
      ))}
      <div onClick={onClickMore}>More...</div>
    </>
  )
})

interface SimpleComponentPickerOptionProps {
  option: PreferredComponentOption
  onItemClick: (preferredChildToInsert: ElementToInsert) => React.MouseEventHandler
}

const SimpleComponentPickerOption = React.memo((props: SimpleComponentPickerOptionProps) => {
  const { onItemClick, option } = props
  const { label, value } = option

  return <div onClick={onItemClick(value)}>{label}</div>
})
