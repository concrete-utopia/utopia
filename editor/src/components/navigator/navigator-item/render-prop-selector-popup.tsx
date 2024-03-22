/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { useContextMenu, Menu } from 'react-contexify'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  getJSXElementNameAsString,
  jsExpressionOtherJavaScriptSimple,
} from '../../../core/shared/element-template'
import type { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { setProp_UNSAFE } from '../../editor/actions/action-creators'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import type { PreferredChildComponent } from 'utopia-api'
import { capitalize } from '../../../core/shared/string-utils'
import { OnClickOutsideHOC, useColorTheme } from '../../../uuiui'

const usePreferredChildrenForTargetProp = (
  target: ElementPath,
  prop: string,
): PreferredChildComponent[] | null => {
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
    return null
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
}

export const RenderPropPicker = React.memo<RenderPropPickerProps>(({ key, id, target, prop }) => {
  const colorTheme = useColorTheme()
  const { hideRenderPropPicker } = useShowRenderPropPicker(id)

  const preferredChildrenForTargetProp = usePreferredChildrenForTargetProp(
    EP.parentPath(target),
    prop,
  )

  const dispatch = useDispatch()

  const onItemClick = React.useCallback(
    (rawJSCodeForRenderProp: string) => (e: React.MouseEvent) => {
      e.stopPropagation()
      e.preventDefault()

      dispatch([
        setProp_UNSAFE(
          EP.parentPath(target),
          PP.create(prop),
          jsExpressionOtherJavaScriptSimple(rawJSCodeForRenderProp, []),
        ),
      ])
    },
    [dispatch, prop, target],
  )

  const squashEvents = React.useCallback((e: React.MouseEvent<unknown>) => {
    e.stopPropagation()
  }, [])

  if (preferredChildrenForTargetProp == null) {
    return null
  }

  return (
    <OnClickOutsideHOC onClickOutside={hideRenderPropPicker}>
      <Menu key={key} id={id} animation={false} style={{ width: 457 }} onClick={squashEvents}>
        <div
          style={{
            display: 'flex',
            flexDirection: 'column',
            gap: 0,
            width: '100%',
            height: '100%',
            padding: 0,
            backgroundColor: colorTheme.white.value,
            borderRadius: 10,
          }}
        >
          <div
            style={{
              padding: '16px 16px',
              display: 'flex',
              flexDirection: 'column',
              width: '100%',
              alignItems: 'flex-start',
              justifyContent: 'flex-start',
              gap: 10,
              height: 'max-content',
            }}
          >
            <div
              style={{
                width: '100%',
                display: 'flex',
                flexDirection: 'row',
                gap: 5,
                fontFamily: 'Utopian-Inter',
                fontWeight: 700,
                fontSize: '11px',
              }}
            >
              <div>Insert into</div>
              <div
                style={{
                  border: '1px solid rgb(0, 0, 0, 1)',
                  borderRadius: 3,
                  height: 21,
                  contain: 'layout',
                }}
              >
                <div
                  style={{
                    border: '1px solid rgb(0, 0, 0, 1)',
                    height: 21,
                    borderRadius: 3,
                    padding: 3,
                    margin: -1, // Honestly I give up
                    position: 'relative',
                    left: 3,
                    top: -2,
                    lineHeight: 'normal',
                    backgroundColor: colorTheme.white.value,
                  }}
                >
                  {capitalize(prop)}
                </div>
              </div>
              <div style={{ flexGrow: 100 }} />
              <div style={{ fontWeight: 600 }}>Preferred</div>
              <div
                style={{
                  fontWeight: 600,
                  color: colorTheme.subduedForeground.value,
                }}
              >
                All Components
              </div>
              <div style={{ flexGrow: 1 }} />
              <div style={{ fontWeight: 600 }}>X</div>
            </div>
            <div
              style={{
                padding: '10px 6px',
                display: 'flex',
                flexDirection: 'row',
                width: '100%',
                height: 27,
                alignItems: 'center',
                justifyContent: 'flex-start',
                gap: 8,
                border: '1px solid #989999',
                borderColor: colorTheme.subduedBorder.value,
                borderRadius: 6,
              }}
            >
              <div
                style={{
                  fontFamily: 'Utopian-Inter',
                  fontStyle: 'normal',
                  fontWeight: 500,
                  fontSize: '11px',
                  color: colorTheme.subduedForeground.value,
                }}
              >
                🔍
              </div>
              <div
                style={{
                  fontFamily: 'Utopian-Inter',
                  fontStyle: 'normal',
                  fontWeight: 500,
                  fontSize: '11px',
                  color: colorTheme.subduedForeground.value,
                }}
              >
                Filter...
              </div>
            </div>
          </div>
          <div
            style={{
              width: '100%',
              borderWidth: '1px 0 0 0',
              borderStyle: 'solid',
              borderColor: colorTheme.subduedBorder.value,
            }}
          />
          <div
            style={{
              padding: 16,
              display: 'flex',
              flexDirection: 'column',
              width: '100%',
              height: 'max-content',
              gap: 10,
            }}
          >
            {preferredChildrenForTargetProp.map((option, idx) => {
              return (
                <div
                  style={{
                    backgroundColor: colorTheme.bg2.value,
                    borderRadius: 5,
                    display: 'flex',
                    flexDirection: 'column',
                    width: '100%',
                    height: 'max-content',
                    gap: 5,
                    padding: 10,
                    fontFamily: 'Utopian-Inter',
                    fontWeight: 500,
                    fontSize: '11px',
                  }}
                  key={`${idx}-label`}
                >
                  <div style={{ fontWeight: 700 }}>{option.name}</div>
                  <div
                    style={{
                      display: 'flex',
                      flexDirection: 'row',
                      width: '100%',
                      height: 'max-content',
                      alignItems: 'center',
                      justifyContent: 'flex-start',
                      flexWrap: 'wrap',
                      gap: 9,
                    }}
                  >
                    {option.variants?.map((v, i) => (
                      <div
                        key={`${idx}-${v.label ?? i}`}
                        onClick={onItemClick(v.code)}
                        css={{
                          backgroundColor: colorTheme.bg5.value,
                          paddingTop: 5,
                          paddingRight: 5,
                          paddingBottom: 5,
                          paddingLeft: 5,
                          borderTopLeftRadius: 3,
                          borderTopRightRadius: 3,
                          borderBottomRightRadius: 3,
                          borderBottomLeftRadius: 3,
                          color:
                            v.label === '(empty)'
                              ? colorTheme.subduedForeground.value
                              : colorTheme.black.value,
                          '&:hover': {
                            backgroundColor: colorTheme.dynamicBlue10.value,
                          },
                        }}
                      >
                        {v.label ?? v.code}
                      </div>
                    ))}
                  </div>
                </div>
              )
            })}
          </div>
        </div>
      </Menu>
    </OnClickOutsideHOC>
  )
})
