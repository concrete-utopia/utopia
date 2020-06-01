import * as React from 'react'
import { betterReactMemo, InspectorContextMenuWrapper } from 'uuiui-deps'
import * as PP from '../../../../core/shared/property-path'
import { identity } from '../../../../core/shared/utils'
import utils from '../../../../utils/utils'
import { addOnUnsetValues } from '../../common/context-menu-items'
import { DOMEventHandler, DOMEventHandlerNames } from '../../common/css-utils'
import { ParsedValues, useInspectorInfo } from '../../common/property-path-hooks'
import { GridRow } from '../../widgets/grid-row'
import { isJSXAttributeOtherJavaScript } from '../../../../core/shared/element-template'
import { InspectorSectionHeader, StringInput } from '../../../../uuiui'
import { PropertyLabel } from '../../widgets/property-label'

const ppCreate = (p: string) => PP.create([p])

export const EventHandlersSection = betterReactMemo('EventHandlersSection', () => {
  const { value, onUnsetValues } = useInspectorInfo<DOMEventHandler, ParsedValues<DOMEventHandler>>(
    DOMEventHandlerNames,
    identity,
    identity,
    ppCreate,
  )

  const eventHandlersContextMenuItems = utils.stripNulls([
    value != null ? addOnUnsetValues(DOMEventHandlerNames, onUnsetValues) : null,
  ])

  return (
    <>
      <InspectorSectionHeader>Event Handlers</InspectorSectionHeader>
      <InspectorContextMenuWrapper
        id='event-handlers-section-context-menu'
        items={eventHandlersContextMenuItems}
        style={{ gridColumn: '1 / span 4' }}
        data={null}
      >
        {Object.keys(value).map((handlerName) => {
          if (value.hasOwnProperty(handlerName)) {
            const attributeValue = value[handlerName as DOMEventHandler]
            if (isJSXAttributeOtherJavaScript(attributeValue)) {
              const eventHandlerValue = attributeValue.javascript
              return (
                <GridRow padded={true} type='<--1fr--><--1fr-->'>
                  <PropertyLabel target={[PP.create([handlerName])]}>{handlerName}</PropertyLabel>
                  <StringInput value={eventHandlerValue} controlStatus='disabled' />
                </GridRow>
              )
            }
          }
          return null
        })}
      </InspectorContextMenuWrapper>
    </>
  )
})
EventHandlersSection.displayName = 'EventHandlersSection'
