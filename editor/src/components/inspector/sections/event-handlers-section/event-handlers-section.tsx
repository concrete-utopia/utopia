import * as React from 'react'
import * as PP from '../../../../core/shared/property-path'
import { identity } from '../../../../core/shared/utils'
import utils from '../../../../utils/utils'
import { addOnUnsetValues } from '../../common/context-menu-items'
import { DOMEventHandler, DOMEventHandlerNames } from '../../common/css-utils'
import { ParsedValues, useInspectorInfo } from '../../common/property-path-hooks'
import { UIGridRow } from '../../widgets/ui-grid-row'
import { isJSXAttributeOtherJavaScript } from '../../../../core/shared/element-template'
import { PropertyLabel } from '../../widgets/property-label'
import {
  betterReactMemo,
  useKeepReferenceEqualityIfPossible,
} from '../../../../utils/react-performance'
import { StringInput, InspectorSectionHeader } from '../../../../uuiui'
import { InspectorContextMenuWrapper } from '../../../../uuiui-deps'

const ppCreate = (p: string) => PP.create([p])

const regularArrayDOMEventHandlerNames = [...DOMEventHandlerNames]

interface EventHandlerControlProps {
  handlerName: string
  value: string
}

export const EventHandlerControl = betterReactMemo(
  'EventHandlerControl',
  (props: EventHandlerControlProps) => {
    const { handlerName, value } = props
    const target = useKeepReferenceEqualityIfPossible([PP.create([handlerName])])
    return (
      <>
        <PropertyLabel target={target}>{handlerName}</PropertyLabel>
        <StringInput
          value={value}
          controlStatus='disabled'
          testId={`event-handler-control-${handlerName}`}
        />
      </>
    )
  },
)

export const EventHandlersSection = betterReactMemo('EventHandlersSection', () => {
  const { value, onUnsetValues } = useInspectorInfo<DOMEventHandler, ParsedValues<DOMEventHandler>>(
    regularArrayDOMEventHandlerNames,
    identity,
    identity,
    ppCreate,
  )

  const valueKeys = Object.keys(value)

  if (valueKeys.length === 0) {
    return null
  }

  return (
    <React.Fragment>
      <InspectorSectionHeader>Event Handlers</InspectorSectionHeader>
      {valueKeys.map((handlerName) => {
        if (value.hasOwnProperty(handlerName)) {
          const attributeValue = value[handlerName as DOMEventHandler]
          if (isJSXAttributeOtherJavaScript(attributeValue)) {
            const eventHandlersContextMenuItems = utils.stripNulls([
              value != null ? addOnUnsetValues([handlerName], onUnsetValues) : null,
            ])

            const eventHandlerValue = attributeValue.javascript
            return (
              <InspectorContextMenuWrapper
                id='event-handlers-section-context-menu'
                items={eventHandlersContextMenuItems}
                style={{ gridColumn: '1 / span 4' }}
                data={null}
              >
                <UIGridRow
                  key={`event-handler-row-${handlerName}`}
                  padded={true}
                  type='<--1fr--><--1fr-->'
                >
                  <EventHandlerControl handlerName={handlerName} value={eventHandlerValue} />
                </UIGridRow>
              </InspectorContextMenuWrapper>
            )
          }
        }
        return null
      })}
    </React.Fragment>
  )
})
EventHandlersSection.displayName = 'EventHandlersSection'
