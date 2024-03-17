import React from 'react'
import { forEachRight, isRight } from '../../../../core/shared/either'
import { modifiableAttributeIsAttributeOtherJavaScript } from '../../../../core/shared/element-template'
import { forEachValue } from '../../../../core/shared/object-utils'
import * as PP from '../../../../core/shared/property-path'
import { useKeepReferenceEqualityIfPossible } from '../../../../utils/react-performance'
import utils from '../../../../utils/utils'
import { InspectorSectionHeader, StringInput } from '../../../../uuiui'
import { InspectorContextMenuWrapper } from '../../../../uuiui-deps'
import { addOnUnsetValues } from '../../common/context-menu-items'
import { DOMEventHandlerNames } from '../../common/css-utils'
import { useGetMultiselectedProps, useInspectorContext } from '../../common/property-path-hooks'
import { PropertyLabel } from '../../widgets/property-label'
import { UIGridRow } from '../../widgets/ui-grid-row'

const ppCreate = (p: string) => PP.create(p)

const regularArrayDOMEventHandlerNames = [...DOMEventHandlerNames]

interface EventHandlerControlProps {
  handlerName: string
  value: string
}

export const EventHandlerControl = React.memo((props: EventHandlerControlProps) => {
  const { handlerName, value } = props
  const target = useKeepReferenceEqualityIfPossible([PP.create(handlerName)])
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
})

type RawJavaScript = string
type EventHandlerValues = { [eventHandlerName: string]: RawJavaScript }

function useGetEventHandlerInfo(): EventHandlerValues {
  const multiselectedProps = useGetMultiselectedProps(ppCreate, regularArrayDOMEventHandlerNames)
  let result: EventHandlerValues = {}

  forEachValue((values, eventHandlerName) => {
    if (values.length === 1) {
      const eitherValue = values[0]
      forEachRight(eitherValue, (value) => {
        if (modifiableAttributeIsAttributeOtherJavaScript(value)) {
          result[eventHandlerName] = value.javascriptWithUIDs
        }
      })
    }
  }, multiselectedProps)

  return useKeepReferenceEqualityIfPossible(result)
}

const EventHandlerSectionRow = React.memo((props: { eventHandlerName: string; value: string }) => {
  const { eventHandlerName, value } = props

  const { onContextUnsetValue } = useInspectorContext()
  const onUnsetValue = React.useCallback(
    () => onContextUnsetValue([ppCreate(eventHandlerName)], false),
    [eventHandlerName, onContextUnsetValue],
  )

  const eventHandlersContextMenuItems = React.useMemo(
    () => utils.stripNulls([addOnUnsetValues([eventHandlerName], onUnsetValue)]),
    [eventHandlerName, onUnsetValue],
  )

  return (
    <InspectorContextMenuWrapper
      id={`event-handlers-section-context-menu-${eventHandlerName}`}
      items={eventHandlersContextMenuItems}
      style={{ gridColumn: '1 / span 4' }}
      data={null}
    >
      <UIGridRow padded={true} variant='<--1fr--><--1fr-->'>
        <EventHandlerControl handlerName={eventHandlerName} value={value} />
      </UIGridRow>
    </InspectorContextMenuWrapper>
  )
})

export const EventHandlersSection = React.memo(() => {
  const values = useGetEventHandlerInfo()
  const valueKeys = Object.keys(values)

  if (valueKeys.length === 0) {
    return null
  }

  return (
    <React.Fragment>
      <InspectorSectionHeader>Event Handlers</InspectorSectionHeader>
      {valueKeys.map((eventHandlerName) => {
        if (values.hasOwnProperty(eventHandlerName)) {
          const eventHandlerValue = values[eventHandlerName]
          return (
            <EventHandlerSectionRow
              key={`event-handlers-section-row-${eventHandlerName}`}
              eventHandlerName={eventHandlerName}
              value={eventHandlerValue}
            />
          )
        }
        return null
      })}
    </React.Fragment>
  )
})
EventHandlersSection.displayName = 'EventHandlersSection'
