import * as React from 'react'
import { FlexRow, FunctionIcons, StringInput } from '../../../uuiui'
import { betterReactMemo } from '../../../uuiui-deps'
import { GridRow } from '../../inspector/widgets/grid-row'
import { ResourcesListGridRowConfig } from './generic-external-resources-list'

interface MultiStringControlProps {
  hrefValueToEdit?: string
  relValueToEdit?: string
  closeField: () => void
  onSubmitValues: (values: { hrefValue: string; relValue: string }) => void
}

export const GenericExternalResourcesInput = betterReactMemo(
  'GenericExternalResourcesInput',
  ({ hrefValueToEdit, relValueToEdit, onSubmitValues, closeField }: MultiStringControlProps) => {
    const [hrefValue, setHrefValue] = React.useState(hrefValueToEdit ?? '')
    const [relValue, setRelValue] = React.useState(relValueToEdit ?? '')

    const onHrefValueChange = React.useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
      setHrefValue(e.target.value)
    }, [])
    const onRelValueChange = React.useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
      setRelValue(e.target.value)
    }, [])

    function onKeyDown(e: React.KeyboardEvent) {
      if (e.key === 'Enter') {
        onSubmitValues({ hrefValue, relValue })
        closeField()
      } else if (e.key === 'Escape') {
        e.preventDefault()
        closeField()
      }
    }

    function onConfirmClick() {
      onSubmitValues({ hrefValue, relValue })
      closeField()
    }

    const hrefInputRef = React.useRef<HTMLInputElement>(null)
    React.useEffect(() => {
      if (hrefInputRef.current != null) {
        hrefInputRef.current.focus()
      }
    }, [])

    return (
      <FlexRow style={{ paddingLeft: 12, paddingRight: 8 }}>
        <GridRow {...ResourcesListGridRowConfig} style={{ paddingRight: 8 }}>
          <StringInput
            ref={hrefInputRef}
            value={hrefValue}
            onChange={onHrefValueChange}
            onKeyDown={onKeyDown}
            placeholder='href'
            testId=''
          />
          <StringInput
            value={relValue}
            onChange={onRelValueChange}
            onKeyDown={onKeyDown}
            placeholder='rel'
            testId=''
          />
        </GridRow>
        <FunctionIcons.Confirm
          style={{
            flexGrow: 0,
            flexShrink: 0,
            marginRight: 8,
          }}
          onClick={onConfirmClick}
        />
        <FunctionIcons.Close
          style={{
            flexGrow: 0,
            flexShrink: 0,
          }}
          onClick={closeField}
        />
      </FlexRow>
    )
  },
)
