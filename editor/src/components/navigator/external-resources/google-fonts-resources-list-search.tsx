import * as React from 'react'
import Select, { ValueType } from 'react-select'
import { PortalTargetID } from '../../../core/shared/utils'
import {
  ExternalResources,
  googleFontsResource,
  GoogleFontsResource,
} from '../../../printer-parsers/html/external-resources-parser'
import { isOptionType } from '../../../utils/utils'
import { betterReactMemo, SelectOption } from '../../../uuiui-deps'
import { UseSubmitValueFactory } from '../../inspector/common/property-path-hooks'
import { fontVariant, GoogleFontVariant, GoogleWebFontsURL } from './google-fonts-utils'

interface GoogleFontsResourcesListSearchProps {
  useSubmitValueFactory: UseSubmitValueFactory<ExternalResources>
}

function updatePushNewGoogleFontsResource(
  value: GoogleFontsResource,
  oldValue: ExternalResources,
): ExternalResources {
  const working = { ...oldValue }
  working.googleFontsResources = [...oldValue.googleFontsResources, value]
  return working
}

export const GoogleFontsResourcesListSearch = betterReactMemo<GoogleFontsResourcesListSearchProps>(
  'GoogleFontsResourcesListSearch',
  ({ useSubmitValueFactory }) => {
    const [options, setOptions] = React.useState<Array<SelectOption>>([])
    const [inputValue, setInputValue] = React.useState<string | undefined>(undefined)

    const onInputChange = React.useCallback((newValue: string) => {
      setInputValue(newValue)
    }, [])

    React.useEffect(() => {
      fetch(GoogleWebFontsURL).then((response) => {
        response.json().then((object) => {
          setOptions(
            object.items.map((value: { family: string; variants: Array<GoogleFontVariant> }) => {
              return {
                value: value.family,
                label: value.family,
              }
            }),
          )
        })
      })
    }, [])

    const [pushNewGoogleFontsResource] = useSubmitValueFactory(updatePushNewGoogleFontsResource)

    const onSubmitValue = React.useCallback(
      (newValue: ValueType<SelectOption>) => {
        if (isOptionType(newValue)) {
          pushNewGoogleFontsResource(googleFontsResource(newValue.value, [fontVariant(400, false)]))
        }
      },
      [pushNewGoogleFontsResource],
    )

    return (
      <Select
        value={null}
        inputValue={inputValue}
        onInputChange={onInputChange}
        placeholder='Search for fonts…'
        options={options}
        onChange={onSubmitValue}
        menuPortalTarget={document.getElementById(PortalTargetID)}
        menuPlacement='auto'
      />
    )
  },
)
