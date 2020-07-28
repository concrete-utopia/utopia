import * as React from 'react'
import Select, { ValueType } from 'react-select'
import { GOOGLE_WEB_FONTS_KEY } from '../../../common/env-vars'
import { PortalTargetID } from '../../../core/shared/utils'
import {
  ExternalResources,
  googleFontsResource,
} from '../../../printer-parsers/html/external-resources-parser'
import { betterReactMemo, SelectOption } from '../../../uuiui-deps'
import { UseSubmitValueFactory } from '../../inspector/common/property-path-hooks'

interface GoogleFontsResourcesListSearchProps {
  useSubmitValueFactory: UseSubmitValueFactory<ExternalResources>
}

function updatePushNewGoogleFontsResource(
  { fontFamily, fontStyleParams }: { fontFamily: string; fontStyleParams?: string },
  oldValue: ExternalResources,
): ExternalResources {
  oldValue.googleFontsResources = [
    ...oldValue.googleFontsResources,
    googleFontsResource(fontFamily, fontStyleParams),
  ]
  return oldValue
}

const GoogleWebFontsURL = `https://www.googleapis.com/webfonts/v1/webfonts?key=${GOOGLE_WEB_FONTS_KEY}`

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
            object.items.map(({ family }: { family: string }) => ({
              value: family,
              label: family,
            })),
          )
        })
      })
    }, [])

    const [pushNewGoogleFontsResource] = useSubmitValueFactory(updatePushNewGoogleFontsResource)

    const onSubmitValue = React.useCallback(
      (newValue: ValueType<SelectOption>) => {
        if (!Array.isArray(newValue)) {
          pushNewGoogleFontsResource(googleFontsResource((newValue as SelectOption).value))
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
