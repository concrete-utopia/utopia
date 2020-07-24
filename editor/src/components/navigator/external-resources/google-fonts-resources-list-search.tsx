import * as React from 'react'
import Select, { ValueType } from 'react-select'
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

const TempGoogleWebFontsKeyKILLME =
  'https://www.googleapis.com/webfonts/v1/webfonts?key=AIzaSyBffJtCo2vL68hdQKH3IYjo0ELFAAGYNW4'

export const GoogleFontsResourcesListSearch = betterReactMemo<GoogleFontsResourcesListSearchProps>(
  'GoogleFontsResourcesListSearch',
  ({ useSubmitValueFactory }) => {
    const [options, setOptions] = React.useState<Array<SelectOption>>([])
    const [inputValue, setInputValue] = React.useState<string | undefined>(undefined)

    const onInputChange = React.useCallback((newValue: string) => {
      setInputValue(newValue)
    }, [])

    React.useEffect(() => {
      fetch(TempGoogleWebFontsKeyKILLME).then((response) => {
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
