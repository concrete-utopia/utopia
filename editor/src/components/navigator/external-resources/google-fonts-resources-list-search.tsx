import * as React from 'react'
import { ValueType, createFilter, OptionsType } from 'react-select'
import WindowedSelect from 'react-windowed-select'
import { isRight } from '../../../core/shared/either'
import { PortalTargetID } from '../../../core/shared/utils'
import {
  ExternalResources,
  googleFontsResource,
  GoogleFontsResource,
} from '../../../printer-parsers/html/external-resources-parser'
import { isOptionType } from '../../../utils/utils'
import { betterReactMemo, SelectOption, Utils } from '../../../uuiui-deps'
import { UseSubmitValueFactory } from '../../inspector/common/property-path-hooks'
import {
  fontVariant,
  GoogleFontVariantIdentifier,
  GoogleWebFontsURL,
  parseAndSortVariants,
  prettyNameForFontVariant,
  googleFontsOption,
  fontFamilyVariant,
  GoogleFontVariantOption,
  GoogleFontFamilyOption,
  GoogleFontsOptionValue,
  googleFontFamilyOption,
} from './google-fonts-utils'
import { ObjectInterpolation } from '@emotion/core'
import { GoogleFontsResourcesListSearchOption } from './google-fonts-resources-list-search-option'
import { GoogleFontsResourcesListSearchGroupHeading } from './google-fonts-resources-list-search-group-heading'

interface GoogleFontsResourcesListSearchProps {
  optionsValues: Array<GoogleFontsOptionValue>
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
  ({ optionsValues, useSubmitValueFactory }) => {
    const [options, setOptions] = React.useState<Array<GoogleFontFamilyOption>>([])

    const values = Utils.stripNulls(
      optionsValues.map((optionValue) => {
        const [familyName, variantPrettyName] = optionValue.split('_')
        const familyOption = options.find((option) => option.label === familyName)
        if (familyOption != null) {
          return familyOption.options.find((v) => v.label === variantPrettyName)
        } else {
          return null
        }
      }),
    )

    React.useEffect(() => {
      fetch(GoogleWebFontsURL).then((response) => {
        response
          .json()
          .then(
            (object: {
              items: Array<{ family: string; variants: Array<GoogleFontVariantIdentifier> }>
            }) => {
              const downloadedOptions = Utils.stripNulls(
                object.items.map((value) => {
                  const parsedAndSorted = parseAndSortVariants(value.variants)
                  if (isRight(parsedAndSorted)) {
                    return googleFontFamilyOption(
                      value.family,
                      parsedAndSorted.value.map((variant) =>
                        googleFontsOption(fontFamilyVariant(value.family, variant), value.family),
                      ),
                    )
                  } else {
                    return null
                  }
                }),
              )
              if (downloadedOptions.length > 0) {
                setOptions(downloadedOptions)
              }
            },
          )
      })
    }, [])

    const [pushNewGoogleFontsResource] = useSubmitValueFactory(updatePushNewGoogleFontsResource)

    const onSubmitValue = React.useCallback(
      (newValue: ValueType<GoogleFontVariantOption>) => {
        if (isOptionType(newValue)) {
          pushNewGoogleFontsResource(googleFontsResource(newValue.value, [fontVariant(400, false)]))
        }
      },
      [pushNewGoogleFontsResource],
    )

    return (
      <WindowedSelect
        menuIsOpen
        value={values}
        placeholder='Search for fonts…'
        options={options}
        onChange={onSubmitValue}
        menuPortalTarget={document.getElementById(PortalTargetID)}
        menuPlacement='auto'
        filterOption={createFilter({ ignoreAccents: false })}
        components={{
          Option: GoogleFontsResourcesListSearchOption,
          GroupHeading: GoogleFontsResourcesListSearchGroupHeading,
        }}
        styles={{
          option: (base: ObjectInterpolation<any>, props: any) => ({
            ...base,
            paddingLeft: 18 + 12,
          }),
        }}
      />
    )
  },
)
