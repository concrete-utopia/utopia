import * as React from 'react'
import { FixedSizeTree } from 'react-vtree'
import { TreeWalker } from 'react-vtree/dist/es/Tree'
import { isRight } from '../../../core/shared/either'
import {
  ExternalResources,
  GoogleFontsResource,
  googleFontsResource,
} from '../../../printer-parsers/html/external-resources-parser'
import { betterReactMemo, Utils } from '../../../uuiui-deps'
import { UseSubmitValueFactory } from '../../inspector/common/property-path-hooks'
import {
  fontFamilyData,
  fontFamilyVariant,
  FontNode,
  FontsRoot,
  fontVariantData,
  GoogleFontVariantIdentifier,
  GoogleWebFontsURL,
  parseAndSortVariants,
  FontFamilyVariant,
} from './google-fonts-utils'
import { GoogleFontsListItem } from './google-fonts-variant-list-item'

interface GoogleFontsResourcesListSearchProps {
  linkedResources: Array<GoogleFontsResource>
  useSubmitValueFactory: UseSubmitValueFactory<ExternalResources>
}

export type PushNewFontFamilyVariant = (newValue: FontFamilyVariant) => void
export type RemoveFontFamilyVariant = (valueToDelete: FontFamilyVariant) => void

function updatePushNewFontFamilyVariant(
  newValue: FontFamilyVariant,
  oldValue: ExternalResources,
): ExternalResources {
  return {
    ...oldValue,
    googleFontsResources: (() => {
      let workingGoogleFontsResources = [...oldValue.googleFontsResources]
      const existingFamilyResourceIndex = workingGoogleFontsResources.findIndex(
        (resource) => resource.fontFamily === newValue.fontFamily,
      )
      if (existingFamilyResourceIndex > -1) {
        let workingGoogleFontsResource: GoogleFontsResource = {
          ...oldValue.googleFontsResources[existingFamilyResourceIndex],
          variants: (() => {
            let workingVariants = [
              ...oldValue.googleFontsResources[existingFamilyResourceIndex].variants,
            ]
            workingVariants.push(newValue.fontVariant)
            return workingVariants
          })(),
        }
        workingGoogleFontsResources[existingFamilyResourceIndex] = workingGoogleFontsResource
      } else {
        workingGoogleFontsResources.push(
          googleFontsResource(newValue.fontFamily, [newValue.fontVariant]),
        )
      }
      return workingGoogleFontsResources
    })(),
  }
}

function updateRemoveFontFamilyVariant(
  valueToDelete: FontFamilyVariant,
  oldValue: ExternalResources,
): ExternalResources {
  return {
    ...oldValue,
    googleFontsResources: (() => {
      let workingGoogleFontsResources = [...oldValue.googleFontsResources]
      const familyIndex = workingGoogleFontsResources.findIndex(
        (resource) => resource.fontFamily === valueToDelete.fontFamily,
      )
      if (familyIndex > -1) {
        let workingGoogleFontsResource = workingGoogleFontsResources[familyIndex]
        let workingVariants = [...workingGoogleFontsResource.variants]
        const variantIndex = workingVariants.findIndex(
          (variant) =>
            variant.italic === valueToDelete.fontVariant.italic &&
            variant.weight === valueToDelete.fontVariant.weight,
        )
        if (variantIndex > -1) {
          if (workingVariants.length > 1) {
            workingVariants.splice(variantIndex, 1)
            workingGoogleFontsResource.variants = workingVariants
            workingGoogleFontsResources[familyIndex] = workingGoogleFontsResource
          } else {
            workingGoogleFontsResources.splice(familyIndex, 1)
          }
        }
      }
      return workingGoogleFontsResources
    })(),
  }
}

export const GoogleFontsResourcesListItemHeight = 26

type GoogleFontsItems = Array<{ family: string; variants: Array<GoogleFontVariantIdentifier> }>

export const GoogleFontsResourcesListSearch = betterReactMemo<GoogleFontsResourcesListSearchProps>(
  'GoogleFontsResourcesListSearch',
  ({ linkedResources, useSubmitValueFactory }) => {
    const [pushNewFontFamilyVariant] = useSubmitValueFactory(updatePushNewFontFamilyVariant)
    const [removeFontFamilyVariant] = useSubmitValueFactory(updateRemoveFontFamilyVariant)
    const [fontData, setFontData] = React.useState<GoogleFontsItems>([])

    const tree = React.useMemo<FontsRoot>(
      () => ({
        id: 'root',
        type: 'root',
        isOpenByDefault: true,
        children: Utils.stripNulls(
          fontData.map((fontDatum) => {
            const linkedVariants = linkedResources.find(
              (resource) => resource.fontFamily === fontDatum.family,
            )
            const parsedAndSorted = parseAndSortVariants(fontDatum.variants)
            if (isRight(parsedAndSorted)) {
              return fontFamilyData(
                fontDatum.family,
                parsedAndSorted.value.map((variant) => {
                  const isDownloaded =
                    (linkedVariants?.variants.findIndex(
                      (linkedVariant) =>
                        linkedVariant.italic === variant.italic &&
                        linkedVariant.weight === variant.weight,
                    ) ?? -1) >= 0
                  return fontVariantData(
                    fontFamilyVariant(fontDatum.family, variant),
                    isDownloaded,
                    pushNewFontFamilyVariant,
                    removeFontFamilyVariant,
                  )
                }),
              )
            } else {
              return null
            }
          }),
        ),
      }),
      [fontData, linkedResources, pushNewFontFamilyVariant, removeFontFamilyVariant],
    )

    const treeWalker: TreeWalker<FontNode> = React.useCallback(
      function* (refresh: boolean) {
        const stack = []
        // Remember all the necessary data of the first node in the stack.
        stack.push({
          nestingLevel: 0,
          node: tree,
        })

        // Walk through the tree until we have no nodes available.
        while (stack.length !== 0) {
          const {
            node: { children = [], id, type, fontFamily, variant, isOpenByDefault, isDownloaded },
            nestingLevel,
          } = stack.pop() as any

          // Here we are sending the information about the node to the Tree component
          // and receive an information about the openness state from it. The
          // `refresh` parameter tells us if the full update of the tree is requested;
          // basing on it we decide to return the full node data or only the node
          // id to update the nodes order.
          const isOpened = yield refresh
            ? {
                id,
                children,
                isLeaf: children.length === 0,
                isOpenByDefault,
                nestingLevel,
                type,
                fontFamily,
                variant,
                isDownloaded,
                pushNewFontFamilyVariant,
                removeFontFamilyVariant,
              }
            : id

          // Basing on the node openness state we are deciding if we need to render
          // the child nodes (if they exist).
          if (children.length !== 0 && isOpened) {
            // Since it is a stack structure, we need to put nodes we want to render
            // first to the end of the stack.
            for (let i = children.length - 1; i >= 0; i--) {
              stack.push({
                nestingLevel: nestingLevel + 1,
                node: children[i],
              })
            }
          }
        }
      },
      [pushNewFontFamilyVariant, removeFontFamilyVariant, tree],
    )

    React.useEffect(() => {
      fetch(GoogleWebFontsURL).then((response) => {
        response.json().then((responseData: { items: GoogleFontsItems }) => {
          setFontData(responseData.items)
        })
      })
    }, [])

    return (
      <FixedSizeTree
        width={227}
        height={300}
        itemSize={GoogleFontsResourcesListItemHeight}
        treeWalker={treeWalker}
      >
        {GoogleFontsListItem}
      </FixedSizeTree>
    )
  },
)
