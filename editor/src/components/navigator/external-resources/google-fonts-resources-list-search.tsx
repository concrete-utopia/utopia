import React from 'react'
import { FixedSizeTree } from 'react-vtree'
import type { TreeWalker } from 'react-vtree/dist/es/Tree'
import { googleFontsList } from '../../../../assets/google-fonts-list'
import type {
  ExternalResources,
  GoogleFontsResource,
} from '../../../printer-parsers/html/external-resources-parser'
import { googleFontsResource } from '../../../printer-parsers/html/external-resources-parser'
import { StringInput } from '../../../uuiui'
import { Utils } from '../../../uuiui-deps'
import type { UseSubmitValueFactory } from '../../inspector/common/property-path-hooks'
import type {
  FontFamilyData,
  FontNode,
  FontsRoot,
  FontVariantData,
  WebFontFamilyVariant,
} from './google-fonts-utils'
import { fontFamilyData, fontVariantData, webFontFamilyVariant } from './google-fonts-utils'
import { GoogleFontsListItem } from './google-fonts-variant-list-item'

interface GoogleFontsResourcesListSearchProps {
  linkedResources: Array<GoogleFontsResource>
  useSubmitValueFactory: UseSubmitValueFactory<ExternalResources>
}

export type PushNewFontFamilyVariant = (newValue: WebFontFamilyVariant) => void
export type RemoveFontFamilyVariant = (valueToDelete: WebFontFamilyVariant) => void

export function updatePushNewFontFamilyVariant(
  newValue: WebFontFamilyVariant,
  oldValue: ExternalResources,
): ExternalResources {
  return {
    ...oldValue,
    googleFontsResources: (() => {
      let workingGoogleFontsResources = [...oldValue.googleFontsResources]
      const existingFamilyResourceIndex = workingGoogleFontsResources.findIndex(
        (resource) => resource.fontFamily === newValue.familyName,
      )
      if (existingFamilyResourceIndex > -1) {
        const variantIsAlreadyAdded =
          oldValue.googleFontsResources[existingFamilyResourceIndex].variants.findIndex(
            (value) =>
              value.webFontStyle === newValue.fontVariant.webFontStyle &&
              value.webFontWeight === newValue.fontVariant.webFontWeight,
          ) > -1
        if (!variantIsAlreadyAdded)
          workingGoogleFontsResources[existingFamilyResourceIndex] = {
            ...oldValue.googleFontsResources[existingFamilyResourceIndex],
            variants: (() => {
              let workingVariants = [
                ...oldValue.googleFontsResources[existingFamilyResourceIndex].variants,
              ]
              workingVariants.push(newValue.fontVariant)
              return workingVariants
            })(),
          }
      } else {
        workingGoogleFontsResources.push(
          googleFontsResource(newValue.familyName, [newValue.fontVariant]),
        )
      }
      return workingGoogleFontsResources
    })(),
  }
}

export function updateRemoveFontFamilyVariant(
  valueToDelete: WebFontFamilyVariant,
  oldValue: ExternalResources,
): ExternalResources {
  return {
    ...oldValue,
    googleFontsResources: (() => {
      let workingGoogleFontsResources = [...oldValue.googleFontsResources]
      const familyIndex = workingGoogleFontsResources.findIndex(
        (resource) => resource.fontFamily === valueToDelete.familyName,
      )
      if (familyIndex > -1) {
        let workingGoogleFontsResource = workingGoogleFontsResources[familyIndex]
        let workingVariants = [...workingGoogleFontsResource.variants]
        const variantIndex = workingVariants.findIndex(
          (variant) =>
            variant.webFontStyle === valueToDelete.fontVariant.webFontStyle &&
            variant.webFontWeight === valueToDelete.fontVariant.webFontWeight,
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

export const GoogleFontsResourcesListSearch = React.memo<GoogleFontsResourcesListSearchProps>(
  ({ linkedResources, useSubmitValueFactory }) => {
    const [searchTerm, setSearchTerm] = React.useState('')
    const lowerCaseSearchTerm = searchTerm.toLowerCase()
    const [pushNewFontFamilyVariant] = useSubmitValueFactory(updatePushNewFontFamilyVariant)
    const [removeFontFamilyVariant] = useSubmitValueFactory(updateRemoveFontFamilyVariant)

    const tree = React.useMemo<FontsRoot>(
      () => ({
        id: 'root',
        type: 'root',
        isOpenByDefault: true,
        children: Utils.stripNulls(
          googleFontsList.map((fontDatum) => {
            const linkedVariants = linkedResources.find(
              (resource) => resource.fontFamily === fontDatum.name,
            )
            return fontFamilyData(
              fontDatum.name,
              fontDatum.variants.map((variant) => {
                const isDownloaded =
                  (linkedVariants?.variants.findIndex(
                    (linkedVariant) =>
                      linkedVariant.webFontStyle === variant.webFontStyle &&
                      linkedVariant.webFontWeight === variant.webFontWeight,
                  ) ?? -1) >= 0
                return fontVariantData(
                  webFontFamilyVariant(fontDatum.name, variant),
                  isDownloaded,
                  pushNewFontFamilyVariant,
                  removeFontFamilyVariant,
                )
              }),
            )
          }),
        ),
      }),
      [linkedResources, pushNewFontFamilyVariant, removeFontFamilyVariant],
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
          const { node, nestingLevel } = stack.pop() as any
          const { id, type, familyName, variant, isOpenByDefault, isDownloaded } = node
          let children: Array<FontFamilyData | FontVariantData> = node.children ?? []
          if (type === 'root') {
            children = (children as Array<FontFamilyData>).filter((child) =>
              child.familyName.toLowerCase().includes(lowerCaseSearchTerm),
            )
          }

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
                familyName,
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
      [pushNewFontFamilyVariant, removeFontFamilyVariant, tree, lowerCaseSearchTerm],
    )

    const onChange = React.useCallback(
      (e: React.ChangeEvent<HTMLInputElement>) => setSearchTerm(e.target.value),
      [],
    )

    return (
      <div>
        <StringInput
          testId='google-fonts-list-search'
          placeholder='Search for fonts…'
          value={searchTerm}
          onChange={onChange}
        />
        <FixedSizeTree
          width={227}
          height={300}
          itemSize={GoogleFontsResourcesListItemHeight}
          treeWalker={treeWalker}
        >
          {GoogleFontsListItem}
        </FixedSizeTree>
      </div>
    )
  },
)
