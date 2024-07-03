import {
  defaultComponentDescriptor,
  type ComponentDescriptorsForFile,
  ComponentDescriptorDefaults,
} from '../../components/custom-code/code-file'
import {
  emptyComments,
  jsExpressionValue,
  jsxAttributesEntry,
  jsxElementWithoutUID,
  jsxTextBlock,
} from '../shared/element-template'

export const RemixRunReactComponents: ComponentDescriptorsForFile = {
  Link: {
    properties: {
      to: { control: 'string-input' },
    },
    supportsChildren: true,
    preferredChildComponents: [],
    variants: [
      {
        insertMenuLabel: 'Link',
        importsToAdd: {
          '@remix-run/react': {
            importedWithName: null,
            importedFromWithin: [
              {
                name: 'Link',
                alias: 'Link',
              },
            ],
            importedAs: null,
          },
        },
        elementToInsert: () =>
          jsxElementWithoutUID(
            'Link',
            [
              jsxAttributesEntry(
                'to',
                jsExpressionValue('/', emptyComments),
                emptyComments,
                'include-in-printing',
              ),
            ],
            [jsxTextBlock('Link')],
          ),
      },
    ],
    source: defaultComponentDescriptor(),
    ...ComponentDescriptorDefaults,
  },
  Outlet: {
    properties: {},
    supportsChildren: false,
    preferredChildComponents: [],
    variants: [
      {
        insertMenuLabel: 'Outlet',
        importsToAdd: {
          '@remix-run/react': {
            importedWithName: null,
            importedFromWithin: [
              {
                name: 'Outlet',
                alias: 'Outlet',
              },
            ],
            importedAs: null,
          },
        },
        elementToInsert: () => jsxElementWithoutUID('Outlet', [], []),
      },
    ],
    source: defaultComponentDescriptor(),
    ...ComponentDescriptorDefaults,
  },
}
