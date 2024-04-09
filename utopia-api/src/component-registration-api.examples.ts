import type { ComponentRegistrations } from './component-registration-api'
import type {
  ObjectControlDescription,
  PopUpListControlDescription,
  RadioControlDescription,
} from './core'

// Placeholders

const PlaceholderComponent = () => {}

const Button = PlaceholderComponent
const Text = PlaceholderComponent
const Heading = PlaceholderComponent
const Image = PlaceholderComponent
const Card = PlaceholderComponent
const Section = PlaceholderComponent
const Row = PlaceholderComponent
const Column = PlaceholderComponent
const Grid = PlaceholderComponent
const Page = PlaceholderComponent

// from @shopify/hydrogen
const Money = PlaceholderComponent

// from the sample store
const FeaturedCollection = PlaceholderComponent

// End Placeholders

const ButtonLevelControl: PopUpListControlDescription = {
  control: 'popuplist',
  options: ['primary', 'default', 'subdued', 'warning'],
}

const TextLevelControl: PopUpListControlDescription = {
  control: 'popuplist',
  options: ['primary', 'default', 'subdued'],
}

const AlignmentControl: RadioControlDescription = {
  control: 'radio',
  options: ['center', 'fill'],
}

const CurrencyCodeControl: PopUpListControlDescription = {
  control: 'popuplist',
  options: [
    {
      value: 'AED',
      label: 'United Arab Emirates Dirham (AED)',
    },
    {
      value: 'AFN',
      label: 'Afghan Afghani (AFN).',
    },
    {
      value: 'EUR',
      label: 'Euro (EUR).',
    },
    // TODO: all currencies
  ],
}

const UnitPriceMeasurementMeasuredUnit: PopUpListControlDescription = {
  control: 'popuplist',
  options: [
    {
      value: 'KG',
      label: 'Kilogram',
    },
    {
      value: 'L',
      label: 'Liter',
    },
    {
      value: 'M',
      label: 'Meter',
    },
  ],
}

const MoneyV2Control: ObjectControlDescription = {
  control: 'object',
  object: {
    amount: { control: 'string-input' },
    currencyCode: CurrencyCodeControl,
  },
}

const UnitPriceMeasurementControl: ObjectControlDescription = {
  control: 'object',
  object: {
    measuredType: {
      control: 'popuplist',
      options: [
        {
          value: 'AREA',
          label: 'Area',
        },
        {
          value: 'LENGTH',
          label: 'Length',
        },
        {
          value: 'VOLUME',
          label: 'Volume',
        },
        {
          value: 'WEIGHT',
          label: 'Weight',
        },
      ],
    },
    quantityUnit: UnitPriceMeasurementMeasuredUnit,
    quantityValue: { control: 'number-input' },
    referenceUnit: UnitPriceMeasurementMeasuredUnit,
    referenceValue: { control: 'number-input' },
  },
}

/**
 * TODO
 * - [x] Props with inference turned on
 *    - inference seems very unnecessary, we shouldn't expose this at all
 */

const Components: ComponentRegistrations = {
  '/src/component-registration-api.examples': {
    Button: {
      component: Button,
      properties: {
        level: ButtonLevelControl,
      },
      children: { preferredContent: 'text', placeholder: { type: 'text', contents: 'Button' } },
    },
    Text: {
      component: Text,
      properties: {
        level: TextLevelControl,
      },
      children: { preferredContent: 'text', placeholder: { type: 'text', contents: 'Button' } },
    },
    Heading: {
      component: Heading,
      properties: {
        level: TextLevelControl,
      },
      children: { preferredContent: 'text', placeholder: { type: 'text', contents: 'Button' } },
    },
    Image: {
      component: Image,
      properties: {
        source: { control: 'string-input' },
        rounded: { control: 'checkbox' },
        borderRadius: { control: 'number-input', required: false },
      },
    },
    Card: {
      component: Card,
      focus: 'always',
      properties: {
        title: {
          control: 'jsx',
          preferredContent: [{ name: 'Heading' }, { name: 'Text' }, { name: 'Button' }],
          placeholder: { type: 'spacer', width: 100, height: 20 },
        },
        footer: {
          control: 'jsx',
          preferredContent: [{ component: Text }, { component: Button }],
        },
      },
      children: 'supported',
      variants: [
        {
          label: 'Card with Title, Image and Button',
          imports: [
            `import { Image } from '@shopify/hydrogen'`,
            `import { Heading, Button } from '@shopify/hydrogen';`,
          ],
          code: `
        <Card
          title={<Heading level='primary'/>}>New Stuff</Title>}
          footer={<Button level='default'>}>Buy it ASAP</Button>}
        >
          <Image data={data.product.image} />
        </Card>
        `,
        },
      ],
    },
    Section: {
      component: Section,
      children: {
        preferredContent: [{ name: 'Row' }, { name: 'Column' }, { name: 'Grid' }],
      },
    },
    Row: {
      component: Row,
      properties: {
        gap: { control: 'checkbox' },
        padded: { control: 'checkbox' },
      },
      children: {
        preferredContent: [{ name: 'Column' }, { name: 'Card' }, { name: 'Image' }],
      },
    },
    Column: {
      component: Column,
      properties: {
        padded: { control: 'checkbox' },
        alignment: AlignmentControl,
      },
      children: {
        preferredContent: [{ name: 'Row' }, { name: 'Card' }, { name: 'Image' }],
      },
    },
    Grid: {
      component: Grid,
      properties: {
        left: {
          control: 'jsx',
          preferredContent: [{ name: 'Row' }, { name: 'Column' }, { name: 'Image' }],
          placeholder: { type: 'spacer', width: 100, height: 100 },
        },
        right: {
          control: 'jsx',
          preferredContent: [{ name: 'Row' }, { name: 'Column' }, { name: 'Image' }],
          placeholder: { type: 'spacer', width: 100, height: 100 },
        },
      },
    },
    Page: {
      component: Page,
      children: {
        preferredContent: [{ name: 'Section' }, { name: 'Column' }, { name: 'Row' }],
      },
    },
  },
  '@shopify/hydrogen': {
    Money: {
      component: Money,
      properties: {
        data: MoneyV2Control,
        withoutCurrency: { control: 'checkbox' },
        withoutTrailingZeros: { control: 'checkbox' },
        measurement: UnitPriceMeasurementControl,
        measurementSeparator: {
          control: 'jsx',
          preferredContent: [
            {
              label: '/',
              code: '<span>/</span>',
            },
            {
              label: ' per ',
              code: '<span> per </span>',
            },
          ],
        },
      },
      variants: [
        {
          label: 'Money',
          code: '<Money data={{ amount: "9.99", currencyCode: "EUR" }} />',
        },
        {
          label: 'Money, with measurement',
          code: '<Money data={{ amount: "9.99", currencyCode: "EUR" }} measurement={{ referenceValue: 1, referenceUnit: "KG", }} />',
        },
        {
          label: 'Money, without currency',
          code: '<Money withoutCurrency data={{ amount: "9.99", currencyCode: "EUR" }} />',
        },
      ],
    },
  },
  'src/sample-store-components': {
    FeaturedCollection: {
      component: FeaturedCollection,
      focus: 'always',
      variants: [
        {
          label: 'Featured collection',
          code: `<FeaturedCollection collection={data['collections']['nodes'][4]} />`,
        },
      ],
    },
  },
}

export default Components
