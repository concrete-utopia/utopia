import * as R from 'ramda'
import { JsonSchema } from '../../missing-types/json-schema'
import { BaseTemplateName, SvgTemplateName } from '../../core/shared/project-file-types'

// TODO move it to 'packages'

const userEventDetails: JsonSchema = {
  type: 'object',
  description: 'An event, likely triggered by a user by moving a mouse for example.',
  properties: {
    altKey: {
      type: 'boolean',
      default: false,
      description: 'True if the alt key is depressed.',
    },
    button: {
      type: 'number',
      default: 0,
      description: 'Button of the mouse that was pressed to trigger the event.',
    },
    buttons: {
      type: 'number',
      default: 0,
      description: 'Buttons of the mouse that are currently held down.',
    },
    clientX: {
      type: 'number',
      default: 0,
      description:
        'Horizontal coordinate of the position within the viewport, starting on the left edge at zero.',
    },
    clientY: {
      type: 'number',
      default: 0,
      description:
        'Vertical coordinate of the position within the viewport, starting on the top edge at zero.',
    },
    ctrlKey: {
      type: 'boolean',
      default: false,
      description: 'True if the ctrl key is depressed.',
    },
    layerX: {
      type: 'number',
      default: 0,
      description:
        'Horizontal coordinate of the position within the entire content relative to the current layer, starting on the left edge at zero.',
    },
    layerY: {
      type: 'number',
      default: 0,
      description:
        'Vertical coordinate of the position within the entire content relative to the current layer, starting on the top edge at zero.',
    },
    metaKey: {
      type: 'boolean',
      default: false,
      description: 'True if the meta key is depressed.',
    },
    pageX: {
      type: 'number',
      default: 0,
      description:
        'Horizontal coordinate of the position within the entire content, starting on the left edge at zero.',
    },
    pageY: {
      type: 'number',
      default: 0,
      description:
        'Vertical coordinate of the position within the entire content, starting on the top edge at zero.',
    },
    screenX: {
      type: 'number',
      default: 0,
      description:
        'Horizontal coordinate of the position on the screen, starting on the left edge at zero.',
    },
    screenY: {
      type: 'number',
      default: 0,
      description:
        'Vertical coordinate of the position on the screen, starting on the top edge at zero.',
    },
    shiftKey: {
      type: 'boolean',
      default: false,
      description: 'True if the shift key is depressed.',
    },
    timeStamp: {
      type: 'number',
      default: 0,
      description:
        'Point in time when the event was triggered in milliseconds starting at midnight on 01/01/1970.',
    },
    type: {
      type: 'string',
      default: '',
      description: 'Textual name of the type of event that was triggered.',
    },
    movementX: {
      type: 'number',
      default: 0,
      description: 'Horizontal movement since the last event.',
    },
    movementY: {
      type: 'number',
      default: 0,
      description: 'Vertical movement since the last event.',
    },
    fired: {
      type: 'boolean',
      default: false,
      description:
        'True if this event has actually been fired, false if it is the starting default value.',
    },
  },
}

const dynamicEventDetails: JsonSchema = {
  type: 'object',
  description: 'An event from the physics system.',
  properties: {
    type: {
      type: 'string',
      default: '',
      description: 'Textual name of the type of event that was triggered.',
    },
    timestamp: {
      type: 'number',
      default: 0,
      description:
        'Point in time when the event was triggered in milliseconds starting at midnight on 01/01/1970.',
    },
    collisionWith: {
      type: 'string',
      default: '',
      description: 'The other element this one collided with.',
    },
  },
}

export const BaseDefinitions: { [k: string]: JsonSchema } = {
  UserEvent: R.set<JsonSchema, JsonSchema>(
    R.lensPath<JsonSchema, JsonSchema>(['properties', 'previous']),
    userEventDetails,
    userEventDetails,
  ),
  DynamicEvent: R.set<JsonSchema, JsonSchema>(
    R.lensPath<JsonSchema, JsonSchema>(['properties', 'previous']),
    dynamicEventDetails,
    dynamicEventDetails,
  ),
  Color: {
    type: 'object',
    default: { alpha: 1, red: 245, blue: 245, green: 245 },
    description: 'A color in the RGBA schema',
    properties: {
      red: {
        type: 'number',
        default: 120,
        description: `Range: 0 to 255`,
      },
      green: {
        type: 'number',
        default: 120,
        description: `Range: 0 to 255`,
      },
      blue: {
        type: 'number',
        default: 120,
        description: `Range: 0 to 255`,
      },
      alpha: {
        type: 'number',
        minimum: 0,
        maximum: 1,
        stepSize: 0.01,
        numberFormatting: 'percent',
        default: 1,
        description: `Range: 0.0 to 1.0. Number. Values outside this range work but are set to closest value within the range.`,
      },
    },
  },
  Fill: {
    type: 'object',
    properties: {
      enabled: {
        type: 'boolean',
        default: false,
        defaultSpecialForSvg: true,
        description: `If the fill is enabled or not. Boolean.`,
      },
      type: {
        type: 'string',
        enum: ['gradient', 'color'],
        default: '==',
      },
      color: {
        $ref: 'main#/definitions/Color',
        default: { alpha: 1, red: 221, blue: 221, green: 221 },
        defaultSpecialForSvg: { alpha: 1, red: 221, blue: 221, green: 221 },
      },
      gradient: {
        type: 'object',
        properties: {
          angle: {
            type: 'number',
            default: 0,
          },
          blending: {
            default: 'rgb',
            type: 'string',
            enum: ['rgb', 'hsl', 'hsv', 'hsi', 'lab', 'lch', 'hcl'],
          },
          interpolationSamples: {
            type: 'number',
            default: 6,
          },
          stops: {
            type: 'array',
            items: {
              $ref: 'main#/definitions/GradientStop',
            },
            default: [
              {
                color: {
                  red: 97,
                  green: 177,
                  blue: 181,
                  alpha: 0.7,
                },
                position: 0.2,
              },
              {
                color: {
                  red: 255,
                  green: 200,
                  blue: 78,
                  alpha: 0.45,
                },
                position: 0.6,
              },
              {
                color: {
                  red: 255,
                  green: 86,
                  blue: 29,
                  alpha: 1.0,
                },
                position: 1.0,
              },
            ],
          },
        },
      },
    },
  },
  Point: {
    type: 'object',
    description: `Location in 2D space, from the top left, measured in resolution-independent points (not pixels). {x: number, y: number}.`,
    default: {
      x: 0,
      y: 0,
    },
    properties: {
      x: {
        type: 'number',
        default: 0,
        description: `A location along a left-to-right axis, from the left. A number measured in points (not pixels). Round to the nearest .5 or .0 for best rendering. Examples: 100, 0.5, -20.5.`,
      },
      y: {
        type: 'number',
        default: 0,
        description: `A location along a top-to-bottom axis, from the top. A number measured in points (not pixels). Round to the nearest .5 or .0 for best rendering. Examples: 100, 0.5, -4.5.`,
      },
    },
  },
  SnapDestination: {
    type: 'object',
    properties: {
      snapAnchorIndex: {
        type: 'number',
        default: 0,
        description: `Index of snap anchor to use when snapping. If no snap anchors, this should alway be 0`,
      },
      snapPointIndex: {
        type: 'number',
        default: 0,
        description: `Index of snap point to use when snapping.`,
      },
    },
  },
  Size: {
    type: 'object',
    properties: {
      width: {
        type: 'number',
        default: 100,
        description: `The width of a 2D object.\nMeasured in points (not pixels). Needs to be positive.\nExamples: 200, 20.5`,
      },
      height: {
        type: 'number',
        default: 100,
        description: `The height of a 2D object.\nMeasured in points (not pixels). Needs to be positive.\nExamples: 200, 20.5`,
      },
    },
  },
  Frame: {
    type: 'object',
    properties: {
      left: {
        type: 'number',
        default: 0,
        description: `A location along a left-to-right axis, from the left. A number measured in points (not pixels).\nRound to the nearest .5 or .0 for best rendering. Examples: 100, 0.5, -20.5.`,
      },
      top: {
        type: 'number',
        default: 0,
        description: `A location along a top-to-bottom axis, from the top. A number measured in points (not pixels).\nRound to the nearest .5 or .0 for best rendering. Examples: 100, 0.5, -4.5.`,
      },
      width: {
        type: 'number',
        default: 100,
        description: `The width of a 2D object.\nMeasured in points (not pixels). Needs to be positive.\nExamples: 200, 20.5`,
      },
      height: {
        type: 'number',
        default: 100,
        description: `The height of a 2D object.\nMeasured in points (not pixels). Needs to be positive.\nExamples: 200, 20.5`,
      },
    },
  },
  Shadow: {
    type: 'object',
    properties: {
      enabled: {
        type: 'boolean',
        default: false,
        excludeFromComposite: true,
      },
      color: {
        $ref: 'main#/definitions/Color',
        default: { alpha: 0.24, red: 0, blue: 0, green: 0 },
      },
      offset: {
        $ref: 'main#/definitions/Point',
        default: { x: 0, y: 1 },
        excludeFromComposite: true,
      },
      radius: {
        type: 'number',
        default: 3,
      },
      spread: {
        type: 'number',
        default: 0,
      },
      inset: {
        type: 'boolean',
        default: false,
      },
    },
  },
  Border: {
    type: 'object',
    description: `An object describing the borders around an element.\nSimilar to CSS or Framer, it has width, color, border and border radius.\nNo support for border style at the moment.`,
    properties: {
      width: {
        type: 'number',
        default: 1,
        description: `The width of a border. It can take half pixels.\nBorders are drawn inside a view and render on top of it. To draw a border outside, use a shadow (with no blur) instead.`,
      },
    },
  },
  Eventhandler: {
    type: 'string',
    default: 'return null',
    description: `A javascript function.\nIf you're used to Framer or Coffeescript: you need to return the value at the end :)`,
  },
  Expression: {
    type: 'string',
    default: '10 + 20',
    description: `An expression—the right-hand side of a javascript assignment.`,
  },
  Relation: {
    type: 'string',
    enum: ['==', '<=', '>='],
    default: '==',
  },
  Collisions: {
    type: 'object',
    description: 'This element can collide with other physics elements that have physics enabled',
    properties: {
      collidesWithWall: {
        type: 'boolean',
        default: true,
        description: 'This element can collide with any enabled Walls.',
      },
      isStatic: {
        type: 'boolean',
        default: false,
        description: `A flag that indicates whether a body is considered static. A static body can never change position or angle and is completely fixed. If you need to set a body as static after its creation, you should use Body.setStatic as this requires more than just setting this flag.`,
      },
      collides: {
        type: 'boolean',
        default: false,
        description:
          'This element can collide with other physics elements that have physics enabled',
      },
      restitution: {
        type: 'number',
        default: 0.5,
        description: `The bounciness of the element 🎾. Range: 0 to 1. Play with it, change the default.  When two objects collide, it takes the lower value of the two restitutions and that defines the 'bounce'. Think of it as a percentage of the velocity that's preserved, kind of. `,
      },
    },
  },
  Body: {
    type: 'object',
    description: '',
    properties: {
      enabled: {
        type: 'boolean',
        default: false,
        description:
          'Enables physics on this object. It means you can use physics events, acceleration etc.',
      },
      friction: {
        type: 'number',
        default: 0.1,
        description:
          'Range: 0 to 1. Try very low values, e.g. 0.1 or 0.01. Zero means that the body may slide indefinitely. A value of 1 means the body may come to a stop almost instantly after a force is applied. ',
      },
      frictionAir: {
        type: 'number',
        default: 0.1,
        description:
          'Range: 0 to 1. Try very low values, e.g. 0.1 or 0.01.  Number that defines the air friction of the body (air resistance). A value of 0 means the body will never slow as it moves through space. The higher the value, the faster a body slows when moving through space. The effects of the value are non-linear. ',
      },
      frictionStatic: {
        type: 'number',
        default: 0.5,
        description: `Range: 0 to infinity, useful values around 0 - 10. A Number that defines the static friction of the body (in the Coulomb friction model). A value of 0 means the body will never 'stick' when it is nearly stationary and only dynamic friction is used. The higher the value (e.g. 10), the more force it will take to initially get the body moving when nearly stationary. This value is multiplied with the friction property to make it easier to change friction and maintain an appropriate amount of static friction.`,
      },
      mass: {
        type: 'number',
        default: 1,
        description: `The mass (it'll be spread over the volume of the object, but we don't know for sure). You'll need to play with this a lot. We're changing how this works, stay tuned.`,
      },
      gravity: {
        type: 'object',
        description: `Applies gravity to the object`,
        properties: {
          scale: {
            type: 'number',
            default: 0.001,
          },
          x: {
            type: 'number',
            default: 0,
          },
          y: {
            type: 'number',
            default: 0,
          },
        },
      },
      force: {
        type: 'object',
        description: `Apply force on an object.`,
        default: {
          x: 0,
          y: 0,
        },
      },
      constraintDamping: {
        type: 'number',
        description: `Damping applied to any constraints acting on this body`,
        default: 40,
      },
      constraintStiffness: {
        type: 'number',
        description: `Stiffness applied to any constraints acting on this body`,
        default: 2000,
      },
      dragWithSpring: {
        type: 'boolean',
        description: `Use a spring to drag this body`,
        default: false,
      },
      dragDamping: {
        type: 'number',
        description: `Damping applied whilst this body is being dragged`,
        default: 30,
      },
      dragStiffness: {
        type: 'number',
        description: `Stiffness applied whilst this body is being dragged`,
        default: 2000,
      },
      dragFling: {
        type: 'number',
        description: `Amount of time (in s) to fling the body after dragging before attaching to a snap point`,
        default: 0.1,
      },
      stickySnapPoints: {
        type: 'boolean',
        description: `The chosen snap point and anchor combination will be final until the body has settled`,
        default: true,
      },
      velocity: {
        $ref: 'main#/definitions/Point',
        readOnly: true,
        default: { x: 0, y: 0 },
      },
      angularVelocity: {
        type: 'number',
        readOnly: true,
        default: 0,
      },
      motion: {
        type: 'number',
        readOnly: true,
        default: 0,
      },
      speed: {
        type: 'number',
        readOnly: true,
        default: 0,
      },
      angularSpeed: {
        type: 'number',
        readOnly: true,
        default: 0,
      },
      torque: {
        type: 'number',
        readOnly: true,
        default: 0,
      },
    },
  },
  PropertyPath: {
    type: 'object',
    properties: {
      elements: {
        type: 'array',
        items: {
          type: 'string',
          default: '',
        },
        default: [],
      },
    },
  },
  Spring: {
    type: 'object',
    properties: {
      endA: {
        properties: {
          target: {
            $ref: 'main#/definitions/PropertyPath',
          },
          offset: {
            $ref: 'main#/definitions/Point',
          },
        },
      },
      endB: {
        $ref: 'main#/definitions/Point',
      },
      breakingThreshold: {
        type: 'number', // TODO number | null
        default: null,
      },
      enabled: {
        type: 'boolean',
        default: true,
      },
      stiffness: {
        type: 'number',
        default: 50,
      },
      damping: {
        type: 'number',
        default: 5,
      },
      length: {
        type: 'number',
        default: 0,
      },
    },
  },
  SpringMode: {
    type: 'string',
    enum: ['Continuous', 'WhenReleased', 'Instant', 'OverridesDrag'],
    default: 'OverridesDrag',
  },
  SnapAnchor: {
    type: 'object',
    properties: {
      point: {
        $ref: 'main#/definitions/Point',
        default: {
          x: 0,
          y: 0,
        },
      },
      enabled: {
        type: 'boolean',
        default: true,
      },
    },
  },
  SnapPoint: {
    type: 'object',
    properties: {
      point: {
        $ref: 'main#/definitions/Point',
        default: {
          x: 0,
          y: 0,
        },
      },
      stiffness: {
        type: 'number',
        default: 500,
      },
      damping: {
        type: 'number',
        default: 20,
      },
      enabled: {
        type: 'boolean',
        default: true,
      },
    },
  },
  GradientStop: {
    type: 'object',
    properties: {
      color: {
        $ref: 'main#/definitions/Color',
        default: { alpha: 1, red: 245, blue: 245, green: 245 },
      },
      position: {
        type: 'number',
        default: 0.5,
      },
    },
  },
  Control: {
    type: 'object',
    properties: {
      angle: {
        type: 'number',
        default: 90,
      },
      distance: {
        type: 'number',
        default: 60,
      },
    },
  },
  Vertex: {
    type: 'object',
    properties: {
      type: {
        type: 'string',
        enum: ['straight', 'mirrored', 'asymmetric', 'disconnected'],
        default: 'mirrored',
      },
      control1: {
        $ref: 'main#/definitions/Control',
        alias: 'C1',
      },
      control2: {
        $ref: 'main#/definitions/Control',
        alias: 'C2',
      },
      point: {
        $ref: 'main#/definitions/Point',
      },
    },
  },
  TemplatePath: {
    type: 'object',
    properties: {
      elements: {
        type: 'array',
        items: {
          $ref: 'main#/definitions/TemplatePathElement',
        },
        default: [],
      },
    },
    required: ['elements'],
  },
  TemplatePathElement: {
    type: 'object',
    properties: {
      id: {
        type: 'string',
        default: '',
      },
      index: {
        type: 'number',
        default: null,
      },
    },
    required: ['id'],
  },
  PositionDimension: {
    type: 'object',
    properties: {
      derived: {
        type: 'object',
        properties: {
          top: {
            type: 'number',
            alias: 'T',
            default: 0,
            description: `The top of an element.\n⚠️ This is the top _before_ applying transformations (like rotation, scale etc). To get the top of a transformed element, use the transformedFrame property`,
          },
          bottom: {
            type: 'number',
            alias: 'B',
            default: 0,
            description: ``,
          },
          centerY: {
            type: 'number',
            alias: 'CY',
            default: 0,
            description: ``,
          },
          left: {
            type: 'number',
            alias: 'L',
            default: 0,
            description: `The left of an element (along the x axis) in points. Supports half points. Measured from within its parent.\n⚠️ This is the position _before_ applying transformations (like rotation, scale etc). To get the top of a transformed element, use the transformedFrame property.\n⚠️ We do not support getting this position relative to its parent, the screen, or an arbitrary ancestor.`,
          },
          right: {
            type: 'number',
            alias: 'R',
            default: 0,
            description: ``,
          },
          centerX: {
            type: 'number',
            alias: 'CX',
            default: 0,
            description: ``,
          },
          width: {
            type: 'number',
            alias: 'W',
            default: 100,
            description: `The width of an element in points. Supports half points.\n⚠️ This is the width _before_ applying transformations (like scale or skew etc). We do not support getting the computed or transformed frame yet.\n⚠️ We do not support getting this position relative to its parent, the screen, or an arbitrary ancestor.`,
          },
          height: {
            type: 'number',
            alias: 'H',
            default: 100,
            description: `The height of an element in points. Supports half points.\n⚠️ This is the height _before_ applying transformations (like rotation, scale etc). We do not support getting the computed or transformed frame yet.\n⚠️ We do not support getting this position relative to its parent, the screen, or an arbitrary ancestor.`,
          },
        },
      },
      pins: {
        $ref: 'main#/definitions/FramePins',
      },
      points: {
        type: 'object',
        properties: {
          top: {
            type: 'number',
            alias: 'T',
            default: 0,
            description: `The top of an element.\n⚠️ This is the top _before_ applying transformations (like rotation, scale etc). To get the top of a transformed element, use the transformedFrame property`,
          },
          bottom: {
            type: 'number',
            alias: 'B',
            default: 0,
            description: ``,
          },
          centerY: {
            type: 'number',
            alias: 'CY',
            default: 0,
            description: ``,
          },
          left: {
            type: 'number',
            alias: 'L',
            default: 0,
            description: `The left of an element (along the x axis) in points. Supports half points. Measured from within its parent.\n⚠️ This is the position _before_ applying transformations (like rotation, scale etc). To get the top of a transformed element, use the transformedFrame property.\n⚠️ We do not support getting this position relative to its parent, the screen, or an arbitrary ancestor.`,
          },
          right: {
            type: 'number',
            alias: 'R',
            default: 0,
            description: ``,
          },
          centerX: {
            type: 'number',
            alias: 'CX',
            default: 0,
            description: ``,
          },
          width: {
            type: 'number',
            alias: 'W',
            default: 100,
            description: `The width of an element in points. Supports half points.\n⚠️ This is the width _before_ applying transformations (like scale or skew etc). We do not support getting the computed or transformed frame yet.\n⚠️ We do not support getting this position relative to its parent, the screen, or an arbitrary ancestor.`,
          },
          height: {
            type: 'number',
            alias: 'H',
            default: 100,
            description: `The height of an element in points. Supports half points.\n⚠️ This is the height _before_ applying transformations (like rotation, scale etc). We do not support getting the computed or transformed frame yet.\n⚠️ We do not support getting this position relative to its parent, the screen, or an arbitrary ancestor.`,
          },
        },
      },
      yogaPoints: {
        properties: {
          top: {
            type: 'number',
            alias: 'T',
            default: 0,
            description: ``,
          },
          left: {
            type: 'number',
            alias: 'L',
            default: 0,
            description: ``,
          },
          width: {
            type: 'number',
            alias: 'W',
            default: 100,
            description: `The flex-basis-width used in a Flexbox layout`,
          },
          height: {
            type: 'number',
            alias: 'H',
            default: 100,
            description: `The flex-basis-height used in a Flexbox layout`,
          },
        },
      },
    },
  },
  FramePins: {
    type: 'object',
    properties: {
      left: {
        type: 'string',
        default: 'absolute',
        description: 'The left frame pin determining if the position is relative or absolute.',
      },
      right: {
        type: 'string',
        default: 'absolute',
        description: 'The right frame pin determining if the position is relative or absolute.',
      },
      centerX: {
        type: 'string',
        default: 'absolute',
        description: 'The centerX frame pin determining if the position is relative or absolute.',
      },
      width: {
        type: 'string',
        default: 'absolute',
        description: 'The width frame pin determining if the position is relative or absolute.',
      },
      top: {
        type: 'string',
        default: 'absolute',
        description: 'The top frame pin determining if the position is relative or absolute.',
      },
      bottom: {
        type: 'string',
        default: 'absolute',
        description: 'The bottom frame pin determining if the position is relative or absolute.',
      },
      centerY: {
        type: 'string',
        default: 'absolute',
        description: 'The centerY frame pin determining if the position is relative or absolute.',
      },
      height: {
        type: 'string',
        default: 'absolute',
        description: 'The height frame pin determining if the position is relative or absolute.',
      },
    },
  },
  ImageSize: {
    type: 'object',
    allOf: [
      {
        $ref: 'main#/definitions/Size',
      },
      {
        properties: {
          multiplier: {
            type: 'number',
            default: 1,
            description: 'The multiplier used to scale pixels against points.',
          },
        },
      },
    ],
  },
  AspectRatio: {
    type: 'object',
    properties: {
      enabled: {
        type: 'boolean',
        default: false,
        description: 'Specifies if the aspect ratio locking is enabled.',
      },
    },
  },
}

export const BaseProperties: { [k: string]: JsonSchema } = {
  events: {
    // Events have to be added here (to enable autocomplete of events in expressions) and under 'eventHandlers' below
    type: 'object',
    description: 'Events like mouse movement triggered by a user.',
    properties: {
      onMouseDown: {
        $ref: 'main#/definitions/UserEvent',
        description: `Fires when any mouse button is pressed. Common properties: screenX, screenY, pageX, pageY, layerX, layerY, buttons, type, timestamp, movementX, and movementY`,
      },
      onMouseUp: {
        $ref: 'main#/definitions/UserEvent',
        description: `Fires when any mouse button is released. Common properties: screenX, screenY, pageX, pageY, layerX, layerY, buttons, type, timestamp, movementX, and movementY`,
      },
      onMouseMove: {
        $ref: 'main#/definitions/UserEvent',
        description: `Fires when the mouse is moved. ⚠️Use with caution, can impact performance! Common properties: screenX, screenY, pageX, pageY, layerX, layerY, buttons, type, timestamp, movementX, and movementY`,
      },
      onMouseLeave: {
        $ref: 'main#/definitions/UserEvent',
        description: `Fires when the mouse 'leaves' an element'. Not sure about multiple views. Common properties: screenX, screenY, pageX, pageY, layerX, layerY, buttons, type, timestamp, movementX, and movementY`,
      },
      onMouseEnter: {
        $ref: 'main#/definitions/UserEvent',
        description: `Fires when the mouse 'enters' an element'. Not sure about multiple views. Common properties: screenX, screenY, pageX, pageY, layerX, layerY, buttons, type, timestamp, movementX, and movementY`,
      },
      onTouchStart: {
        $ref: 'main#/definitions/UserEvent',
      },
      onTouchEnd: {
        $ref: 'main#/definitions/UserEvent',
      },
      onTouchMove: {
        $ref: 'main#/definitions/UserEvent',
      },
      onTouchCancel: {
        $ref: 'main#/definitions/UserEvent',
      },
      onDragStart: {
        $ref: 'main#/definitions/UserEvent',
      },
      onDrag: {
        $ref: 'main#/definitions/UserEvent',
      },
      onDragEnd: {
        $ref: 'main#/definitions/UserEvent',
      },
      onCollisionStart: {
        $ref: 'main#/definitions/DynamicEvent',
      },
      onCollisionEnd: {
        $ref: 'main#/definitions/DynamicEvent',
      },
      onCollisionActive: {
        $ref: 'main#/definitions/DynamicEvent',
      },
    },
  },
  position: {
    $ref: 'main#/definitions/Point',
  },
  flex: {
    type: 'object',
    properties: {
      element: {
        type: 'object',
        properties: {
          position: {
            type: 'object',
            properties: {
              position: {
                type: 'string',
                default: 'relative',
                description:
                  'The position property specifies how an element is positioned in a document. The top, right, bottom, and left properties determine the final location of positioned elements.',
              },
              left: {
                type: 'number',
                default: 0,
                description:
                  'The left CSS property participates in specifying the horizontal position of a positioned element. It has no effect on non-positioned elements.',
              },
              top: {
                type: 'number',
                default: 0,
                description:
                  'The top CSS property participates in specifying the horizontal position of a positioned element. It has no effect on non-positioned elements.',
              },
              right: {
                type: 'number',
                default: 0,
                description:
                  'The right CSS property participates in specifying the horizontal position of a positioned element. It has no effect on non-positioned elements.',
              },
              bottom: {
                type: 'number',
                default: 0,
                description:
                  'The bottom CSS property participates in specifying the horizontal position of a positioned element. It has no effect on non-positioned elements.',
              },
            },
          },
          flexGrow: {
            type: 'number',
            default: 0,
            description:
              'Flex Grow describes how any space within a container should be distributed among its children along the main axis. After laying out its children, a container will distribute any remaining space according to the flex grow values specified by its children.',
          },
          flexShrink: {
            type: 'number',
            default: 1,
            description:
              'Flex Shrink describes how to shrink children along the main axis in the case that the total size of the children overflow the size of the container on the main axis. flex shrink is very similar to flex grow and can be thought of in the same way if any overflowing size is considered to be negative remaining space. These two properties also work well together by allowing children to grow and shrink as needed.',
          },
          alignSelf: {
            default: 'auto',
            enum: ['auto', 'flex-start', 'center', 'flex-end', 'stretch'],
            description:
              'Aligns flex items of the current flex line overriding the align-items value. If any of the item’s cross-axis margin is set to auto, then align-self is ignored. Please note that `baseline` is not supported.',
          },
          maxWidth: {
            type: 'number',
            default: '0',
            description:
              'The max-width property sets the maximum width of an element. It prevents the width property from becoming larger than the value specified by max-width.',
          },
          maxHeight: {
            type: 'number',
            default: '0',
            description:
              'The max-height property sets the maximum height of an element. It prevents the height property from becoming larger than the value specified by max-height.',
          },
          minWidth: {
            type: 'number',
            default: '0',
            description:
              'The min-width property sets the minimum width of an element. It prevents the width property from becoming smaller than the value specified by min-width.',
          },
          minHeight: {
            type: 'number',
            default: '0',
            description:
              'The min-height property sets the minimum height of an element. It prevents the height property from becoming smaller than the value specified by min-height.',
          },
          margin: {
            type: 'object',
            properties: {
              left: {
                type: 'number',
                default: 0,
                description:
                  'The margin.left property sets the margin area on the left side of an element. A positive value will place it farther than normal from its neighbors, while a negative value will place it closer.',
              },
              top: {
                type: 'number',
                default: 0,
                description:
                  'The margin.top property sets the margin area on the top side of an element. A positive value will place it farther than normal from its neighbors, while a negative value will place it closer.',
              },
              right: {
                type: 'number',
                default: 0,
                description:
                  'The margin.right property sets the margin area on the right side of an element. A positive value will place it farther than normal from its neighbors, while a negative value will place it closer.',
              },
              bottom: {
                type: 'number',
                default: 0,
                description:
                  'The margin.bottom property sets the margin area on the bottom side of an element. A positive value will place it farther than normal from its neighbors, while a negative value will place it closer.',
              },
            },
          },
        },
      },
      container: {
        type: 'object',
        properties: {
          layoutSystem: {
            default: 'pins',
            enum: ['pins', 'flexbox'],
            description:
              'The system used to determine the layout for all direct children of this element.',
          },
          flexDirection: {
            default: 'row',
            enum: ['row', 'row-reverse', 'column', 'column-reverse'],
            description:
              'Specifies how flex items are placed in the flex container defining the main axis and the direction (normal or reversed).',
          },
          flexWrap: {
            default: 'nowrap',
            enum: ['nowrap', 'wrap', 'wrap-reverse'],
            description:
              'Specifies whether flex items are forced into a single line or can be wrapped onto multiple lines. If wrapping is allowed, this property also enables you to control the direction in which lines are stacked.',
          },
          justifyContent: {
            default: 'flex-start',
            enum: [
              'flex-start',
              'center',
              'flex-end',
              'space-between',
              'space-around',
              'space-evenly',
            ],
            description:
              'Defines how space is distributed between and around content items along the main-axis of their container. Note: this property has no effect when there is only one line of flex items.',
          },
          alignItems: {
            default: 'flex-start',
            enum: ['flex-start', 'center', 'flex-end', 'stretch'],
            description:
              'Sets the align-self value on all direct children as a group. The align-self property sets the alignment of an item within its containing block. Note: the `baseline` option is not available.',
          },
          alignContent: {
            default: 'flex-start',
            enum: [
              'flex-start',
              'center',
              'flex-end',
              'space-between',
              'space-around',
              'space-evenly',
            ],
            description:
              'Defines how space is distributed between and around content items along the cross-axis of their container, which is serving as a flexbox container.',
          },
          gap: {
            type: 'object',
            properties: {
              main: {
                type: 'number',
                default: 0,
                description: 'Special margin for elements',
              },
              cross: {
                type: 'number',
                default: 0,
                description: 'Special margin for elements',
              },
            },
          },
          padding: {
            type: 'object',
            properties: {
              left: {
                type: 'number',
                default: 0,
                description:
                  'The padding.left property sets the width of the padding area on the left side of an element.',
              },
              top: {
                type: 'number',
                default: 0,
                description:
                  'The padding.top property sets the width of the padding area on the top side of an element.',
              },
              right: {
                type: 'number',
                default: 0,
                description:
                  'The padding.right property sets the width of the padding area on the right side of an element.',
              },
              bottom: {
                type: 'number',
                default: 0,
                description:
                  'The padding.bottom property sets the width of the padding area on the bottom side of an element.',
              },
            },
          },
        },
      },
    },
  },
  responsive: {
    type: 'boolean',
    default: true,
  },
  eventStates: {
    type: 'object',
    properties: {
      mouseDown: {
        type: 'boolean',
        default: false,
      },
      mouseOver: {
        type: 'boolean',
        default: false,
      },
      drag: {
        type: 'boolean',
        default: false,
      },
      collision: {
        type: 'boolean',
        default: false,
      },
      touch: {
        type: 'boolean',
        default: false,
      },
    },
  },
  signals: {
    type: 'object',
    properties: {
      onMouseDown: {
        type: 'boolean',
        default: false,
      },
      onMouseUp: {
        type: 'boolean',
        default: false,
      },
      onMouseMove: {
        type: 'boolean',
        default: false,
      },
      onMouseLeave: {
        type: 'boolean',
        default: false,
      },
      onMouseEnter: {
        type: 'boolean',
        default: false,
      },
      onTouchStart: {
        type: 'boolean',
        default: false,
      },
      onTouchEnd: {
        type: 'boolean',
        default: false,
      },
      onTouchMove: {
        type: 'boolean',
        default: false,
      },
      onTouchCancel: {
        type: 'boolean',
        default: false,
      },
      onDragStart: {
        type: 'boolean',
        default: false,
      },
      onDrag: {
        type: 'boolean',
        default: false,
      },
      onDragEnd: {
        type: 'boolean',
        default: false,
      },
      onCollisionStart: {
        type: 'boolean',
        default: false,
      },
      onCollisionEnd: {
        type: 'boolean',
        default: false,
      },
      onCollisionActive: {
        type: 'boolean',
        default: false,
      },
    },
  },
  fill: {
    $ref: 'main#/definitions/Fill',
  },
  shadow: {
    $ref: 'main#/definitions/Shadow',
  },
  transformOrigin: {
    type: 'object',
    properties: {
      originX: {
        type: 'number',
        default: 0.5,
        description:
          'Horizontal origin. Number from 0 to 1. 0 is left edge, 1 is right edge, .5 is center (and default).',
      },
      originY: {
        type: 'number',
        default: 0.5,
        description:
          'Vertical origin. Number from 0 to 1. 0 is top edge, 1 is bottom edge, .5 is center (and default).',
      },
    },
  },
  transforms: {
    type: 'object',
    properties: {
      enabled: {
        type: 'boolean',
        default: true,
      },
      dimensions: {
        enum: ['2d', '3d'], // TODO check
        default: '2d',
        description: `A string that says either 2d or 3d (lowercase). `,
      },
      perspective: {
        type: 'number',
        default: 600,
        description: `How many pixels _the children of this element_ are placed from the view. Use it with perspective-origin.
⚠️ The children, not the elemen itself. Place some children and set their 3D transforms, and then edit this value
`,
      },
      rotationX: {
        type: 'number',
        default: 0,
        description: `Think flipping open a laptop. In degrees (0-360). To convert to radians, use degrees * Math.PI / 180`,
      },
      rotationY: {
        type: 'number',
        default: 0,
        description: `Think a spinning top, or a door. In degrees (0-360). To convert to radians, use degrees * Math.PI / 180`,
      },
      rotationZ: {
        type: 'number',
        default: 0,
        description: `The rotation you actually want :) Clockwise, like a clock. In degrees (0-360). To convert to radians, use degrees * Math.PI / 180`,
      },
      scale: {
        stepSize: 0.01,
        numberFormatting: 'percent',
        type: 'number',
        default: 1,
        description: `A single property to set both scaleX and scaleY.`,
      },
      scaleX: {
        type: 'number',
        default: 1,
        description: `The scale along the x-axis. Range is from 0 to infinity, 2 doubles it, .5 halves it. Scaling up affects quality, so use with caution.`,
      },
      scaleY: {
        type: 'number',
        default: 1,
        description: `Lets you scale the objectalong the y-axis, from the center. You probably want scaleX as well. Range is from 0 to infinity, 2 doubles it, .5 halves it.\n⚠️ Scaling up affects quality, so use with caution.\n⚠️ Doesn't change position, width or height of the actual element. We don't yet support a computed or transformed element, stay tuned!`,
      },
      translationX: {
        type: 'number',
        default: 0,
        description: `Moves the element to the right or left. In points. Great for animations that treat the element as a sprite etc.\n⚠️ Doesn't change position, width or height of the actual element. We don't yet support a computed or transformed element, stay tuned!`,
      },
      translationY: {
        type: 'number',
        default: 0,
        description: `Moves the element up or down. In points. Great for animations that treat the element as a sprite etc.\n⚠️ Doesn't change position, width or height of the actual element. We don't yet support a computed or transformed element, stay tuned!`,
      },
      skewX: {
        type: 'number',
        default: 0,
        description: `Skews the element left or right. Think 'move the top edge'. In degrees. Degrees < 0 skew the object to the right, > 0 skew it to the left.
        ⚠️ Doesn't change position, width or height of the actual element. We don't yet support a computed or transformed element, stay tuned!`,
      },
      skewY: {
        type: 'number',
        default: 0,
        description: `Skews the element up or down or right. Think 'move the right edge'. In degrees. Degrees < 0 skew the object to the right, > 0 skew it to the left.
        ⚠️ Doesn't change position, width or height of the actual element. We don't yet support a computed or transformed element, stay tuned!`,
      },
      rotationOrigin: {
        $ref: 'main#/definitions/Point',
      },
    },
  },
  border: {
    type: 'object',
    properties: {
      color: {
        $ref: 'main#/definitions/Color',
        default: { alpha: 1, red: 0, blue: 0, green: 0 },
      },
      all: {
        $ref: 'main#/definitions/Border',
      },
      top: {
        $ref: 'main#/definitions/Border',
        excludeFromComposite: true,
      },
      bottom: {
        $ref: 'main#/definitions/Border',
        excludeFromComposite: true,
      },
      left: {
        $ref: 'main#/definitions/Border',
        excludeFromComposite: true,
      },
      right: {
        $ref: 'main#/definitions/Border',
        excludeFromComposite: true,
      },
      borderType: {
        type: 'string',
        enum: ['all', 'split'],
        default: 'all',
      },
      enabled: {
        type: 'boolean',
        default: true,
        description: `If the border is enabled or not. Boolean.`,
      },
    },
  },
  opacity: {
    minimum: 0,
    maximum: 1,
    stepSize: 0.01,
    numberFormatting: 'percent',
    type: 'number',
    default: 1,
    description: `🐶 Changes the opacity. Range: 0 to 1 (or use percent)`,
  },
  overflow: {
    type: 'boolean',
    default: false,
  },
  radius: {
    type: 'object',
    properties: {
      all: {
        type: 'number',
        default: 0,
      },
      topLeft: {
        type: 'number',
        default: 0,
        excludeFromComposite: true,
      },
      topRight: {
        type: 'number',
        default: 0,
        excludeFromComposite: true,
      },
      bottomRight: {
        type: 'number',
        default: 0,
        excludeFromComposite: true,
      },
      bottomLeft: {
        type: 'number',
        default: 0,
        excludeFromComposite: true,
      },
      borderType: {
        type: 'string',
        enum: ['all', 'split'],
        default: 'all',
      },
    },
    description: `radius on corners. Think CSS border radius, but does not require a border to be set.`,
  },
  css: {
    type: 'object',
    default: {},
    description: `Any other CSS. Use it for 🌈 gradients.`,
  },
  blendMode: {
    enum: ['normal', 'multiply', 'screen'],
    default: 'normal',
    description: 'Supports normal, multiply, screen, and others',
  },
  text: {
    type: 'object', // TODO add type
    default: 'Text',
    description: `Text for a text control. Plain text only, no HTML support for now.`,
  },
  frame: {
    $ref: 'main#/definitions/PositionDimension',
  },
  fillType: {
    enum: ['fill', 'fit', 'stretch', 'tile'],
    default: 'fill',
    description: `How an image can fill a frame. A string. Use 'fill', 'fit', 'stretch', or 'tile'.`,
  },
  filters: {
    type: 'array', // TODO type filters, maybe make them nicer
    description: `A list of stacked filters. Choose from blur, brightness, contrast, grayscale, invert, saturate, or sepia. Each filter has a range from 0 to 1 (or use percent). `,
    default: [
      { enabled: false, filter: { type: 'blur', blur: 1 } },
      { enabled: false, filter: { type: 'brightness', brightness: 1 } },
      { enabled: false, filter: { type: 'contrast', contrast: 1 } },
      { enabled: false, filter: { type: 'grayscale', grayscale: 1 } },
      { enabled: false, filter: { type: 'invert', invert: 1 } },
      { enabled: false, filter: { type: 'saturate', saturate: 1 } },
      { enabled: false, filter: { type: 'sepia', sepia: 1 } },
    ],
  },
  textstyle: {
    type: 'object',
    properties: {
      color: {
        $ref: 'main#/definitions/Color',
        default: {
          red: 0,
          green: 0,
          blue: 0,
          alpha: 1,
        },
      },
      letterSpacing: {
        type: 'number',
        default: 0,
        description: `Additional Space between letters in em (so if your font size changes, this adapts.  Default is 0 (which uses space). Useful range: -0.1 to 1. ⚠️ We currently don't support setting this in pixels.`,
      },
      textAlignment: {
        enum: ['left', 'center', 'right', 'justify'],
        default: 'left',
        description: `Horizontal text alignment: String: 'left', 'center', 'right', or 'justify'. For vertical alignment, position the textfield inside a view manually.`,
      },
      decorationStyle: {
        enum: ['none', 'underline', 'solid', 'line-through'],
        default: 'line-through',
        description: `Pretty much what CSS gives you. Try 'none', 'underline', 'solid' and 'line-through'. NB it's line-through not strike-through :)`,
      },
      lineHeight: {
        type: 'number',
        default: 1.5,
        description: `Line height in standard units. Default is 1. 2 doubles it, .5 halves it.`,
      },
      shadow: {
        type: 'object',
        description: `Text shadow.`,
        properties: {
          enabled: {
            type: 'boolean',
            default: true,
          },
          color: {
            $ref: 'main#/definitions/Color',
            description: `Color of the text shadow`,
            default: { alpha: 1, red: 100, blue: 100, green: 100 },
          },
          offset: {
            $ref: 'main#/definitions/Point',
            default: { x: 1, y: 1 },
          },
          radius: {
            type: 'number',
            default: 3,
          },
        },
      },
      font: {
        type: 'object',
        description: `A font object. Pretty similar to HTML, with support for style, size, weight, etc`,
        properties: {
          family: {
            type: 'string',
            default: '-apple-system, BlinkMacSystemFont',
          },
          size: {
            type: 'number',
            default: 12,
            description: `Font size in pixels. `,
          },
          style: {
            enum: ['normal', 'italic', 'oblique'], // TODO figure out what else can work here
            default: 'italic',
            description: `Font style: 'normal' (or just remove the brick), 'italic', or 'oblique'. For Linethrough etc, use textdecoration instead.`,
          },
          weight: {
            description: `Font weight. Use numbers 100, 200, 300 - 900 (400 is regular), or try named styles available with the font like 'Bold', 'Medium', 'Light' (NB: Capitalization shouldn't matter) `,
            oneOf: [
              {
                type: 'string',
                default: 'normal',
                description: `, Font weight as a named style. What's available depends on the font, but 'Regular', 'Bold', 'Medium', 'Light' are common (NB: Capitalization shouldn't matter)`,
              },
              {
                enum: [100, 200, 300, 400, 500, 600, 700, 800, 900], // TODO figure out what else can work here
                default: 400,
                description: `Font weight using numbers: 100, 200, 300 ... 900 (400 is regular)`,
              },
            ],
            default: 400,
          },
        },
      },
    },
  },
  physics: {
    type: 'object',
    properties: {
      body: {
        $ref: 'main#/definitions/Body',
      },
      collisions: {
        $ref: 'main#/definitions/Collisions',
      },
      rotation: {
        type: 'object',
        description: 'Defines options relating to rotation within the physics model.',
        properties: {
          canRotate: {
            type: 'boolean',
            description: `Enables rotation of an object when collides with other objects.`,
            default: false,
          },
        },
      },
      snapAnchors: {
        type: 'array',
        description: `The closest of these points will be used when applying snapping. If none are set, the center will be used.`,
        items: {
          alias: 'SnapAnchor',
          $ref: 'main#/definitions/SnapAnchor',
        },
      },
      snapPoints: {
        type: 'array',
        items: {
          $ref: 'main#/definitions/SnapPoint',
          alias: 'SnapPoint',
        },
        description: `Objects with physics enabled will snap to the closest of these points.`,
      },
      springs: {
        type: 'array',
        items: {
          $ref: 'main#/definitions/Spring',
          alias: 'Spring',
        },
        description: `Springs connect child components to each other, or to a point on this element.`,
      },
      torsionSpring: {
        type: 'object',
        description: `Causes the element to rotate based on the spring's resting position`,
        properties: {
          angle: {
            type: 'number',
            default: 0,
          },
          damping: {
            type: 'number',
            default: 5,
          },
          stiffness: {
            type: 'number',
            default: 50,
          },
        },
      },
      walls: {
        type: 'object',
        description: `Turns the boundaries of the element into walls to collide with objects inside it. Only use it on the top level element!`,
        default: {
          left: true,
          top: true,
          right: true,
          bottom: true,
        },
      },
      sensor: {
        type: 'object',
        description: `If true, other elements can collide with it but will pass through as if no collision has occurred.`,
        properties: {
          isSensor: {
            type: 'boolean',
            default: false,
          },
        },
      },
    },
  },
  eventHandlers: {
    // Events have to be added here (to add event handlers) and under 'Events' above
    type: 'object',
    properties: {
      onMouseDown: {
        $ref: 'main#/definitions/Eventhandler',
        description: `Fires when any mouse button is pressed. Common properties: screenX, screenY, pageX, pageY, layerX, layerY, buttons, type, timestamp, movementX, and movementY`,
      },
      onMouseUp: {
        $ref: 'main#/definitions/Eventhandler',
        description: `Fires when any mouse button is released. Common properties: screenX, screenY, pageX, pageY, layerX, layerY, buttons, type, timestamp, movementX, and movementY`,
      },
      onMouseMove: {
        $ref: 'main#/definitions/Eventhandler',
        description: `Fires when the mouse is moved.
⚠️Use with caution, can impact performance! Common properties: screenX, screenY, pageX, pageY, layerX, layerY, buttons, type, timestamp, movementX, and movementY`,
      },
      onMouseLeave: {
        $ref: 'main#/definitions/Eventhandler',
        description: `Fires when the mouse 'leaves' an element'. Not sure about multiple views. Common properties: screenX, screenY, pageX, pageY, layerX, layerY, buttons, type, timestamp, movementX, and movementY`,
      },
      onMouseEnter: {
        $ref: 'main#/definitions/Eventhandler',
        description: `Fires when the mouse 'enters' an element'. Not sure about multiple views. Common properties: screenX, screenY, pageX, pageY, layerX, layerY, buttons, type, timestamp, movementX, and movementY`,
      },
      onTouchStart: {
        $ref: 'main#/definitions/Eventhandler',
      },
      onTouchEnd: {
        $ref: 'main#/definitions/Eventhandler',
      },
      onTouchMove: {
        $ref: 'main#/definitions/Eventhandler',
      },
      onTouchCancel: {
        $ref: 'main#/definitions/Eventhandler',
      },
      onDragStart: {
        $ref: 'main#/definitions/Eventhandler',
      },
      onDrag: {
        $ref: 'main#/definitions/Eventhandler',
      },
      onDragEnd: {
        $ref: 'main#/definitions/Eventhandler',
      },
      onCollisionStart: {
        $ref: 'main#/definitions/Eventhandler',
      },
      onCollisionEnd: {
        $ref: 'main#/definitions/Eventhandler',
      },
      onCollisionActive: {
        $ref: 'main#/definitions/Eventhandler',
      },
    },
  },
  draggable: {
    // TODO TYPE check if this is true
    type: 'object',
    properties: {
      type: {
        enum: ['simple', 'noop'],
        default: 'simple',
      },
      simple: {
        type: 'object',
        properties: {
          enabled: {
            type: 'boolean',
            default: false,
          },
          speedX: {
            type: 'number',
            default: 1,
          },
          speedY: {
            type: 'number',
            default: 1,
          },
        },
      },
    },
  },
  dataSource: {
    type: 'array',
    items: {
      type: 'object',
      default: {
        custom: 'value',
      },
    },
  },
  dataSourceUrl: {
    type: 'string',
    default: 'http://example.com/api/data.json',
  },
  inputData: {
    type: 'array',
    default: [],
  },
  isClosed: {
    type: 'boolean',
    default: false,
  },
  template: {
    type: 'string',
    default: '',
  },
  vertices: {
    type: 'array',
    items: {
      $ref: 'main#/definitions/Vertex',
      alias: 'vertex',
    },
  },
  sides: {
    type: 'number',
    default: 5,
  },
  startAngle: {
    type: 'number',
    default: 10,
  },
  endAngle: {
    type: 'number',
    default: 30,
  },
  clockwise: {
    type: 'boolean',
    default: true,
  },
  stroke: {
    type: 'object',
    properties: {
      color: {
        $ref: 'main#/definitions/Color',
        default: { alpha: 1, red: 10, blue: 10, green: 10 },
      },
      dasharray: {
        type: 'string',
        default: '',
        excludeFromComposite: true,
      },
      dashoffset: {
        type: 'string',
        default: '',
        excludeFromComposite: true,
      },
      width: {
        type: 'number',
        default: 1,
      },
      linecap: {
        type: 'string',
        default: 'round',
        excludeFromComposite: true,
      },
      linejoin: {
        type: 'string',
        default: 'round',
        excludeFromComposite: true,
      },
    },
  },
  imageSize: {
    $ref: 'main#/definitions/ImageSize',
  },
  imageId: {
    type: 'string',
    default: '',
  },
  imageBlendMode: {
    type: 'string',
    default: '',
  },
  textSizing: {
    type: 'string',
    enum: ['fixed', 'auto'],
    default: 'fixed',
  },
  aspectRatio: {
    $ref: 'main#/definitions/AspectRatio',
  },
  viewSizing: {
    type: 'string',
    enum: ['fixed', 'group'],
    default: 'fixed',
  },
  contentReference: {
    type: 'string',
    default: '',
  },
  customProps: {
    type: 'object',
    default: {},
  },
  codeTemplateName: {
    type: 'string',
    default: '',
  },
  componentVarName: {
    type: 'string',
    default: '',
  },
}

export const BaseWhitelist: { [key in BaseTemplateName | SvgTemplateName]: string[] } = {
  output: [
    'frame',
    'viewSizing',
    'responsive',
    'transforms',
    'border',
    'overflow',
    'opacity',
    'radius',
    'css',
    'blendMode',
    'eventHandlers',
    'physics.body',
    'physics.rotation',
    'physics.collisions',
    'physics.walls',
    'physics.sensor',
    'physics.snapAnchors',
    'physics.snapPoints',
    'physics.springs',
    'physics.torsionSpring',
    'draggable',
    'fill',
    'shadow',
    'flex',
  ],
  view: [
    'frame',
    'viewSizing',
    'responsive',
    'transforms',
    'border',
    'overflow',
    'opacity',
    'radius',
    'css',
    'blendMode',
    'eventHandlers',
    'physics.body',
    'physics.rotation',
    'physics.collisions',
    'physics.walls',
    'physics.sensor',
    'physics.snapAnchors',
    'physics.snapPoints',
    'physics.springs',
    'physics.torsionSpring',
    'draggable',
    'fill',
    'shadow',
    'flex',
  ],
  placeholder: [
    'frame',
    'responsive',
    'transforms',
    'border',
    'overflow',
    'opacity',
    'radius',
    'css',
    'blendMode',
    'eventHandlers',
    'physics.body',
    'physics.rotation',
    'physics.collisions',
    'physics.walls',
    'physics.sensor',
    'physics.snapAnchors',
    'physics.snapPoints',
    'physics.springs',
    'physics.torsionSpring',
    'draggable',
    'shadow',
    'flex',
    'inputData',
    'template',
  ],
  'multi-generator': [
    'frame',
    'responsive',
    'transforms',
    'border',
    'overflow',
    'opacity',
    'radius',
    'css',
    'blendMode',
    'eventHandlers',
    'physics.body',
    'physics.rotation',
    'physics.collisions',
    'physics.walls',
    'physics.sensor',
    'physics.snapAnchors',
    'physics.snapPoints',
    'physics.springs',
    'physics.torsionSpring',
    'draggable',
    'fill',
    'shadow',
    'inputData',
    'template',
  ],
  image: [
    'frame',
    'responsive',
    'transforms',
    'border',
    'imageSize',
    'imageBlendMode',
    'aspectRatio',
    'eventHandlers',
    'physics.body',
    'physics.rotation',
    'physics.collisions',
    'physics.walls',
    'physics.sensor',
    'physics.snapAnchors',
    'physics.snapPoints',
    'physics.springs',
    'physics.torsionSpring',
    'fillType',
    'filters',
    'shadow',
    'draggable',
    'flex',
  ],
  text: [
    'frame',
    'responsive',
    'eventHandlers',
    'text',
    'textstyle',
    'textstyle.font',
    'textstyle.shadow',
    'physics.body',
    'physics.rotation',
    'physics.collisions',
    'physics.walls',
    'physics.sensor',
    'physics.snapAnchors',
    'physics.snapPoints',
    'physics.springs',
    'physics.torsionSpring',
    'draggable',
    'flex',
  ],
  arc: [
    'frame',
    'responsive',
    'opacity',
    'eventHandlers',
    'transforms',
    'physics.body',
    'physics.rotation',
    'physics.collisions',
    'physics.walls',
    'physics.sensor',
    'physics.snapAnchors',
    'physics.snapPoints',
    'physics.springs',
    'physics.torsionSpring',
    'startAngle',
    'endAngle',
    'clockwise',
    'stroke',
    'fill',
    'draggable',
  ],
  circle: [
    'frame',
    'responsive',
    'opacity',
    'eventHandlers',
    'transforms',
    'physics.body',
    'physics.rotation',
    'physics.collisions',
    'physics.walls',
    'physics.sensor',
    'physics.snapAnchors',
    'physics.snapPoints',
    'physics.springs',
    'physics.torsionSpring',
    'stroke',
    'fill',
    'draggable',
  ],
  path: [
    'opacity',
    'eventHandlers',
    'transforms',
    'physics.body',
    'physics.rotation',
    'physics.collisions',
    'physics.walls',
    'physics.sensor',
    'physics.snapAnchors',
    'physics.snapPoints',
    'physics.springs',
    'physics.torsionSpring',
    'vertices',
    'stroke',
    'fill',
    'draggable',
  ],
  polygon: [
    'frame',
    'responsive',
    'opacity',
    'eventHandlers',
    'transforms',
    'physics.body',
    'physics.rotation',
    'physics.collisions',
    'physics.walls',
    'physics.sensor',
    'physics.snapAnchors',
    'physics.snapPoints',
    'physics.springs',
    'physics.torsionSpring',
    'sides',
    'stroke',
    'fill',
    'draggable',
  ],
  rect: [
    'frame',
    'responsive',
    'opacity',
    'eventHandlers',
    'transforms',
    'physics.body',
    'physics.rotation',
    'physics.collisions',
    'physics.walls',
    'physics.sensor',
    'physics.snapAnchors',
    'physics.snapPoints',
    'physics.springs',
    'physics.torsionSpring',
    'stroke',
    'fill',
    'draggable',
  ],
  rectangle: [
    'frame',
    'responsive',
    'transforms',
    'border',
    'overflow',
    'opacity',
    'radius',
    'css',
    'blendMode',
    'eventHandlers',
    'physics.body',
    'physics.rotation',
    'physics.collisions',
    'physics.walls',
    'physics.sensor',
    'physics.snapAnchors',
    'physics.snapPoints',
    'physics.springs',
    'physics.torsionSpring',
    'draggable',
    'fill',
    'shadow',
    'flex',
  ],
  ellipse: [
    'frame',
    'responsive',
    'transforms',
    'border',
    'overflow',
    'opacity',
    'radius',
    'css',
    'blendMode',
    'eventHandlers',
    'physics.body',
    'physics.rotation',
    'physics.collisions',
    'physics.walls',
    'physics.sensor',
    'physics.snapAnchors',
    'physics.snapPoints',
    'physics.springs',
    'physics.torsionSpring',
    'draggable',
    'fill',
    'shadow',
    'flex',
  ],
  'custom-code': [
    'contentReference',
    'frame',
    'responsive',
    'transforms',
    'border',
    'overflow',
    'eventHandlers',
    'physics.body',
    'physics.rotation',
    'physics.collisions',
    'physics.walls',
    'physics.sensor',
    'physics.snapAnchors',
    'physics.snapPoints',
    'physics.springs',
    'physics.torsionSpring',
    'draggable',
    'shadow',
    'flex',
  ],
  'code-component': [
    'contentReference',
    'frame',
    'responsive',
    'transforms',
    'border',
    'overflow',
    'eventHandlers',
    'physics.body',
    'physics.rotation',
    'physics.collisions',
    'physics.walls',
    'physics.sensor',
    'physics.snapAnchors',
    'physics.snapPoints',
    'physics.springs',
    'physics.torsionSpring',
    'draggable',
    'shadow',
    'flex',
  ],
}
