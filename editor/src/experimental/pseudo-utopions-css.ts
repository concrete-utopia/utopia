const assembleOption = (value: string, label: string, color: string) => {
  return {
    value: value,
    label: label,
    color: color,
  }
}

const layoutClasses = [
  'flex',
  'inline-flex',
  'flex-row',
  'flex-col',
  'flex-wrap',
  'flex-no-wrap',
  'items-start',
  'items-end',
  'items-center',
  'items-baseline',
  'items-stretch',
  'justify-start',
  'justify-between',
  'justify-around',
  'justify-evenly',
  'justify-end',
  'flex-1',
  'flex-grow',
  'flex-grow-0',
  'flex-shrink',
  'flex-shrink-0',
]
const shadowClasses = ['shadow', 'shadow-xs', 'shadow-sm', 'shadow-md', 'shadow-lg', 'shadow-xl']

const interactionClasses = [
  'hover:opacity-0',
  'hover:opacity-25',
  'hover:opacity-50',
  'hover:opacity-75',
  'hover:shadow-xs',
  'hover:shadow-sm',
  'hover:shadow-md',
  'hover:shadow-lg',
  'hover:shadow-xl',
  'hover:shadow-2xl',
  'hover:scale-90',
  'hover:scale-95',
  'hover:scale-105',
  'hover:scale-110',
]

const animationClasses = [
  'transition-none',
  'transition-all',
  'transition',
  'transition-colors',
  'transition-opacity',
  'transition-shadow',
  'transition-transform',
  'ease-linear',
  'ease-in',
  'ease-out',
  'ease-in-out',
  'duration-75',
  'duration-150',
  'duration-200',
  'duration-300',
  'duration-500',
  'duration-700',
  'duration-1000',
  'delay-75',
  'delay-100',
  'delay-150',
  'delay-200',
  'delay-300',
  'delay-500',
  'delay-700',
  'delay-1000',
]

const backgroundClasses = [
  'bg-neonblue',
  'bg-neonpink',
  'bg100',
  'bg95',
  'bg90',
  'bg30',
  'bg20',
  'bg10',
]

const typographyClasses = [
  'c0',
  'c10',
  'c30',
  'c50',
  'c70',
  'c100',
  'font-serif',
  'font-medium',
  'font-bold',
]

const typographyOptions = typographyClasses.map((x) => assembleOption(x, x, '#ff00aa'))
const animationOptions = animationClasses.map((x) => assembleOption(x, x, '#00ffaa'))
const backgroundOptions = backgroundClasses.map((x) => assembleOption(x, x, '#aaff00'))
const interactionOptions = interactionClasses.map((x) => assembleOption(x, x, '#aa00ff'))
const shadowOptions = shadowClasses.map((x) => assembleOption(x, x, '#ff00ff'))
const layoutOptions = layoutClasses.map((x) => assembleOption(x, x, '#00aaff'))

export const utopionsStylesOptions = [
  ...layoutOptions,
  ...backgroundOptions,
  ...shadowOptions,
  ...typographyOptions,
  ...interactionOptions,
  ...animationOptions,
]
