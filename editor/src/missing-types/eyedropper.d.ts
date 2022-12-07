interface EyeDropperConstructor {
  new (): EyeDropper
}

interface ColorSelectionOptions {
  signal: AbortSignal
}

interface ColorSelectionResult {
  sRGBHex: string
}

interface EyeDropper extends EyeDropperConstructor {
  open: (options?: ColorSelectionOptions = {}) => Promise<ColorSelectionResult>
}

interface Window {
  EyeDropper: EyeDropper | undefined
}
