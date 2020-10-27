// This functionality is based on the import helpers created by TS when the `esModuleInterop` flag is set to `true`,
// all of which are created in https://github.com/microsoft/TypeScript/blob/fca9f451d8010a56d7556e8f83f550b28ee50baa/src/compiler/factory/emitHelpers.ts#L803

function isESModule(mod: unknown): boolean {
  return mod != null && typeof mod === 'object' && (mod as any).__esModule
}

export function importStar(mod: unknown): unknown {
  if (isESModule(mod)) {
    return mod
  } else {
    // FIXME This should inline the implementations of __createBinding and __setModuleDefault from https://github.com/microsoft/TypeScript/blob/fca9f451d8010a56d7556e8f83f550b28ee50baa/src/compiler/factory/emitHelpers.ts#L774
    // but doing some trips tests in ui-jsx-canvas.spec for some reason
    let result: any = { default: mod }

    if (typeof mod === 'object' && mod != null) {
      for (var k in mod) {
        if (k !== 'default' && mod.hasOwnProperty(k)) {
          result[k] = (mod as any)[k]
        }
      }
    }

    return result
  }
}

export function importDefault(mod: unknown): unknown {
  return isESModule(mod) ? (mod as any).default : mod
}
