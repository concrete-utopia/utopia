export function assertNever(n: never): never {
  throw new Error(`Expected \`never\`, got ${JSON.stringify(n)}`)
}
