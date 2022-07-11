export type MouseButtonsPressed = Set<number>

export function addButtonPressed(
  buttonsPressed: MouseButtonsPressed,
  button: number,
): MouseButtonsPressed {
  let result: MouseButtonsPressed = new Set(buttonsPressed)
  result.add(button)
  return result
}

export function removeButtonPressed(
  buttonsPressed: MouseButtonsPressed,
  button: number,
): MouseButtonsPressed {
  let result: MouseButtonsPressed = new Set(buttonsPressed)
  result.delete(button)
  return result
}
