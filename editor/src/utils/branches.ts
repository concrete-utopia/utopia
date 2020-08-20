export const BranchNameParameterKey = 'branch_name'

export function getEditorBranchNameFromURL(): string | null {
  const urlParams = new URLSearchParams(window.location.search)
  let possibleBranchName: string | null = null
  if (urlParams.has(BranchNameParameterKey)) {
    possibleBranchName = urlParams.get(BranchNameParameterKey)
  }
  return possibleBranchName
}

export function setBranchNameFromURL(searchParams: URLSearchParams): void {
  const possibleBranchName = getEditorBranchNameFromURL()
  if (possibleBranchName != null) {
    searchParams.set(BranchNameParameterKey, possibleBranchName)
  }
}
