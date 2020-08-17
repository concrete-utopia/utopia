const BranchNameParameterKey = 'branch_name'

export function getEditorBranchNameFromURL(): string | null {
  const urlParams = new URLSearchParams(window.location.search)
  let possibleBranchName: string | null = null
  if (urlParams.has(BranchNameParameterKey)) {
    possibleBranchName = urlParams.get(BranchNameParameterKey)
  }
  return possibleBranchName
}
