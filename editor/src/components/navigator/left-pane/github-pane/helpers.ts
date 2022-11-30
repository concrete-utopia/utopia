export function cleanupBranchName(branchName: string): string {
  return branchName
    .split(/[^0-9a-z\/_.]+/i)
    .join('-')
    .replace(/[-.\/]+$/, '') // does not end with a dash, a slash, or a dot
    .replace(/^[-.\/]+/, '') // does not start with a dash, a slash, or a dot
    .replace(/\.lock$/i, '') // does not end with .lock
    .replace(/\.{2,}/, '-') // does not contain two or more consecutive dots
}
