const methods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"] as const;
export type Method = (typeof methods)[number];

export function isMethod(s: string): s is Method {
  return methods.some((m) => m === s);
}
