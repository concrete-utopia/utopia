const methods = ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS'] as const
export type Method = (typeof methods)[number]
