import { PrismaClient } from 'prisma-client'
import { singleton } from './singleton.server'

/**
 * Throw an error if the object passed as argument contains
 * keys for which their value is `undefined`.
 */
function rejectUndefinedValues(where?: { [key: string]: unknown }) {
  if (where == null) {
    return
  }
  const undefinedValues = Object.keys(where).filter((key) => where[key] === undefined)
  if (undefinedValues.length > 0) {
    throw new Error(`invalid undefined value for keys [${undefinedValues.join(', ')}]`)
  }
}

/**
 * The singleton instance of the Prisma client, extended to suit our needs.
 * Specifically:
 * - apply the rejectUndefinedValues to all selecting queries, so that if by any chance we
 * 	pass a select clause where the value is undefined an error is thrown and the statement/transaction
 *  is aborted.
 */
const prisma = singleton('prisma', () => {
  const client = new PrismaClient().$extends({
    query: {
      $allModels: {
        async findMany({ args, query }) {
          rejectUndefinedValues(args.where)
          return query(args)
        },
        async findFirst({ args, query }) {
          rejectUndefinedValues(args.where)
          return query(args)
        },
        async findFirstOrThrow({ args, query }) {
          rejectUndefinedValues(args.where)
          return query(args)
        },
        async findUnique({ args, query }) {
          rejectUndefinedValues(args.where)
          return query(args)
        },
        async findUniqueOrThrow({ args, query }) {
          rejectUndefinedValues(args.where)
          return query(args)
        },
        async update({ args, query }) {
          rejectUndefinedValues(args.where)
          return query(args)
        },
        async updateMany({ args, query }) {
          rejectUndefinedValues(args.where)
          return query(args)
        },
        async delete({ args, query }) {
          rejectUndefinedValues(args.where)
          return query(args)
        },
        async deleteMany({ args, query }) {
          rejectUndefinedValues(args.where)
          return query(args)
        },
        async upsert({ args, query }) {
          rejectUndefinedValues(args.where)
          return query(args)
        },
      },
    },
  })
  return client
})
prisma.$connect()

export { prisma }

export type UtopiaPrismaClient = typeof prisma
