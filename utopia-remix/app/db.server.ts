import { PrismaClient } from "@utopia/prisma-client";
import { singleton } from "./singleton.server";

function rejectUndefinedValues(where?: { [key: string]: unknown }) {
  if (where == null) {
    return;
  }
  const undefinedValues = Object.keys(where).filter(
    (key) => where[key] === undefined,
  );
  if (undefinedValues.length > 0) {
    throw new Error(
      `invalid undefined value for keys [${undefinedValues.join(", ")}]`,
    );
  }
}

const prisma = singleton("prisma", () => {
  const client = new PrismaClient().$extends({
    query: {
      $allModels: {
        async findMany({ args, query }) {
          rejectUndefinedValues(args.where);
          return query(args);
        },
        async findFirst({ args, query }) {
          rejectUndefinedValues(args.where);
          return query(args);
        },
        async findFirstOrThrow({ args, query }) {
          rejectUndefinedValues(args.where);
          return query(args);
        },
        async findUnique({ args, query }) {
          rejectUndefinedValues(args.where);
          return query(args);
        },
        async findUniqueOrThrow({ args, query }) {
          rejectUndefinedValues(args.where);
          return query(args);
        },
      },
    },
  });
  return client;
});
prisma.$connect();

export { prisma };
