import { UtopiaPrismaClient } from "./db.server";

export async function wait(ms: number) {
  return new Promise((res) => setTimeout(res, ms));
}

export async function createTestUser(
  client: UtopiaPrismaClient,
  params: { id: string; name?: string },
) {
  await wait(50);

  await client.userDetails.create({
    data: {
      user_id: params.id,
      email: `${params.id}@example.com`,
      name: params.name ?? params.id,
    },
  });
}

export async function createTestProject(
  client: UtopiaPrismaClient,
  params: {
    id: string;
    ownerId: string;
    content?: string;
    createdAt?: Date;
    modifiedAt?: Date;
  },
) {
  await wait(50);

  const now = new Date();
  await client.projectID.create({
    data: { proj_id: params.id },
  });
  await client.project.create({
    data: {
      proj_id: params.id,
      title: params.id,
      owner_id: params.ownerId,
      created_at: params.createdAt ?? now,
      modified_at: params.modifiedAt ?? now,
      content: Buffer.from(params.content ?? "test"),
    },
  });
}

export async function createTestSession(
  client: UtopiaPrismaClient,
  params: {
    key: string;
    userId: string;
  },
) {
  await wait(50);

  const now = new Date();
  await client.persistentSession.create({
    data: {
      key: params.key,
      session_json: {
        userID: params.userId,
      },
      created_at: now,
      accessed_at: now,
      session: Buffer.from(""),
    },
  });
}

interface DeletableModel {
  deleteMany: ({}) => Promise<any>;
}

export async function truncateTable(models: DeletableModel[]) {
  await wait(50);

  for (const model of models) {
    await model.deleteMany({});
  }
}
