import urlJoin from "url-join";
import { UtopiaPrismaClient } from "./db.server";
import { SESSION_COOKIE_NAME } from "./util/api.server";

export async function wait(ms: number) {
  return new Promise((res) => setTimeout(res, ms));
}

export async function createTestUser(
  client: UtopiaPrismaClient,
  params: { id: string; name?: string },
) {
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

export async function truncateTables(models: DeletableModel[]) {
  for (const model of models) {
    await model.deleteMany({});
  }
}

export function newTestRequest(params?: {
  path?: string;
  headers?: { [key: string]: string };
  authCookie?: string;
}): Request {
  const path = (params?.path ?? "").replace(/^\/+/, "");
  const req = new Request(`http://localhost:8002/` + path);

  if (params?.headers != null) {
    for (const key of Object.keys(params.headers)) {
      req.headers.set(key, params.headers[key]);
    }
  }

  if (params?.authCookie != null) {
    req.headers.set("cookie", `${SESSION_COOKIE_NAME}=${params.authCookie}`);
  }

  return req;
}
