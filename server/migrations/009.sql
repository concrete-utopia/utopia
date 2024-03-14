CREATE TABLE project_access_request(
    id serial,
    project_id text NOT NULL REFERENCES project(proj_id) ON DELETE CASCADE,
    user_id character varying NOT NULL REFERENCES user_details(user_id) ON DELETE CASCADE,
    token text NOT NULL,
    status integer NOT NULL DEFAULT 0,
    created_at timestamp with time zone NOT NULL DEFAULT NOW(),
    updated_at timestamp with time zone NOT NULL DEFAULT NOW()
);

CREATE INDEX "idx_project_access_request_project_id" ON "public"."project_access_request"(project_id);

ALTER TABLE ONLY "project_access_request"
    ADD CONSTRAINT "unique_project_access_request_project_id_user_id" UNIQUE ("project_id", "user_id");

ALTER TABLE ONLY "project_access_request"
    ADD CONSTRAINT "unique_project_access_request_project_id_token" UNIQUE ("project_id", "token");

