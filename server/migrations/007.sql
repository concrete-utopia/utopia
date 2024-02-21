CREATE TABLE project_collaborators(
    id serial,
    project_id character varying NOT NULL REFERENCES project_i_d(proj_id) ON DELETE CASCADE,
    user_id character varying NOT NULL REFERENCES user_details(user_id) ON DELETE CASCADE,
    created_at timestamp with time zone NOT NULL DEFAULT NOW()
);

CREATE INDEX "idx_project_collaborators_project_id" ON "public"."project_collaborators"(project_id);

ALTER TABLE ONLY "public"."project_collaborators"
    ADD CONSTRAINT "unique_project_collaborator_project_id_user_id" UNIQUE ("project_id", "user_id");

