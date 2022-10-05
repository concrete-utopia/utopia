CREATE TABLE "public"."github_authentication" (
    "user_id" character varying NOT NULL,
    "access_token" character varying NOT NULL,
    "refresh_token" character varying NOT NULL,
    "expires_at" timestamp with time zone NOT NULL
);

ALTER TABLE ONLY "public"."github_authentication"
    ADD CONSTRAINT "unique_github_authentication" UNIQUE ("user_id");

