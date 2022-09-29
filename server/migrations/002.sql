ALTER TABLE ONLY "public"."github_authentication"
    ALTER COLUMN "refresh_token" DROP NOT NULL;

ALTER TABLE ONLY "public"."github_authentication"
    ALTER COLUMN "expires_at" DROP NOT NULL;
