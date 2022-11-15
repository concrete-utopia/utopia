ALTER TABLE ONLY "public"."user_configuration"
    ALTER COLUMN "theme" SET DEFAULT '"light"';

UPDATE "public"."user_configuration" SET "theme" = '"light"' WHERE "theme" IS NULL;

ALTER TABLE ONLY "public"."user_configuration"
    ALTER COLUMN "theme" SET NOT NULL;