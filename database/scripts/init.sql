-- Reset Database
DROP SCHEMA public CASCADE;
CREATE SCHEMA public;

GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO public;
-- Extensions
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Scripts
\i tables.sql
\i inserts.sql
\i functions.sql

