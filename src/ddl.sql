DROP DATABASE echo_bot;
CREATE DATABASE echo_bot;

DROP TABLE usersVK;
DROP TABLE usersTG;

CREATE TABLE usersVK (
  id integer PRIMARY KEY,
  count integer NOT NULL DEFAULT 1
);

CREATE TABLE usersTG (
  id integer PRIMARY KEY,
  count integer NOT NULL DEFAULT 1
);