DROP DATABASE test_echo_bot;
CREATE DATABASE test_echo_bot;

DROP TABLE usersTestVK;
DROP TABLE usersTestTG;

CREATE TABLE usersTestVK (
  id integer PRIMARY KEY,
  count integer NOT NULL DEFAULT 1
);

CREATE TABLE usersTestTG (
  id integer PRIMARY KEY,
  count integer NOT NULL DEFAULT 1
);