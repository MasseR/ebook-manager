CREATE TABLE public.users (
  identifier bigserial NOT NULL,
  email text NOT NULL,
  username text NOT NULL,
  "role" text NOT NULL,
  "password" bytea NOT NULL,
  CONSTRAINT users_email_key UNIQUE (email),
  CONSTRAINT users_pkey PRIMARY KEY (identifier),
  CONSTRAINT users_username_key UNIQUE (username)
)
WITH (
  OIDS=FALSE
) ;
CREATE TABLE public.books (
  identifier bigserial NOT NULL,
  "contentHash" text NULL,
  "contentType" text NOT NULL,
  title text NULL,
  description text NULL,
  CONSTRAINT books_pkey PRIMARY KEY (identifier)
)
WITH (
  OIDS=FALSE
) ;
CREATE TABLE public.channels (
  identifier bigserial NOT NULL,
  channel text NOT NULL,
  "owner" bigserial NOT NULL,
  CONSTRAINT channels_pkey PRIMARY KEY (identifier),
  CONSTRAINT fk0_owner FOREIGN KEY (owner) REFERENCES users(identifier)
)
WITH (
  OIDS=FALSE
) ;
CREATE TABLE public.tags (
  identifier bigserial NOT NULL,
  tag text NOT NULL,
  "owner" bigserial NOT NULL,
  CONSTRAINT tags_pkey PRIMARY KEY (identifier),
  CONSTRAINT fk0_owner FOREIGN KEY (owner) REFERENCES users(identifier)
)
WITH (
  OIDS=FALSE
) ;
CREATE TABLE public.book_channels (
  channel int8 NOT NULL,
  book int8 NOT NULL,
  CONSTRAINT fk0_channel FOREIGN KEY (channel) REFERENCES channels(identifier),
  CONSTRAINT fk1_book FOREIGN KEY (book) REFERENCES books(identifier)
)
WITH (
  OIDS=FALSE
) ;
CREATE TABLE public.book_tags (
  tag int8 NOT NULL,
  book int8 NOT NULL,
  CONSTRAINT fk0_tag FOREIGN KEY (tag) REFERENCES tags(identifier),
  CONSTRAINT fk1_book FOREIGN KEY (book) REFERENCES books(identifier)
)
WITH (
  OIDS=FALSE
) ;
CREATE TABLE public.user_book (
  "user" bigserial NOT NULL,
  book int8 NOT NULL,
  CONSTRAINT fk0_user FOREIGN KEY ("user") REFERENCES users(identifier),
  CONSTRAINT fk1_book FOREIGN KEY (book) REFERENCES books(identifier)
)
WITH (
  OIDS=FALSE
) ;
