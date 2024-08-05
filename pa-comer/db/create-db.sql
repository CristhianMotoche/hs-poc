CREATE TABLE IF NOT EXISTS meals
   ( name VARCHAR NOT NULL
   , description VARCHAR NOT NULL
   , type VARCHAR NOT NULL
   , PRIMARY KEY( name )
   );

CREATE TABLE IF NOT EXISTS menus
   ( id INT NOT NULL
   , time DATETIME NOT NULL
   , PRIMARY KEY( id )
   );
