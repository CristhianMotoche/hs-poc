CREATE TABLE IF NOT EXISTS meals
   ( name VARCHAR NOT NULL
   , description VARCHAR NOT NULL
   , type VARCHAR NOT NULL
   , PRIMARY KEY( name )
   );

CREATE TABLE IF NOT EXISTS menus
   ( id INTEGER PRIMARY KEY AUTOINCREMENT
   , time DATETIME NOT NULL
   );

CREATE TABLE IF NOT EXISTS meal_for_menus
   ( id INTEGER PRIMARY KEY AUTOINCREMENT
   , menu__id INT NOT NULL
   , meal__name VARCHAR NOT NULL
   , FOREIGN KEY(menu__id) REFERENCES menu(id)
   , FOREIGN KEY(meal__name) REFERENCES meals(name)
   );
