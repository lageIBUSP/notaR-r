# notaR-r
An R package to be used with notaR - https://github.com/lageIBUSP/notaR

# Installation

To use notaR with a database backend, you first need to create the MySQL/MariaDB database, with commands such as:
```sql
CREATE DATABASE notaR;
CREATE USER notaR@localhost IDENTIFIED BY 'notaR';
GRANT ALL ON notaR.* to notaR@localhost;
```
