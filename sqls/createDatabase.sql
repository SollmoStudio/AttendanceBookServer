CREATE DATABASE attendance;
USE attendance;

CREATE TABLE `user` (
  `email` varchar(90) NOT NULL,
  `password` varchar(90) NOT NULL,
  UNIQUE KEY (email)
) ENGINE=InnoDB;

