CREATE DATABASE attendance;
USE attendance;

CREATE TABLE `user` (
  `email` varchar(90) NOT NULL,
  `password` varchar(90) NOT NULL,
  UNIQUE KEY (email)
) ENGINE=InnoDB;

CREATE TABLE `attendance` (
  `email` varchar(90) NOT NULL,
  `attendanceTime` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  INDEX ix_email (email),
  FOREIGN KEY fx_email (email) REFERENCES `user` (email) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB;
