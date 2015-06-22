
How to install
================

Install haskell
-----------------

We use LTS Haskell 2.14 - GHC 7.8.4
Link is here https://www.stackage.org/lts-2.14

### install ghc

Should install ghc version 7.8
Recommend install ghc version 7.8.4

#### install at ubuntu

You can install ghc 7.8.4 using custom PPA.
https://launchpad.net/~hvr/+archive/ubuntu/ghc

```shell
  sudo add-apt-repository ppa:hvr/ghc
  sudo apt-get update
  sudo apt-get install ghc-7.8.4
```

Setup project
---------------------

### setup haskell-lts

Goto project folder and execute below commands.

```shell
  wget https://www.stackage.org/lts-2.14/cabal.config
  cabal update
  cabal install
```

### mysql setup

1. Create mysql user attendance / attendance
2. Give privileges on attendance database.
3. Execute below command.

```shell
  mysql -uattendance -pattendance < sqls/createDatabase.sql
```

How to run
==============

```shell
  cabal run
```
