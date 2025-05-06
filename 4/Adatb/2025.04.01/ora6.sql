DROP TABLE vasarol; --tábla törlés
DROP TABLE felhasznalo;
DROP TABLE termekek;

CREATE TABLE felhasznalo ( --tábla kreáció
	f_nev	VARCHAR2(20)	PRIMARY KEY, --elsődleges kulcs mint megszorítás
	cim 	VARCHAR2(100)
);

CREATE TABLE termekek (
	t_id	INT,
	t_nev	VARCHAR2(20)	UNIQUE, --egyediség mint megszorítás
	ar 		INT				NOT NULL, --nem üresség mint megszorítás
	CONSTRAINT termek_pk PRIMARY KEY (t_id) --elsődleges kulcs csak külön megszorításként
);

CREATE TABLE vasarol (
	t_id 	INT,
	f_nev	VARCHAR2(20),
	db 		INT,
	CONSTRAINT termek_fk FOREIGN KEY (t_id) REFERENCES termekek(t_id), --idegen kulcs megszorítás
	CONSTRAINT felhasznalo_fk FOREIGN KEY (f_nev) REFERENCES felhasznalo(f_nev)
);

DROP TABLE dolgozo2;
CREATE TABLE dolgozo2 AS SELECT * FROM dolgozo; --tábla másolás
SELECT * FROM dolgozo2;

DROP VIEW dolgozo_osztaly;
CREATE VIEW dolgozo_osztaly AS SELECT * FROM dolgozo NATURAL JOIN osztaly; --nézet, autómatikusan frissül

ALTER TABLE dolgozo2 ADD tel_szam INT; --oszlop hozzáadás
ALTER TABLE dolgozo2 MODIFY tel_szam VARCHAR2(10); --oszlop típus változtatás, castable kell
SELECT * FROM dolgozo2;

INSERT INTO felhasznalo VALUES ('Peti', 'asd'); --beszúrás oszlopok sorrendjében
INSERT INTO felhasznalo VALUES ('Jani', 'huha');
SELECT * FROM felhasznalo;

INSERT INTO termekek (t_nev, ar, t_id) VALUES ('Szék', 50, 1); --beszúrás egyedi sorrendben
INSERT INTO termekek (t_nev, ar, t_id) VALUES ('Asztal', 100, 2);
SELECT * FROM termekek;

INSERT INTO vasarol VALUES (1, 'Peti', 4);
SELECT * FROM vasarol;

DELETE FROM dolgozo2 WHERE fizetes < 2000; --adott sorok törlése
SELECT * FROM dolgozo2;

TRUNCATE TABLE dolgozo2; --töröl minden sort

UPDATE dolgozo2 SET fizetes = fizetes * 2 WHERE dnev = 'KING'; --oszlop frissítése
SELECT * FROM dolgozo2;

--CREATE
--1
DROP TABLE sportcsapatok;
CREATE TABLE sportcsapatok(
    cs_id       INT,
    cs_nev      VARCHAR2(20),
    CONSTRAINT csapat_pk PRIMARY KEY (cs_id)
);

--2
DROP TABLE jatekosok;
CREATE TABLE jatekosok(
    j_id        INT,
    j_nev       VARCHAR2(20),
    mez         INT,
    cs_id       INT,
    CONSTRAINT jatekos_pk PRIMARY KEY (j_id),
    CONSTRAINT csapat_fk FOREIGN KEY (cs_id) REFERENCES sportcsapatok(cs_id)
);

--DELETE
--1
DROP TABLE dolg2;
CREATE TABLE dolg2 AS SELECT * FROM dolgozo;
DELETE FROM dolg2 WHERE jutalek IS NULL;
SELECT * FROM dolg2;

--2
DROP TABLE dolg2;
CREATE TABLE dolg2 AS SELECT * FROM dolgozo;
DELETE FROM dolg2 WHERE belepes < TO_DATE('1982-01-01', 'YYYY-MM-DD');
SELECT * FROM dolg2;

--3
DROP TABLE dolg2;
CREATE TABLE dolg2 AS SELECT * FROM dolgozo NATURAL JOIN osztaly;
DELETE FROM dolg2 WHERE telephely = 'DALLAS';
SELECT * FROM dolg2;

--4
DROP TABLE dolg2;
CREATE TABLE dolg2 AS SELECT * FROM dolgozo, (SELECT AVG(fizetes) avg FROM dolgozo);
DELETE FROM dolg2 WHERE fizetes < avg;
SELECT * FROM dolg2;

--5
DROP TABLE dolg2;
CREATE TABLE dolg2 AS SELECT * FROM dolgozo, (SELECT MAX(fizetes) max FROM dolgozo);
DELETE FROM dolg2 WHERE fizetes = max;
SELECT * FROM dolg2;

--6
DROP TABLE oszt2;
CREATE TABLE oszt2 AS SELECT * FROM osztaly;
DELETE FROM oszt2 WHERE oazon in (SELECT oazon FROM osztaly NATURAL JOIN dolgozo JOIN fiz_kategoria on fizetes >= also AND fizetes <= felso WHERE kategoria = 2 GROUP BY oazon, kategoria HAVING count(*) = 2);
SELECT * FROM oszt2;


--INSERT
DROP TABLE dolg2;
CREATE TABLE dolg2 AS SELECT * FROM dolgozo;
INSERT INTO dolg2 (dkod, dnev, oazon, belepes, fizetes) VALUES (1, 'Kovacs', 10, SYSDATE, (SELECT AVG(fizetes) FROM dolgozo NATURAL JOIN osztaly WHERE oazon = 10));
SELECT * FROM dolg2;


--UPDATE
--1
DROP TABLE dolg2;
CREATE TABLE dolg2 AS SELECT * FROM dolgozo;
UPDATE dolg2 SET fizetes = fizetes * 1.2 WHERE oazon = 20;
SELECT * FROM dolg2;

--2
DROP TABLE dolg2;
CREATE TABLE dolg2 AS SELECT * FROM dolgozo;
UPDATE dolg2 SET fizetes = fizetes + 500 WHERE jutalek is null OR fizetes < (SELECT avg(fizetes) FROM dolgozo);
SELECT * FROM dolg2;

--3
DROP TABLE dolg2;
CREATE TABLE dolg2 AS SELECT * FROM dolgozo;
UPDATE dolg2 SET jutalek = NVL(jutalek, 0) + (SELECT max(jutalek) FROM dolgozo);
SELECT * FROM dolg2;

--4
DROP TABLE dolg2;
CREATE TABLE dolg2 AS SELECT * FROM dolgozo;
UPDATE dolg2 SET dnev = 'Loser' WHERE fizetes = (SELECT min(fizetes) FROM dolgozo);
SELECT * FROM dolg2;

--5
DROP TABLE dolg2;
CREATE TABLE dolg2 AS SELECT * FROM dolgozo;
UPDATE dolg2 SET jutalek = NVL(jutalek, 0) + 3000 WHERE dkod in (SELECT d1.dkod FROM dolgozo d1, dolgozo d2 WHERE d2.fonoke = d1.dkod GROUP BY d1.dkod HAVING count(*) >= 2);
SELECT * FROM dolg2;

--6
DROP TABLE dolg2;
CREATE TABLE dolg2 AS SELECT * FROM dolgozo;
UPDATE dolg2 SET fizetes = fizetes + (SELECT min(fizetes) FROM dolgozo) WHERE dkod in (SELECT DISTINCT d1.dkod FROM dolgozo d1, dolgozo d2 WHERE d2.fonoke = d1.dkod);
SELECT * FROM dolg2;