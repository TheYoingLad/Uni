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


UPDATE dolgozo SET fizetes = fizetes * 2 WHERE dnev = 'KING'; --oszlop frissítése
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

--3
DROP TABLE dolg2;
CREATE TABLE dolg2 AS SELECT * FROM dolgozo;
DELETE FROM dolg2 WHERE belepes < TO_DATE('1982-01-01', 'YYYY-MM-DD');
SELECT * FROM dolg2;

--4
DROP TABLE dolg_oszt;
CREATE TABLE dolg_oszt AS SELECT * FROM dolgozo NATURAL JOIN osztaly;
DELETE FROM dolg_oszt WHERE telephely = 'DALLAS';
SELECT * FROM dolg_oszt;

--5
DROP TABLE dolg2;
CREATE TABLE dolg2 AS SELECT * FROM dolgozo;
DELETE FROM dolg2 WHERE fizetes < (SELECT AVG(fizetes) FROM dolgozo);
SELECT * FROM dolg2;

--6


--2












