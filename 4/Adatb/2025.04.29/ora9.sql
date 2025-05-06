SET SERVEROUTPUT ON;

-- zh: 4-5 feladat, 60 perc

-- ACCEPT
ACCEPT nev CHAR  PROMPT 'Add meg a dolozó nevét!'; --varchar-t nem lehet, char = szöveg

SELECT * FROM dolgozo WHERE dnev = 'KING';
SELECT * FROM dolgozo WHERE dnev = &nev; -- ha promptba KING -> itt is KING => promptba 'KING' vagy '&nev'

SELECT * FROM dolgozo WHERE dnev = '&nev2'; -- ha nincs definiálva akkor mindig bekéri

-- TRIGGEREK
DROP TABLE dolgozo2;
CREATE TABLE dolgozo2 AS SELECT * FROM dolgozo;

CREATE OR REPLACE TRIGGER sorszamoszto BEFORE INSERT ON dolgozo2 FOR EACH ROW -- meg kell mondani hogy mikor fusson le
DECLARE
    akt_max INT;
BEGIN
    SELECT MAX(dkod) INTO akt_max FROM dolgozo2;
    dbms_output.put_line('Új beszúrás efelülbírálva, régi dkod: ' || :new.dkod || ' helyett ' || (akt_max + 1));
    
    -- trigger specifiukus dolgok
    :new.dkod := akt_max; -- új érték
    -- :OLD régi érték; beszúrásnál nincs, hasonlóan működik mint a rekord, viszont ez módosítható
END;
/

INSERT INTO dolgozo2(dkod, dnev, fizetes) VALUES (1, 'Teszt3', 1234);
SELECT * FROM dolgozo2;

CREATE OR REPLACE FUNCTION fiz_emeles(szazalek INT) RETURN INT IS
    -- CURSOR cursor1 IS SELECT * FROM dolgozo2 FOR UPDATE; -- lehessen vele módosítani
    CURSOR cursor1 IS SELECT * FROM osztaly NATURAL JOIN dolgozo2 FOR UPDATE of fizetes; -- kapcsolt táblánál elv meg kell adni melyik oszlopokat fog módosítani (de egy sorral lehet változtatni az egészet)
    osszeg INT := 0;
BEGIN
    FOR rec IN cursor1 LOOP
        -- UPDATE dolgozo SET fizetes = fizetes * (1 + (szazalek / 100.0)) WHERE dkod = rec.dkod; -- csak azt a sort frissítse, de ha nem tudjuk az elsődleges kulcsot akkor ???
        UPDATE dolgozo2 SET fizetes = fizetes * (1 + (szazalek / 100.0)) WHERE CURRENT OF cursor1; -- mindig működik, háttérbeli oracle kulcsot használ
        
        osszeg := osszeg + rec.fizetes * (szazalek / 100.0); -- update nincs hatással a rec-beli adatea
    END LOOP;
    RETURN osszeg;
END;
/

SELECT fiz_emeles(10) FROM dual; -- select utasítás nem módosíthatja az adatbázist => így nem lehet módosító fgv-eket meghívni

-- megoldás: névtelen blokk
DECLARE
BEGIN
    dbms_output.put_line(fiz_emeles(10));
END;
/
SELECT * FROM dolgozo2;


-- FELADATOK
-- 1
DROP TABLE dolgozo2;
CREATE TABLE dolgozo2 AS SELECT 1 sorszam, dolgozo.* FROM dolgozo;

CREATE OR REPLACE PROCEDURE nevsorrend IS
    CURSOR cursor1 IS SELECT * FROM dolgozo2 ORDER BY dnev FOR UPDATE;
    sorsz INT := 1;
BEGIN
    for rec in cursor1 loop
        UPDATE dolgozo2 SET sorszam = sorsz WHERE CURRENT OF cursor1;
        sorsz := sorsz + 1;
    end loop;
END;
/
call nevsorrend();
SELECT * FROM dolgozo2 order by dnev;


-- 2
CREATE OR REPLACE FUNCTION is_prime(n INT) RETURN BOOLEAN IS
BEGIN
    FOR i in 2..sqrt(n) LOOP
        IF mod(n, i) = 0 THEN
            RETURN false;
        END IF;
    END LOOP;
    RETURN true;
END;
/
CREATE OR REPLACE PROCEDURE fiz_emeles_prim IS    
    CURSOR cursor1 IS SELECT * FROM dolgozo2 FOR UPDATE;
BEGIN
    FOR rec IN cursor1 LOOP
        if is_prime(rec.sorszam) then
            UPDATE dolgozo2 SET fizetes = fizetes * (1.5) WHERE CURRENT OF cursor1;
        end if;
    END LOOP;
END;
/
call fiz_emeles_prim();
SELECT * FROM dolgozo2 order by dnev;


-- 3
DROP TABLE dolgozo2;
CREATE TABLE dolgozo2 AS SELECT * FROM dolgozo;

DELETE FROM dolgozo2 WHERE dkod in (SELECT dkod from dolgozo2 JOIN fiz_kategoria ON fizetes >= also and fizetes <= felso WHERE kategoria = 3);
SELECT * from dolgozo2 JOIN fiz_kategoria ON fizetes >= also and fizetes <= felso;


-- 4
DROP TABLE dolgozo2;
CREATE TABLE dolgozo2 AS SELECT * FROM dolgozo;

CREATE OR REPLACE PROCEDURE fiz_emeles_param (kat INT) IS    
    CURSOR cursor1 IS SELECT * FROM dolgozo2 JOIN fiz_kategoria ON fizetes BETWEEN also AND felso WHERE kategoria = kat FOR UPDATE OF fizetes;
    osszeg INT := 0;
    db INT := 0;
BEGIN
    FOR rec IN cursor1 LOOP
        UPDATE dolgozo2 SET fizetes = fizetes + 1 WHERE CURRENT OF cursor1;
        db := db + 1;
        osszeg := osszeg + rec.fizetes + 1;
    END LOOP;
    dbms_output.put_line(round(osszeg/db, 2));
END;
/
CALL fiz_emeles_param(1);
SELECT * FROM dolgozo2 JOIN fiz_kategoria ON fizetes BETWEEN also AND felso;


-- 5
DROP TABLE dolgozo2;
CREATE TABLE dolgozo2 AS SELECT * FROM dolgozo;

CREATE OR REPLACE FUNCTION mgh(szo VARCHAR2) RETURN INT IS
    db INT := 0;
BEGIN
    for i in 1..length(szo) loop
        if lower(substr(szo,i,1)) in ('a','e','i','o','u') then
            db := db + 1;
        end if;
    end loop;
    return db;
END;
/

CREATE OR REPLACE PROCEDURE fiz_modositas_osztaly (oszt INT) IS    
    CURSOR cursor1 IS SELECT * FROM dolgozo2 NATURAL JOIN osztaly WHERE oazon = oszt FOR UPDATE OF fizetes;
BEGIN
    FOR rec IN cursor1 LOOP
        UPDATE dolgozo2 SET fizetes = fizetes + mgh(rec.dnev)*10000 WHERE CURRENT OF cursor1;
        dbms_output.put_line(rec.dnev || ': ' || (rec.fizetes + mgh(rec.dnev)*10000));
    END LOOP;
END;
/
call fiz_modositas_osztaly(10);


-- 6
DROP TABLE coords;
CREATE TABLE coords(
    x INT NOT NULL,
    y INT NOT NULL
);

INSERT INTO coords VALUES (1, 2);
INSERT INTO coords VALUES (-1, 4);
INSERT INTO coords VALUES (0, 3);
INSERT INTO coords VALUES (3, 2);
INSERT INTO coords VALUES (-3, 0);

select * from coords;

create or replace function legmesszebb return real is
    tav real := 0;
    cursor curs1 is SELECT * from coords;
    cursor curs2 is SELECT * from coords;
begin
    for rec1 in curs1 loop
        for rec2 in curs2 loop
            if sqrt(power((rec1.x - rec2.x), 2) + power((rec1.y - rec2.y), 2)) > tav then
                tav := sqrt(power((rec1.x - rec2.x), 2) + power((rec1.y - rec2.y), 2));
            end if;
        end loop;
    end loop;
    return tav;
end;
/

SELECT legmesszebb() from dual;