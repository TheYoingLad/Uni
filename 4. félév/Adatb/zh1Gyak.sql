//1
//a
SELECT haz_nev FROM got_karakterek NATURAL JOIN got_hazak WHERE vagyon < sereg*15 GROUP BY haz_nev HAVING count(*) >= 2;

//b
SELECT haz_nev FROM got_karakterek NATURAL JOIN got_hazak WHERE vagyon >= sereg*15 GROUP BY haz_nev HAVING count(*) >= 2;


//2
//a
SELECT haz_nev, SUM(NVL(sereg, 1000)) teljes_sereg FROM got_karakterek NATURAL JOIN got_hazak GROUP BY haz_nev HAVING SUM(NVL(vagyon, 0)) > 20000;

//b
SELECT haz_nev, SUM(NVL(sereg, 2000)) teljes_sereg FROM got_karakterek NATURAL JOIN got_hazak GROUP BY haz_nev HAVING SUM(NVL(vagyon, 0)) < 10000;


//3
//a
SELECT DISTINCT haz_nev FROM
(
    SELECT * FROM (SELECT haz_nev FROM got_hazak), (SELECT DISTINCT csata_nev FROM got_csatak)
        MINUS
    SELECT haz_nev, csata_nev FROM got_hazak NATURAL JOIN got_csatak
);

//b
SELECT haz_nev FROM got_hazak
MINUS
SELECT DISTINCT haz_nev FROM
(
    SELECT * FROM (SELECT haz_nev FROM got_hazak), (SELECT DISTINCT csata_nev FROM got_csatak)
        MINUS
    SELECT haz_nev, csata_nev FROM got_hazak NATURAL JOIN got_csatak
);


//4
//a
SELECT nagy.haz_nev, sereg, nev FROM
(SELECT haz_nev, MAX(NVL(sereg, 0)) tobb FROM got_karakterek NATURAL JOIN got_hazak GROUP BY haz_nev) nagy
JOIN
(SELECT * FROM got_karakterek NATURAL JOIN got_hazak) osszes
ON tobb = NVL(sereg, 0) AND nagy.haz_nev = osszes.haz_nev;

//b
SELECT szegeny.haz_nev, vagyon, nev FROM
(SELECT haz_nev, MIN(NVL(vagyon, 0)) keves FROM got_karakterek NATURAL JOIN got_hazak GROUP BY haz_nev) szegeny
JOIN
(SELECT * FROM got_karakterek NATURAL JOIN got_hazak) osszes
ON keves = NVL(vagyon, 0) AND szegeny.haz_nev = osszes.haz_nev;


//5
//a
SELECT motto FROM
(SELECT haz_nev FROM
(
    (SELECT * FROM
    (
        (SELECT csata_nev, count(*) db FROM got_csatak GROUP BY csata_nev)
        NATURAL JOIN
        (SELECT MIN(db) db FROM (SELECT csata_nev, COUNT(*) db FROM got_csatak GROUP BY csata_nev))
    ))
    NATURAL JOIN
    (SELECT * FROM got_csatak)
)
WHERE gyozott = 'nem')
NATURAL JOIN
(SELECT * FROM got_hazak);

//b
SELECT DISTINCT motto FROM
(SELECT haz_nev FROM
(
    (SELECT * FROM
    (
        (SELECT csata_nev, count(*) db FROM got_csatak GROUP BY csata_nev)
        NATURAL JOIN
        (SELECT MAX(db) db FROM (SELECT csata_nev, COUNT(*) db FROM got_csatak GROUP BY csata_nev))
    ))
    NATURAL JOIN
    (SELECT * FROM got_csatak)
)
WHERE gyozott = 'nem')
NATURAL JOIN
(SELECT * FROM got_hazak);