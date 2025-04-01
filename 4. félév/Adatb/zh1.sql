//1
SELECT ugyfel_nev FROM rendelesi_tortenet GROUP BY ugyfel_nev HAVING MAX(darabszam) <= 10;

//2
SELECT ugyfel_nev, sum(darabszam * egysegar) koltes FROM
(
    (SELECT * FROM
    (
        (SELECT ugyfel_nev FROM ugyfel WHERE vip = 1)
        NATURAL JOIN
        rendelesi_tortenet
    ))
    NATURAL JOIN
    (SELECT termek_nev, egysegar FROM termek)
)
GROUP BY ugyfel_nev;


//3
SELECT ugyfel_nev FROM ugyfel
MINUS
SELECT ugyfel_nev FROM ugyfel, korpusz WHERE jelszo LIKE '%'||szo||'%';


//4
SELECT ugyfel_nev FROM
(SELECT ugyfel_nev, termek_csoport, sum(darabszam * egysegar) koltes FROM
termek
NATURAL JOIN
rendelesi_tortenet
GROUP BY ugyfel_nev, termek_csoport)
GROUP BY ugyfel_nev
HAVING MIN(koltes) >= 5000;

//5
SELECT max(db) FROM
(SELECT ugyfel_nev, count(*) db FROM
(SELECT * FROM rendelesi_tortenet)
NATURAL JOIN
(SELECT ugyfel_nev FROM ugyfel WHERE vip = 0)
GROUP BY ugyfel_nev)
;


SELECT * FROM korpusz;
SELECT * FROM ugyfel;
SELECT * FROM rendelesi_tortenet;
SELECT * FROM termek;