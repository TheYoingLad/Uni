set serveroutput on;

-- 1. Írjon PL/SQL eljárást, amely előállítja, és egymást követő sorokba kiírja a felhasználó által megadott két egész szám összes közös osztóját. Például 18 és 24 esetén: 1, 2, 3, 6.
create or replace procedure kozos_osztok(a int, b int) is
begin
    dbms_output.put_line(1);
    for i in 2..greatest(a, b) loop
        if mod(a, i) = 0 and mod(b, i) = 0 then
            dbms_output.put_line(i);
        end if;
    end loop;
end;
/
call kozos_osztok(17, 36);


-- 2. Írjon PL/SQL eljárást, amely megnöveli a felhasználó által megadott százalékértékkel minden, az átlagfizetésnél alacsonyabb fizetéssel rendelkező dolgozó fizetését.
drop table dolgozo2;
create table dolgozo2 as SELECT * from dolgozo;

create or replace procedure atlag_alatt_emeles(szazalek int) is
    cursor curs1 is select * from dolgozo2 for update of fizetes;
    atlag real;
begin
    select avg(fizetes) into atlag from dolgozo2;
    for rec in curs1 loop
        if rec.fizetes < atlag then
            update dolgozo2 set fizetes = fizetes * (1 + (szazalek / 100)) where current of curs1;
        end if;
    end loop;
end;
/
select * from dolgozo2;
call atlag_alatt_emeles(10);
select * from dolgozo2;


-- 3. 
-- Készítsen két táblát az egyikben legyenek sportcsapatok csapat_id, név. 
drop table sportcsapatok;
create table sportcsapatok(
csapat_id int not null,
nev varchar(100),
constraint csapat_pk primary key (csapat_id)
);

-- A másikban a játékosok, id, név, mezszám, csapat_id. Rendeljen a táblákhoz megfelelő elsődleges és idegen kulcs megszorításokat.
drop table jatekosok;
create table jatekosok(
id int not null,
nev varchar(100),
mezszam int,
csapat_id int,
constraint id_pk primary key (id),
constraint csapat_id_fk foreign key (csapat_id) references sportcsapatok(csapat_id)
);

-- A táblákba szúrjon be 1 soprtcsapatot, 2 játékost a sportcsapathoz, valamint egy olyan játékost akinek nincs csapata.
insert into sportcsapatok values(1, 'Csömör');
insert into jatekosok values(1, 'Jani', 10, 1);
insert into jatekosok values(2, 'Peti', 30, 1);
insert into jatekosok (id, nev, mezszam) values(3, 'Tomi', 40);

-- Törölje az összes olyan játékost akinek nincs csapata.
delete from jatekosok where csapat_id is null;


--4. Írjon PL/SQL függvényt, mi megnöveli azoknak a dolgozóknak a fizetését, akiknek az azonosítója páros és fizetésük kisebb, mint e csoport átlagfizetése.
--   A növelés e fizetéskülönbség 20%-a. A függvény térjen vissza a fizetésemelések összértékével.
drop table dolgozo2;
create table dolgozo2 as select * from dolgozo;

create or replace function paritasemeles return int is
    cursor curs1 is select * from dolgozo2 for update of fizetes;
    osszeg int := 0;
    atlag int;
begin
    select avg(fizetes) into atlag from dolgozo2 where mod(dkod, 2) = 0;
    for rec in curs1 loop
        if mod(rec.dkod, 2) = 0 and rec.fizetes < atlag then
            update dolgozo2 set fizetes = fizetes + (atlag - fizetes) * 0.2 where current of curs1;
            osszeg := osszeg + (atlag - rec.fizetes) * 0.2;
        end if;
    end loop;
    return osszeg;
end;
/

begin
    dbms_output.put_line(paritasemeles());
end;