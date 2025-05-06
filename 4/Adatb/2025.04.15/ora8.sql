set serveroutput on;

-- in változó csak jobb oldalon
-- out változó csak bal oldalon
create or replace procedure valami (a in out int) is --minden alapértelmezetten bemeneti változó
begin
    dbms_output.put_line(a);
    a := a*2;
    dbms_output.put_line(a);
end;
/
call valami(1); -- nem jó mert az 1 literál nem írható

--megoldás: névtelen blokk, azonnal lefut
declare
    x int := 3;
begin
    valami(x);
    
end;
/

----------------------------------------------
--tömbök
declare
    type sajat_tomb_tipus is varray(5) of int; --saját tömb típus ami max 5 elemű, int-et tartalmazhat
    tomb sajat_tomb_tipus := sajat_tomb_tipus(4,5,6); --konstruktor, max 5 elem
begin
    tomb.extend(2); --2-vel megnöveli a méretét, max 5
    tomb(5) := 10; --1-től indexel
    for i in 1..tomb.count loop
        dbms_output.put_line(tomb(i));
    end loop;
end;
/

----------------------------------------------
--exception kezelés
create or replace procedure valami2 (a in out int) is
    sajat_hiba EXCEPTION;
begin
    if a < 0 then
        raise sajat_hiba;
    end if;
    dbms_output.put_line(a);
    a := a*2;
    dbms_output.put_line(a);
EXCEPTION
    when sajat_hiba then
        dbms_output.put_line('a paraméter legyen legalább 0: ' || a);
end;
/

declare
    x int := -1;
begin
    valami2(x);
end;
/

----------------------------------------------
--cursor kezelés
declare
    cursor curs1 is select * from dolgozo where fizetes > 600; --tetszőleges lekérdezés
    rec curs1%rowtype; --sor tipusa
begin
    open curs1; --lehet tobbször openelni, akkor visszaugrik az elejére
    loop
        fetch curs1 into rec;
        exit when curs1%notfound; -- == break, csak a ciklusból
        dbms_output.put_line(rec.dnev || ': ' || rec.fizetes);
    end loop;
    
    close curs1;
end;
/

declare
    cursor curs1 is select * from dolgozo natural join osztaly;
begin
    for rec in curs1 loop --kezeli az open, fetch, exit, close parancsokat a háttérben
    end loop;
end;
/

----------------------------------------------
--feladatok
create or replace function hanyszor(szoveg varchar2, keresett varchar2) return int is
    n int := 0;
begin
    for i in 1..length(szoveg) loop
        if substr(szoveg,i,length(keresett)) = keresett then
            n := n + 1;
        end if;        
    end loop;
    return n;
end;
/
SELECT hanyszor ('ab c ab ab de ab fg ab', 'ab') FROM dual;


create or replace function osszeg(szoveg varchar2) return int is
    next_num int := 0;
    n int := 0;
begin
    for i in 1..length(szoveg) loop
        if substr(szoveg,i,1) = '+' then
            n := n + next_num;
            next_num := 0;
        else
            next_num := next_num * 10 + to_number(substr(szoveg, i, 1));
        end if;
    end loop;
    n := n + next_num;
    return n;
end;
/
select osszeg('1+2+3+40+5+6+10') from dual;
select osszeg('10') from dual;