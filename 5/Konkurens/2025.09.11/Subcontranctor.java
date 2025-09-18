public class Subcontranctor implements SalariedEntity {
    double salary;
    long taxNumber;

    public Subcontranctor(double salary, long taxNumber) {
        this.salary = salary;
        this.taxNumber = taxNumber;
    }

    public double getSalary(){
        return salary;
    }

    public long getTaxNumber(){
        return taxNumber;
    }
}