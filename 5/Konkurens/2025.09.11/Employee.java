public abstract class Employee implements SalariedEntity {
    double salary;
    String name;

    public Employee(double salary, String name) {
        this.salary = salary;
        this.name = name;
    }

    public abstract double getSalary();

    public String getName() {
        return name;
    }

    public void modifySalary(double multiplier) {
        salary *= multiplier;
    }
}
