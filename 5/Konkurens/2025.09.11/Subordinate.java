public class Subordinate extends Employee {
    public Subordinate(double salary, String name) {
        super(salary, name);
    }

    @Override
    public double getSalary() {
        return salary;
    }
}
