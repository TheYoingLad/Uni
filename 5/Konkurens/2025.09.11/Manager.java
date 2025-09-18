import java.util.ArrayList;
import java.util.List;

public class Manager extends Employee {
    List<Employee> employees;

    public Manager(double salary, String name) {
        super(salary, name);
        employees = new ArrayList<Employee>();
    }

    public void addEmployee(Employee e) {
        employees.add(e);
    }

    public void removeEmployee(Employee e) {
        employees.remove(e);
    }

    @Override
    public double getSalary() {
        return salary + employees.stream().map(Employee::getSalary).reduce(0.0, Double::sum) * 0.05;
    }
}
