import java.util.ArrayList;
import java.util.List;

public class Company {
    List<SalariedEntity> salariedEntities;

    public Company() {
        salariedEntities = new ArrayList<>();
    }

    public void addSalariedEntity(SalariedEntity e) {
        salariedEntities.add(e);
    }

    public void removealariedEntity(SalariedEntity e) {
        salariedEntities.remove(e);
    }

    public void raiseEmployeeSalary(double multiplier) {
        salariedEntities.
                stream().
                filter(e -> e instanceof Employee).
                map(e -> (Employee) e).
                forEach((e) -> e.modifySalary(multiplier));
    }
}
