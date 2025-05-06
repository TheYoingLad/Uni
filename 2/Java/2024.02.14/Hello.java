public class Hello{
    public static void main(String[] args){
        System.out.println("Name: ");
        String name = System.console().readLine();
        System.console().printf("Hello %s!\n", name);
    }
}