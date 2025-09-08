package alma.fa.bigyusz.cucc;

public enum Ertek {
    EGY, KETTO, HAROM;

    public int numeric(){
        return switch (this){
            case EGY -> 1;
            case KETTO -> 2;
            case HAROM -> 3;
        };
    }
}
