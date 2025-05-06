public class ComplexMain{
    public static void main(String[] args){
        Complex z1 = new Complex();
        Complex z2 = new Complex(1,2);
        Complex z3 = new Complex(3,5);
        Complex z4 = new Complex(1,1);
        z1.add(z2);
        System.out.println(z1);
        z1.sub(z3);
        System.out.println(z1);
        z1.sub(z4);
        System.out.println(z1);
    }
}

class Complex{
    private double r;
    private double i;

    public Complex(double r, double i){
        this.r = r;
        this.i = i;
    }

    public Complex(){
        this(0.0, 0.0);
    }

    public double abs(){
        return Math.sqrt(r*r + i*i);
    }

    public void conj(){
        i *= -1;
    }

    public void rec(){
        r /= r*r + i*i;
        i /= -(r*r + i*i);
    }

    public void add(Complex that){
        r += that.r;
        i += that.i;
    }
    public void sub(Complex that){
        r -= that.r;
        i -= that.i;
    }
    public void mul(Complex that){
        r = (r*that.r + i*that.i) / (that.r*that.r + that.i*that.i);
        i = (that.r*i - r*that.i) / (that.r*that.r + that.i*that.i);
    }

    public void div(Complex that){
        r = r * that.r - i * that.i;
        i = r*that.i + that.r * i;
    }

    public String toString(){
        if(i > 0) return r + "+" + i + "i";
        else return r + "" + i + "i";
    }
}