package enums;

public enum EnumDemo {
	A(523),       // public static final EnumDemo A = new EnumDemo(523);
	B("a c"),
	C,
	D(),
	E(432, "gd", new double[] { 1.0 }),
	E2(432, "gd", new double[] {}),
	F(432, "gd", new double[654]),
	G(432, "gd", new double[0])
	;

	int x;

	private EnumDemo() {
		this(123);
	}

	private EnumDemo(int x) {
		this.x = x;
	}

	private EnumDemo(int x, String txt, double[] arr) {
		this.x = x;
	}

	EnumDemo(String txt) {
	}

	public static void main(String[] args) {
//		new EnumDemo(523);

		f("gdsfds", true);
		f("gdsfds", true, 1);
		f("gdsfds", true, 1, 6324, 32145, 643, 324);

//		f("gdsfds", true, new int[1] { 0 });

		f("gdsfds", true, new int[0]);
		f("gdsfds", true, new int[] {});
		f("gdsfds", true, new int[1]);
		f("gdsfds", true, new int[] {1});
		f("gdsfds", true, new int[] {1, 6324, 32145, 643, 324});

		EnumDemo elem = EnumDemo.A;

		// Incompatible conditional operand types EnumDemo and String
//		boolean b = elem instanceof String;

//		boolean b = elem instanceof Enum;
		boolean b = elem instanceof java.lang.Enum;

		System.out.println(b);

		if (elem instanceof Enum e) {
			System.out.println(e);
		}

//		boolean b2 = (new B() instanceof A a) ? a.a : false;

		EnumDemo[] allValues = EnumDemo.values();
		EnumDemo myElem = EnumDemo.valueOf("B");
		String txt = EnumDemo.C.toString();

		System.out.println(myElem == EnumDemo.B);
		System.out.println(new A() == new A());

		System.out.println("abc" == "abc");
		System.out.println("abc".equals("abc"));

		Integer val1 = 11;
		Integer val2 = 3+8;

		System.out.println(val1 == val2);
	}

	static void f(String txt, boolean b, int... elems) {

	}
}


class A {
	public int a;
}

class B extends A {
	public int b;
}
