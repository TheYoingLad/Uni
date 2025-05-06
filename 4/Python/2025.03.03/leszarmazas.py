class A:
    def say_hello(self):
        print("A")

class B(A):
    def say_hello(self):
        print("B")

class C(B,A):
    pass


a = A()
a.say_hello()

b = B()
b.say_hello()
print(B.mro()) #leszármazások listája, method resolution error

c = C()
c.say_hello()
print(C.mro())