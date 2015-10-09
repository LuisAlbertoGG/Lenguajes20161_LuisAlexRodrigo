public class Tarea2.2 {

     public static  boolean foo(int a){

        return true;
        }
     public static int foo0(int a){
         return a / 0;
     }
     public static int foo1(int a){
         return a*5;
     }
    public static void main(String[] args) {
        int b = 1;
        System.out.println("El resultado de la evalución glotona es: " + foo(foo0(foo1(b))));
    }
    
}
