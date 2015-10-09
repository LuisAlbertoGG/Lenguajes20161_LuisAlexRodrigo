public class Ejerrccio4 {

    public static void main(String[] args) {
        int i = 5;
        int numero = cuadrado(fac(i)-1);
        
        System.out.println("numero: " + numero);

    }

    public static int cuadrado(int n) {
        int cuadrado = n * n;
        return cuadrado;
    }

    public static int fac(int n) {
        if (n <= 1) {
            return 1;
        } else {
            return n * fac(n - 1);
        }
    }

}
