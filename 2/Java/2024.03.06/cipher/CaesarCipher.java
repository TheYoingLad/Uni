package cipher;

public class CaesarCipher{
    private int shift;

    public CaesarCipher(int n){
        shift = n % 26;
    }

    public String decrypt(String s){
        String out;
        for(int i = 0; i < s.length; i++){
            if(s[i] >= 'a' && s[i] <= 'z') out[i] = s[i] + shift;
        }
        return "";
    }

    public String encrypt(String s){
        return "";
    }
}

