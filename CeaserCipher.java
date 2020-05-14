public class CeaserCipher {
    public static StringBuffer cipher(String original, int shifts) {
        StringBuffer cipheredMessage = new StringBuffer();      
        for (int i = 0; i < original.length(); i++) {
            cipheredMessage.append((char)(((int)original.toUpperCase().charAt(i) + shifts - 65) % 26 + 65));
        }
        return cipheredMessage;
    }
    public static StringBuffer decrypt(String message, int shifts) {
        StringBuffer originalMessage = new StringBuffer();      
        for (int i = 0; i < message.length(); i++) {
            originalMessage.append((char)(((int)message.toUpperCase().charAt(i) - shifts - 65) % 26 + 65));
        }
        return originalMessage;
    }
    public static void main(String[] args) {
        if(args.length > 0){
            if(args[0].equals("encrypt")){
                System.out.println(CeaserCipher.cipher(args[1], Integer.parseInt(args[2])));
            } else if(args[0].equals("decrypt")){
                System.out.println(CeaserCipher.decrypt(args[1], Integer.parseInt(args[2])));
            }
        } else {
            System.out.println("Try again.");
        }
    }    
}
