package gamelib;

import java.awt.*;
import java.io.IOException;
import javax.imageio.ImageIO;

public class AssetsLoader {
    public static Image loadImage(String name) throws IOException {
        Image img = ImageIO.read(AssetsLoader.class.getResourceAsStream("/"+name));
        return img;
    }
}
