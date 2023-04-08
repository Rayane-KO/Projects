package gamelib;

import java.awt.*;

public interface Cell {
    /**
     * Deze methode laat de cell zichzelf tekenen
     * @param g the grafische context waarin de cel getekend moet worden
     */
    public void draw(Graphics2D g);
}
