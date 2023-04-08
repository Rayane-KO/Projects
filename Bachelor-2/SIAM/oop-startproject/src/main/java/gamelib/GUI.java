package gamelib;

import javax.swing.*;
import java.awt.*;

public class GUI extends JFrame {
    private GridPanel panel;

    public GUI(int width, int height, int rows, int columns, int padding) {
        super();
        this.setSize(new Dimension(width, height+40));
        this.setResizable(false);
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);
        panel = new GridPanel(width, height, rows, columns);
        panel.setPadding(padding);
        this.setTitle("Cell Game");
        this.setContentPane(panel);
        this.setVisible(true);
    }

    public GridPanel getGridPanel() {
        return panel;
    }
}
