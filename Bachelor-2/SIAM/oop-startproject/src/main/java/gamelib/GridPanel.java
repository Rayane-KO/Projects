package gamelib;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.sql.Array;
import java.util.ArrayList;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * GridPanel is de grafische representatie van een grid,
 * je zal ook nog een logische representatie hiervoor moeten maken.
 *
 * Het voorziet twee methodes die nuttig zijn:
 * - repaint: zorgt ervoor dat het scherm wordt leeggemaakt en dat alles wordt getekend
 * - setCells: geef een lijst van cellen die getekend moeten worden
 * - removeCell: verwijderd een specifieke cell van het tekenopervlak en hertekent het tekenopervlak
 * - clear verwijderd alles cellen
 */
public class GridPanel extends JPanel {
    /**
     * De lijst van cellen (intern)
     */
    private CopyOnWriteArrayList<Cell> cells;

    /**
     * Een referentie naar de renderer waarmee we interessante figuren kunnen tekenen (intern)
     */
    private Graphics2D mGraphics;

    /**
     * De breedte van 1 cell (intern)
     */
    private int cellWidth;

    /**
     * De hoogte van 1 cell (intern)
     */
    private int cellHeight;

    /**
     * De breedte van het venster (intern)
     */
    private int windowWidth;

    /**
     * De hoogte van het venster (intern)
     */
    private int windowHeight;

    /**
     * Het aantal rijen (intern)
    */
    private int rows;

    /**
     * Het aantal kolommen (intern)
     */
    private int columns;

    /**
     * De padding (witruimte langst alle kanten) van het grid (intern)
     */
    private int padding;

    /**
     * Vraag de padding op  
     */
    public int getPadding() {
        return padding;
    }

    private SimpleMouseListener pressListener = new EmptyMouseListener();
    private SimpleMouseListener moveListener = new EmptyMouseListener();
    private SimpleMouseListener releaseListener = new EmptyMouseListener();

    /**
     * Stel een muis klik listener in
     */
    public void setPressListener(SimpleMouseListener clickListener) {
        this.pressListener = clickListener;
    }

    public void setMoveListener(SimpleMouseListener moveListener) {
        this.moveListener = moveListener;
    }

    public void setReleaseListener(SimpleMouseListener releaseListener) {
        this.releaseListener = releaseListener;
    }

    /**
     * Stel de padding in
     */
    public void setPadding(int padding) {
        this.padding = padding;
        cellWidth  = (windowWidth  - padding*2) / columns;
        cellHeight = (windowHeight - padding*2) / rows;
    }

    public GridPanel(int width, int height, int rows, int columns) {
        super();
        cellWidth  = (width  - padding*2) / columns;
        cellHeight = (height - padding*2) / rows;
        windowWidth = width;
        windowHeight = height;
        this.rows = rows;
        this.columns = columns;
        cells = new CopyOnWriteArrayList<>();
        this.addMouseMotionListener(new MouseMotionListener() {
            @Override
            public void mouseDragged(MouseEvent e) {}

            @Override
            public void mouseMoved(MouseEvent e) {
                moveListener.onEvent(e.getX(), e.getY());
            }
        });
        this.addMouseListener(new MouseListener() {
            @Override
            public void mouseClicked(MouseEvent e) {}

            @Override
            public void mousePressed(MouseEvent e) {
                pressListener.onEvent(e.getX(), e.getY());
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                releaseListener.onEvent(e.getX(), e.getY());
            }

            @Override
            public void mouseEntered(MouseEvent e) {

            }

            @Override
            public void mouseExited(MouseEvent e) {

            }
        });
    }

    private void drawGrid() {
        for (int i = 0; i < columns; i++) {
            for (int j = 0; j < rows; j++) {
                Rectangle rect = new Rectangle(padding + i*cellWidth, padding + j*cellHeight, cellWidth, cellHeight);
                mGraphics.draw(rect);
            }
        }
    }

    @Override
    public void paint(Graphics g) {
        super.paintComponent(g);
        mGraphics = (Graphics2D) g.create();
        this.repaintCells();
        mGraphics.dispose();
    }

    /**
     * Hertekend het volledig venster gegeven de cellen
     */
    public void repaintCells() {
        mGraphics.clearRect(0, 0, windowWidth, windowHeight);
        this.drawGrid();
        for (Cell cell : cells) {
            cell.draw(mGraphics);
        }

        this.repaint();
    }

    /**
     * Stel de nieuwe cellen in
     */
    public void addCells(Iterable<Cell> cells) {
        cells.forEach(this.cells::add);
    }

    /** Verwijder een cell */
    public void removeCell(Cell cell) {
        this.cells.remove(cell);
    }

    /** Verwijder alle cellen */
    public void clear() {
        this.cells.clear();
    }

}
