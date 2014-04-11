package com.igormaznitsa.elife;

import com.igormaznitsa.prol.annotations.*;
import com.igormaznitsa.prol.data.*;
import com.igormaznitsa.prol.exceptions.ProlCriticalError;
import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.libraries.ProlAbstractLibrary;
import com.igormaznitsa.prol.logic.*;
import com.igormaznitsa.prol.parser.ProlConsult;
import com.igormaznitsa.prol.utils.Utils;
import java.util.*;

/**
 * The class describes a world model of the application p.s. Of course I
 * understand that in pure Java it will be working much more quickly but the
 * game was written to check possibilities of intercommunication between the
 * Prol engine an Java
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @version 1.00
 */
@Consult(URL = "this://com/igormaznitsa/elife/elife.prl")
public final class WorldModel extends ProlAbstractLibrary {
  
  public static final class Cell implements Cloneable {

    protected int cellX;
    protected int cellY;
    protected int cellState;
    protected int cellID;

    public Cell(final int x, final int y, final int state, final int id) {
      cellX = x;
      cellY = y;
      cellState = state;
      cellID = id;
    }

    @Override
    public Object clone() {
      return new Cell(cellX, cellY, cellState, cellID);
    }

    @Override
    public boolean equals(Object obj) {
      if (obj == null) {
        return false;
      }
      if (getClass() != obj.getClass()) {
        return false;
      }
      final Cell other = (Cell) obj;
      if (this.cellID != other.cellID) {
        return false;
      }
      return true;
    }

    @Override
    public int hashCode() {
      int hash = 5;
      hash = 37 * hash + this.cellID;
      return hash;
    }
  }
  private final int columns;
  private final int rows;
  private final Map<Integer, Cell> cellTable = new HashMap<Integer, Cell>();
  private final ProlContext prologEngine;
  private final ProlConsult prologConsult;
  public static final int EMPTY_CELL = 0;
  private int insideIdCellCounter;
  private Cell[] prevCellArray;
  private static final Term TERM_EMPTY = new Term("empty");
  private static final Term TERM_CELL = new Term("cell");

  private final PreparedGoal preparedGoal;

  public WorldModel(final int columns, final int rows) throws Exception {
    super("E-Life");

    this.columns = columns;
    this.rows = rows;

    insideIdCellCounter = 0;

    prologEngine = new ProlContext("eLife", DefaultProlStreamManagerImpl.getInstance());

    prologEngine.addLibrary(this);

    prologConsult = new ProlConsult(prologEngine);
    prologConsult.consult();

    preparedGoal = new PreparedGoal("process_life({?},{?}).", prologEngine);
  }

  // +number,+number,?variable
  @Predicate(Signature = "get_cell_state/3")
  @Determined
  public final void get_cell_state(final Goal goal, final TermStruct predicate) {
    final int x = Utils.getNumberFromElement(predicate.getElement(0)).intValue();
    final int y = Utils.getNumberFromElement(predicate.getElement(1)).intValue();
    final Var var = (Var) predicate.getElement(2);

    final Cell cell = getCellAt(x, y);

    if (cell == null) {
      if (!var.Equ(TERM_EMPTY)) {
        throw new ProlCriticalError("Can't set variable");
      }
    }
    else {
      if (!var.Equ(TERM_CELL)) {
        throw new ProlCriticalError("Can't set variable");
      }
    }
  }

  @Predicate(Signature = "get_cell_neighbours/3")
  @Determined
  public final void get_cell_neighbours(final Goal goal, final TermStruct predicate) {
    final int x = Utils.getNumberFromElement(predicate.getElement(0)).intValue();
    final int y = Utils.getNumberFromElement(predicate.getElement(1)).intValue();
    final Var var = (Var) predicate.getElement(2);

    final Cell[] p_cell = getAllNeighboursForCell(x, y, prevCellArray);
    if (!var.Equ(new TermInteger(p_cell.length))) {
      throw new ProlCriticalError("Can't set variable");
    }
  }

  @Predicate(Signature = "delete_cell/2")
  @Determined
  public final void delete_cell(final Goal goal, final TermStruct predicate) {
    final int x = Utils.getNumberFromElement(predicate.getElement(0)).intValue();
    final int y = Utils.getNumberFromElement(predicate.getElement(1)).intValue();

    final Cell cell = getCellAt(x, y);
    if (cell != null) {
      removeCell(cell);
    }
  }

  @Predicate(Signature = "create_cell/2")
  @Determined
  public final void create_cell(final Goal goal, final TermStruct predicate) {
    final int x = Utils.getNumberFromElement(predicate.getElement(0)).intValue();
    final int y = Utils.getNumberFromElement(predicate.getElement(1)).intValue();

    createCell(x, y, 0);
  }

  public synchronized final void processIteration() {
    prevCellArray = getCellsArray();

    try {
      final Goal goal = preparedGoal.forIntegerParameters(columns, rows);
      goal.solve();
    }
    catch (Throwable _ww) {
      _ww = Utils.getRootThrowable(_ww);
      _ww.printStackTrace();
      System.exit(1);
    }
  }

  public final int getColumnNumber() {
    return columns;
  }

  public final int getRowNumber() {
    return rows;
  }

  public synchronized final void clearAll() {
    cellTable.clear();
    insideIdCellCounter = 0;
  }

  public synchronized final Cell getCellAt(final int x, final int y) {
    final int xx = Math.abs(x % columns);
    final int yy = Math.abs(y % rows);

    final Iterator<Cell> cellIterator = cellTable.values().iterator();
    while (cellIterator.hasNext()) {
      Cell p_cl = cellIterator.next();
      if (p_cl.cellX == xx && p_cl.cellY == yy) {
        return p_cl;
      }
    }
    return null;
  }

  public synchronized final void setCellAt(final int x, final int y, final Cell cell) {
    final int xx = Math.abs(x % columns);
    final int yy = Math.abs(y % rows);

    if (getCellAt(xx, yy) != null) {
      throw new Error("You are trying to set a cell to a non empty field");
    }

    removeCell(cell);
    addCell(xx, yy, cell);
  }

  public synchronized final void removeCell(final Cell cell) {
    if (cell == null) {
      return;
    }

    cellTable.remove(cell.hashCode());
  }

  public synchronized final Cell[] getAllNeighboursForCell(final Cell cell, final Cell[] cells) {
    if (cells == null || cell == null) {
      return null;
    }

    return getAllNeighboursForCell(cell.cellX, cell.cellY, cells);

  }

  public synchronized final Cell[] getAllNeighboursForCell(final int x, final int y, final Cell[] cells) {
    if (cells == null) {
      return null;
    }

    if (cells.length == 0) {
      return new Cell[0];
    }

    final int xx = Math.abs(x % columns);
    final int yy = Math.abs(y % rows);

    final int cellArrayLength = cells.length;
    final ArrayList<Cell> cellList = new ArrayList<Cell>(8);
    int curX = xx;
    int curY = yy;

    int curXLeft = curX - 1;
    if (curXLeft < 0) {
      curXLeft = columns - 1;
    }

    int curXRight = curX + 1;
    if (curXRight == columns) {
      curXRight = 0;
    }

    int curYTop = curY - 1;
    if (curYTop < 0) {
      curYTop = rows - 1;
    }

    int curYBottom = curY + 1;
    if (curYBottom == rows) {
      curYBottom = 0;
    }

    for (int li = 0; li < cellArrayLength; li++) {
      final Cell cell = cells[li];

      final int cellX = cell.cellX;
      final int cellY = cell.cellY;

      if (cellX == xx && cellY == yy) {
        continue;
      }

      if ((cellX == curXLeft || cellX == curXRight || cellX == curX) && (cellY == curYTop || cellY == curYBottom || cellY == curY)) {
        cellList.add(cell);
      }

    }

    return cellList.toArray(new Cell[cellList.size()]);
  }

  public synchronized final void addCell(final int x, final int y, final Cell cell) {
    if (cell == null) {
      return;
    }
    
    final int hashCodeForCell = cell.hashCode();
    if (cellTable.containsKey(hashCodeForCell)) {
      cellTable.remove(hashCodeForCell);
    }

    cellTable.put(hashCodeForCell, cell);
  }

  public synchronized final void createCell(final int x, final int y, final int state) {
    final Cell newCell = new Cell(x, y, state, insideIdCellCounter++);
    cellTable.put(newCell.hashCode(), newCell);
  }

  public synchronized Cell[] getCellsArray() {
    final int cellTableSize = cellTable.values().size();
    final Cell[] cells = new Cell[cellTableSize];
    int index = 0;
    
    final Iterator<Cell> cellIterator = cellTable.values().iterator();

    while (cellIterator.hasNext()) {
      Cell p_cell = (Cell) cellIterator.next().clone();
      cells[index] = p_cell;
      index++;
    }
    return cells;
  }
}
