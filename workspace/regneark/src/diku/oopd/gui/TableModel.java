package diku.oopd.gui;

import java.util.ArrayList;

import javax.swing.event.TableModelListener;

import org.eclipse.jdt.annotation.Nullable;

public class TableModel<T> extends AbstractTableModel<T>
{
  private ArrayList<TableModelListener> listeners;
  private Object[][] values;
  private int width, height;
  
  public TableModel(int width, int height)
  {
    this.values = new Object[height][width];
    this.listeners = new ArrayList<>();
    this.width = width;
    this.height = height;
  }
  
  @Override
  public void removeTableModelListener(@Nullable TableModelListener listener)
  {
    this.listeners.remove(listener);
  }

  @Override
  public void addTableModelListener(@Nullable TableModelListener listener)
  {
    this.listeners.add(listener);
  }

  @Override
  public boolean isCellEditable(int rowIndex, int columnIndex)
  {
    return false;
  }

  @Override
  public void setValueAt(T value, int rowIndex, int columnIndex)
  {
    System.out.println("nice: " + rowIndex + ", " + columnIndex);
    this.values[rowIndex][columnIndex] = value;
  }

  @SuppressWarnings("unchecked")
  @Override
  public @Nullable T getValueAt(int rowIndex, int columnIndex)
  {
    return (T)this.values[rowIndex][columnIndex];
  }

  @Override
  public int getRowCount()
  {
    return this.height;
  }

  @Override
  public int getColumnCount()
  {
    return this.width;
  }

  @Override
  public String getColumnName(int columnIndex)
  {
    return "nice";
  }

}
