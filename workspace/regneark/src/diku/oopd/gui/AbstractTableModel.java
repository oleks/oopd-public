package diku.oopd.gui;

import javax.swing.event.TableModelListener;

import org.eclipse.jdt.annotation.Nullable;

public abstract class AbstractTableModel<T>
{
  
  class TableModelImpl implements javax.swing.table.TableModel
  {
    
    @Override
    public void addTableModelListener(@Nullable TableModelListener listener)
    {
      AbstractTableModel.this.addTableModelListener(listener);
    }
    
    @Override
    public void removeTableModelListener(@Nullable TableModelListener listener)
    {
      AbstractTableModel.this.removeTableModelListener(listener);
    }

    @Override
    public int getColumnCount()
    {
      return AbstractTableModel.this.getColumnCount();
    }
    
    @Override
    public int getRowCount()
    {
      return AbstractTableModel.this.getRowCount();
    }

    @Override
    @Nullable
    public Class<?> getColumnClass(int columnIndex)
    {
      return String.class;
    }
    
    @Override
    public String getColumnName(int columnIndex)
    {
      return AbstractTableModel.this.getColumnName(columnIndex);
    }

    @Override
    public @Nullable Object getValueAt(int rowIndex, int columnIndex)
    {
      return AbstractTableModel.this.getValueAt(rowIndex, columnIndex);
    }
    
    @Override
    public void setValueAt(@Nullable Object aValue, int rowIndex, int columnIndex)
    {
      return;
    }

    @Override
    public boolean isCellEditable(int rowIndex, int columnIndex)
    {
      return AbstractTableModel.this.isCellEditable(rowIndex, columnIndex);
    }
  }
  
  private TableModelImpl tableModel;
  
  public AbstractTableModel()
  {
    this.tableModel = new TableModelImpl();
  }
  
  @Nullable
  TableModelImpl getTableModel()
  {
    return this.tableModel;
  }
  
  public abstract void removeTableModelListener(@Nullable TableModelListener listener);
  public abstract void addTableModelListener(@Nullable TableModelListener listener);
  public abstract boolean isCellEditable(int rowIndex, int columnIndex);
  public abstract void setValueAt(T value, int rowIndex, int columnIndex);
  public abstract @Nullable T getValueAt(int rowIndex, int columnIndex);
  public abstract String getColumnName(int columnIndex);
  public abstract int getRowCount();
  public abstract int getColumnCount();
}
