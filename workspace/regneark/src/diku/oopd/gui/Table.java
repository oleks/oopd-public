package diku.oopd.gui;

import java.awt.Component;

import javax.swing.JTable;

import org.eclipse.jdt.annotation.Nullable;

public class Table<T>
{
  private JTable table;
  
  public Table(AbstractTableModel<T> tableModel)
  {
    this.table = new JTable(tableModel.getTableModel());
    this.table.setColumnSelectionAllowed(true);
  }
  
  @Nullable
  public Component getComponent()
  {
    return this.table;
  }
}
