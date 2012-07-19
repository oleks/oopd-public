package diku.oopd.regneark.view;

import javax.swing.JFrame;
import javax.swing.JTable;

import diku.oopd.gui.Table;
import diku.oopd.gui.TableModel;

public class MainView extends JFrame
{
  private static final long serialVersionUID = -5626410877829943310L;

  private Table table;
  
  public MainView()
  {
    this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    
    TableModel<String> tableModel = new TableModel<>(10,10);
    tableModel.setValueAt("hej", 1, 1);
    /*
    this.table = new JTable(new String[][] { {"a","b","c" }, {"d", "e", "f"} }, new String[] { "lol", "nice", "beh"});
    this.add(this.table);*/
    this.table = new Table(tableModel);
    this.add(table.getComponent());
    this.pack();
  }
}
