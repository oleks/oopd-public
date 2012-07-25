package diku.oopd.regneark.view;

import java.awt.BorderLayout;

import javax.swing.JLabel;
import javax.swing.JPanel;

import diku.oopd.gui.Frame;
import diku.oopd.gui.Table;
import diku.oopd.gui.TableModel;

public class MainView extends Frame
{
  private static final long serialVersionUID = -5626410877829943310L;

  private Table<String> table;
  private JPanel panel;
  
  public MainView()
  {
    this.getContentPane().setLayout(new BorderLayout());
    
    TableModel<String> tableModel = new TableModel<>(10,10);
    tableModel.setValueAt("hej", 1, 1);
    this.table = new Table<>(tableModel);
    
    this.panel = new JPanel();
    JLabel label = new JLabel("hej");
    this.panel.add(label);
    
    this.add(this.table.getComponent(), BorderLayout.CENTER);
    this.add(this.panel, BorderLayout.SOUTH);
    this.pack();
  }
}
